open Pxp_reader;;
open Pxp_types;;
open Netchannels;;
open Minilex;;

let null_rid = { rid_private = None;
		 rid_system = None;
		 rid_system_base = None;
		 rid_public = None }
;;

(**********************************************************************)
(* t00X: lexical level *)

let t001 () =
  (* Reads from a string (without recoding it), checks active_id *)
  let s = "0123456789abc" in
  let pid = allocate_private_id() in
  let xid = Private pid in
  let channel_of_id rid =
    assert(rid.rid_private = Some pid);
    assert(rid.rid_public = None);
    assert(rid.rid_system = None);
    assert(rid.rid_system_base = None);
    let ch = new input_string s in
    (ch, None, None)
  in
  let r = new resolve_to_any_obj_channel ~channel_of_id () in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in xid in
  let lb = Lazy.force lsrc.lsrc_lexbuf in
  let aid = r # active_id in
  assert(aid.rid_private = Some pid);
  assert(aid.rid_public = None);
  assert(aid.rid_system = None);
  assert(aid.rid_system_base = None);
  let c = nextchar lb in
  assert (c = Some '0');
  ignore(nextchar lb);
  ignore(nextchar lb);
  ignore(nextchar lb);
  ignore(nextchar lb);
  ignore(nextchar lb);
  ignore(nextchar lb);
  ignore(nextchar lb);
  ignore(nextchar lb);
  let c = nextchar lb in
  assert (c = Some '9');
  r # change_encoding "";
  let c = nextchar lb in
  assert (c = Some 'a');
  ignore(nextchar lb);
  let c = nextchar lb in
  assert (c = Some 'c');
  let c = nextchar lb in
  assert (c = None);
  r # close_in;
  true
;;

(**********************************************************************)
(* t01X: acceptance algebra *)

let t010() =
  (* A rid matches itself, and a stronger rid matches a weaker rid *)
  let pid = allocate_private_id() in
  let rid1 =
    { null_rid with rid_private = Some pid } in
  let rid2 = 
    { null_rid with rid_system = Some "file:///this/is/a/file" } in
  let rid3 =
    { null_rid with rid_public = Some "//A//PUBIC//NAME" } in
  let rid12 =
    { null_rid with 
	rid_private = Some pid;
	rid_system = Some "file:///this/is/a/file"; } in
  let rid23 =
    { null_rid with 
	rid_system = Some "file:///this/is/a/file";
	rid_public = Some "//A//PUBIC//NAME" } in
  let rid13 =
    { null_rid with 
	rid_private = Some pid;
	rid_public = Some "//A//PUBIC//NAME"; } in
  let rid123 =
    { null_rid with 
	rid_private = Some pid;
	rid_system = Some "file:///this/is/a/file";
	rid_public = Some "//A//PUBIC//NAME" } in
  
  let matches r r' r'' =
    (* r matches with r', and the active_id is r'' *)
    let ch = new input_string "" in
    let res = new resolve_to_this_obj_channel ~rid:r ch in
    res # init_rep_encoding `Enc_iso88591;
    res # init_warner None (new drop_warnings);
    try
      ignore(res # open_rid r');
      assert(res # active_id = r'');
      true
    with
	Not_competent -> false in
  
  assert (matches rid1 rid1 rid1);
  assert (matches rid2 rid2 rid2);
  assert (matches rid3 rid3 rid3);
  assert (matches rid12 rid12 rid12);
  assert (matches rid23 rid23 rid23);
  assert (matches rid13 rid13 rid13);
  assert (matches rid123 rid123 rid123);

  assert (matches rid1 rid12 rid1);
  assert (matches rid1 rid13 rid1);
  assert (matches rid1 rid123 rid1);
  assert (not(matches rid1 rid2 rid1));
  assert (not(matches rid1 rid3 rid1));
  assert (not(matches rid1 rid23 rid1));

  assert (matches rid2 rid12 rid2);
  assert (matches rid2 rid23 rid2);
  assert (matches rid2 rid123 rid2);
  assert (not(matches rid2 rid1 rid2));
  assert (not(matches rid2 rid3 rid2));
  assert (not(matches rid2 rid13 rid2));

  assert (matches rid3 rid13 rid3);
  assert (matches rid3 rid23 rid3);
  assert (matches rid3 rid123 rid3);
  assert (not(matches rid3 rid1 rid3));
  assert (not(matches rid3 rid2 rid3));
  assert (not(matches rid3 rid12 rid3));

  assert (matches rid12 rid12 rid12);
  assert (matches rid12 rid123 rid12);
  assert (matches rid12 rid1 rid1);
  assert (matches rid12 rid2 rid2);
  assert (matches rid12 rid13 rid1);
  assert (not(matches rid12 rid3 rid1));

  assert (matches rid13 rid12 rid1);
  assert (matches rid13 rid123 rid13);
  assert (matches rid13 rid1 rid1);
  assert (matches rid13 rid3 rid3);
  assert (matches rid13 rid23 rid3);
  assert (not(matches rid13 rid2 rid1));

  assert (matches rid23 rid12 rid2);
  assert (matches rid23 rid123 rid23);
  assert (matches rid23 rid2 rid2);
  assert (matches rid23 rid3 rid3);
  assert (matches rid23 rid13 rid3);
  assert (not(matches rid23 rid1 rid1));

  assert (matches rid123 rid12 rid12);
  assert (matches rid123 rid23 rid23);
  assert (matches rid123 rid2 rid2);
  assert (matches rid123 rid3 rid3);
  assert (matches rid123 rid13 rid13);
  assert (matches rid123 rid1 rid1);

  true
;;

let t011() =
  (* A xid matches corresponding rid itself, and a stronger xid matches a
   * weaker rid *)
  let pid = allocate_private_id() in
  let rid1 =
    { null_rid with rid_private = Some pid } in
  let xid1 = 
    Private pid in
  let rid2 = 
    { null_rid with rid_system = Some "file:///this/is/a/file" } in
  let xid2 =
    System "file:///this/is/a/file" in
  let rid3 =
    { null_rid with rid_public = Some "//A//PUBIC//NAME" } in
  let rid12 =
    { null_rid with 
	rid_private = Some pid;
	rid_system = Some "file:///this/is/a/file"; } in
  let rid23 =
    { null_rid with 
	rid_system = Some "file:///this/is/a/file";
	rid_public = Some "//A//PUBIC//NAME" } in
  let xid23 =
    Public("//A//PUBIC//NAME", "file:///this/is/a/file") in
  let rid13 =
    { null_rid with 
	rid_private = Some pid;
	rid_public = Some "//A//PUBIC//NAME"; } in
  let rid123 =
    { null_rid with 
	rid_private = Some pid;
	rid_system = Some "file:///this/is/a/file";
	rid_public = Some "//A//PUBIC//NAME" } in
  
  let matches r r' r'' =
    (* r matches with r', and the active_id is r'' *)
    let ch = new input_string "" in
    let res = new resolve_to_this_obj_channel ~id:r ch in
    res # init_rep_encoding `Enc_iso88591;
    res # init_warner None (new drop_warnings);
    try
      ignore(res # open_rid r');
      assert(res # active_id = r'');
      true
    with
	Not_competent -> false in

  assert (not(matches Anonymous rid1 rid1));
  assert (not(matches Anonymous rid2 rid2));
  assert (not(matches Anonymous rid3 rid3));
  assert (not(matches Anonymous null_rid null_rid));

  assert (matches xid1 rid1 rid1);
  assert (matches xid1 rid12 rid1);
  assert (matches xid1 rid13 rid1);
  assert (matches xid1 rid123 rid1);
  assert (not(matches xid1 rid2 rid1));
  assert (not(matches xid1 rid3 rid1));
  assert (not(matches xid1 rid23 rid1));

  assert (matches xid2 rid2 rid2);
  assert (matches xid2 rid12 rid2);
  assert (matches xid2 rid23 rid2);
  assert (matches xid2 rid123 rid2);
  assert (not(matches xid2 rid1 rid1));
  assert (not(matches xid2 rid3 rid1));
  assert (not(matches xid2 rid13 rid1));

  assert (matches xid23 rid2 rid2);
  assert (matches xid23 rid3 rid3);
  assert (matches xid23 rid23 rid23);
  assert (matches xid23 rid12 rid2);
  assert (matches xid23 rid13 rid3);
  assert (matches xid23 rid123 rid23);
  assert (not(matches xid23 rid1 rid1));

  true
;;

(**********************************************************************)
(* t02X: URLs and relative resolution *)

let url_syntax =
  { Neturl.null_url_syntax with
      Neturl.url_enable_scheme = Neturl.Url_part_allowed;
      Neturl.url_enable_host   = Neturl.Url_part_allowed;
      Neturl.url_enable_path   = Neturl.Url_part_required;
      Neturl.url_accepts_8bits = true;
  }
;;

let t020 () =
  let u1 = Neturl.url_of_string url_syntax "http://host/x/y" in
  let u2 = Neturl.url_of_string url_syntax "http://host/y/z" in
  let u3 = Neturl.url_of_string url_syntax "/a/b" in
  let u4 = Neturl.url_of_string url_syntax "c" in

  let test_urls base url active_url =
    let ch = new input_string "" in
    let url_of_id _ = url in
    let base_url_of_id _ = base in
    let channel_of_url _ _ = ch, None, None in
    let res = new resolve_to_url_obj_channel
		~url_of_id ~base_url_of_id ~channel_of_url () in
    res # init_rep_encoding `Enc_iso88591;
    res # init_warner None (new drop_warnings);
    try
      ignore(res # open_rid null_rid);
      (res # active_id).rid_system = Some active_url
    with
	Not_competent -> assert false 
  in

  assert(test_urls u1 u2 "http://host/y/z");
  assert(test_urls u1 u3 "http://host/a/b");
  assert(test_urls u1 u4 "http://host/x/c");

  true
;;

let t021 () =
  let u1 = Neturl.url_of_string url_syntax "http://host/x/y" in
  let u2 = Neturl.url_of_string url_syntax "http://host/y/z" in
  let u3 = Neturl.url_of_string url_syntax "/a/b" in
  let u4 = Neturl.url_of_string url_syntax "c" in

  let a = new input_string "a" in
  let b = new input_string "b" in
  let c = new input_string "c" in

  let pid_a = allocate_private_id() in
  let pid_b = allocate_private_id() in
  let pid_c = allocate_private_id() in

  let url_of_id rid = 
    match rid.rid_private with
	Some p when p = pid_a -> u2
      | Some p when p = pid_b -> u3 
      | Some p when p = pid_c -> u4
      | _ -> assert false
  in
	
  let base_url_of_id rid = 
    match rid.rid_system_base with
	Some base -> Neturl.url_of_string url_syntax base 
      | _ -> assert false
  in

  let channel_of_url _ u = 
    match Neturl.string_of_url u with
	"http://host/y/z" -> a, None, None
      | "http://host/a/b" -> b, None, None
      | "http://host/a/c" -> c, None, None
      | _ -> assert false
  in

  let res_a = new resolve_to_url_obj_channel
		~url_of_id ~base_url_of_id ~channel_of_url () in
  res_a # init_rep_encoding `Enc_iso88591;
  res_a # init_warner None (new drop_warnings);

  let lex_a_src = res_a # open_rid { null_rid with
				       rid_private = Some pid_a;
				       rid_system_base = Some "http://host/x/y"
				   } in
  let lex_a = Lazy.force lex_a_src.lsrc_lexbuf in
  assert(nextchar lex_a = Some 'a');

  let res_b = res_a # clone in
  let lex_b_src = res_b # open_rid { null_rid with
				       rid_private = Some pid_b;
				       rid_system_base = 
				       (res_a # active_id).rid_system;
				   } in
  let lex_b = Lazy.force lex_b_src.lsrc_lexbuf in
  assert(nextchar lex_b = Some 'b');
  
  let res_c = res_b # clone in
  let lex_c_src = res_c # open_rid { null_rid with
				       rid_private = Some pid_c;
				       rid_system_base = 
				       (res_b # active_id).rid_system;
				   } in
  let lex_c = Lazy.force lex_c_src.lsrc_lexbuf in
  assert(nextchar lex_c = Some 'c');

  true
;;

let t022 () =
  let res_a = new resolve_as_file ~base_url_defaults_to_cwd:true () in
  res_a # init_rep_encoding `Enc_iso88591;
  res_a # init_warner None (new drop_warnings);

  let lex_a_src = res_a # open_rid { null_rid with
				       rid_system = Some "t_a.dat";
				       rid_system_base = None;
				   } in
  let lex_a = Lazy.force lex_a_src.lsrc_lexbuf in
  assert(nextchar lex_a = Some 'a');

  let res_b = res_a # clone in
  let lex_b_src = res_b # open_rid { null_rid with
				       rid_system = Some "t_b.dat";
				       rid_system_base = 
				       (res_a # active_id).rid_system;
				   } in
  let lex_b = Lazy.force lex_b_src.lsrc_lexbuf in
  assert(nextchar lex_b = Some 'b');

  true
;;

let t023 () =
  let prefix = "file://" ^ Sys.getcwd() in
  let res_a = new norm_system_id
		( new lookup_id_as_file 
		    [ Public("A", prefix ^ "/t_a.dat"), "t_a.dat";
		      System(prefix ^ "/t_b.dat"), "t_b.dat"
		    ]
		) in
  res_a # init_rep_encoding `Enc_iso88591;
  res_a # init_warner None (new drop_warnings);

  let lex_a_src = res_a # open_rid { null_rid with
				       rid_public = Some "A";
				       rid_system_base = None;
				   } in
  let lex_a = Lazy.force lex_a_src.lsrc_lexbuf in
  assert(nextchar lex_a = Some 'a');

  let res_b = res_a # clone in
  let lex_b_src = res_b # open_rid { null_rid with
				       rid_system = Some "./%74_b.dat";
				       rid_system_base = 
				       (res_a # active_id).rid_system;
				   } in
  let lex_b = Lazy.force lex_b_src.lsrc_lexbuf in
  assert(nextchar lex_b = Some 'b');

  true
;;

let t024 () =
  let prefix = "file://" ^ Sys.getcwd() in
  let res_a = new rewrite_system_id
		[ "http://user@foo/x/y/", prefix ^ "/" ]
		( new lookup_id_as_file 
		    [ Public("A", prefix ^ "/t_a.dat"), "t_a.dat";
		      System(prefix ^ "/t_b.dat"), "t_b.dat"
		    ]
		) in
  res_a # init_rep_encoding `Enc_iso88591;
  res_a # init_warner None (new drop_warnings);

  let lex_a_src = res_a # open_rid { null_rid with
				       rid_system = 
				       Some "http://user@foo/x/y/t_a.dat";
				       rid_system_base = None;
				   } in
  let lex_a = Lazy.force lex_a_src.lsrc_lexbuf in
  assert(nextchar lex_a = Some 'a');

  let res_b = res_a # clone in
  let lex_b_src = res_b # open_rid { null_rid with
				       rid_system = Some "./%74_b.dat";
				       rid_system_base = 
				       (res_a # active_id).rid_system;
				   } in
  let lex_b = Lazy.force lex_b_src.lsrc_lexbuf in
  assert(nextchar lex_b = Some 'b');

  true
;;

let t025 () =
  let prefix = "file://" ^ Sys.getcwd() in
  let res_a = new rewrite_system_id
		[ prefix ^ "/accessible/", prefix ^ "/accessible/" ]
		( new resolve_as_file() ) in
  res_a # init_rep_encoding `Enc_iso88591;
  res_a # init_warner None (new drop_warnings);

  (* try to open a file outside $HOME/accessible: *)
  try
    let lex_a = res_a # open_rid { null_rid with
				     rid_system = 
				       Some (prefix ^ "/t_a.dat");
				     rid_system_base = None;
				 } in
    assert false
  with
      Not_competent ->
	true
;;


(**********************************************************************)
(* t03X: Combination *)

let t030() =
  (* Combine a catalog SYSTEM ID with "resolve_as_file" *)
  let file_pwd = "file://" ^ Sys.getcwd() ^ "/" in

  let res_a = 
    new lookup_system_id_as_file
      [ "foo", "t_a.dat" ] in

  let res_b =
    new resolve_as_file
      ~base_url_defaults_to_cwd:false
      () in

  let res_c =
    new combine [ res_a; res_b ] in

  res_c # init_rep_encoding `Enc_iso88591;
  res_c # init_warner None (new drop_warnings);

  let lex_c1_src = res_c # open_rid { null_rid with
					rid_system = Some "foo";
					rid_system_base = Some file_pwd;
				   } in
  let lex_c1 = Lazy.force lex_c1_src.lsrc_lexbuf in
  assert(nextchar lex_c1 = Some 'a');

  (* The following works because catalogs ignore system_base: *)
  let res_c' = res_c # clone in
  let lex_c1'_src = res_c' # open_rid { null_rid with
					  rid_system = Some "foo";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c1' = Lazy.force lex_c1'_src.lsrc_lexbuf in
  assert(nextchar lex_c1' = Some 'a');
  res_c' # close_in;

  (* But this does not work, because system_base is not absolute: *)
  ( try
      let res_c' = res_c # clone in
      let lex_c1'_src = res_c' # open_rid { null_rid with
					      rid_system = Some "t_b.dat";
					      rid_system_base = 
					        res_c#active_id.rid_system;
					  } in
      let lex_c1' = Lazy.force lex_c1'_src.lsrc_lexbuf in
      res_c' # close_in;
      assert false
    with
	Not_resolvable Neturl.Malformed_URL -> ()
  );

  res_c # close_in;

  let lex_c2_src = res_c # open_rid { null_rid with
					rid_system = Some "t_b.dat";
					rid_system_base = Some file_pwd;
				   } in
  let lex_c2 = Lazy.force lex_c2_src.lsrc_lexbuf in
  assert(nextchar lex_c2 = Some 'b');

  (* The following works because catalogs ignore system_base: *)
  let res_c' = res_c # clone in
  let lex_c2'_src = res_c' # open_rid { null_rid with
					  rid_system = Some "foo";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c2' = Lazy.force lex_c2'_src.lsrc_lexbuf in
  assert(nextchar lex_c2' = Some 'a');
  res_c' # close_in;

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c2'_src = res_c' # open_rid { null_rid with
					  rid_system = Some "t_a.dat";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c2' = Lazy.force lex_c2'_src.lsrc_lexbuf in
  assert(nextchar lex_c2' = Some 'a');
  res_c' # close_in;

  res_c # close_in;

  true
;;


let t031() =
  (* Combine a catalog PUBLIC ID with "resolve_as_file" *)
  let file_pwd = "file://" ^ Sys.getcwd() ^ "/" in

  let res_a = 
    new lookup_public_id_as_file
      [ "foo", "t_a.dat" ] in

  let res_b =
    new resolve_as_file
      ~base_url_defaults_to_cwd:false
      () in

  let res_c =
    new combine [ res_a; res_b ] in

  res_c # init_rep_encoding `Enc_iso88591;
  res_c # init_warner None (new drop_warnings);

  let lex_c1_src = res_c # open_rid { null_rid with
					rid_public = Some "foo";
					rid_system_base = Some file_pwd;
				   } in
  let lex_c1 = Lazy.force lex_c1_src.lsrc_lexbuf in
  assert(nextchar lex_c1 = Some 'a');

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c1'_src = res_c' # open_rid { null_rid with
					  rid_public = Some "foo";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c1' = Lazy.force lex_c1'_src.lsrc_lexbuf in
  assert(nextchar lex_c1' = Some 'a');
  res_c' # close_in;

  (* But this does not work, because system_base is None: *)
  ( try
      let res_c' = res_c # clone in
      let lex_c1'_src = res_c' # open_rid { null_rid with
					      rid_system = Some "t_b.dat";
					      rid_system_base = 
					        res_c#active_id.rid_system;
					  } in
      let lex_c1' = Lazy.force lex_c1'_src.lsrc_lexbuf in
      res_c' # close_in;
      assert false
    with
	Not_resolvable Not_found -> ()
  );

  res_c # close_in;

  let lex_c2_src = res_c # open_rid { null_rid with
					rid_system = Some "t_b.dat";
					rid_system_base = Some file_pwd;
				   } in
  let lex_c2 = Lazy.force lex_c2_src.lsrc_lexbuf in
  assert(nextchar lex_c2 = Some 'b');

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c2'_src = res_c' # open_rid { null_rid with
					  rid_public = Some "foo";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c2' = Lazy.force lex_c2'_src.lsrc_lexbuf in
  assert(nextchar lex_c2' = Some 'a');
  res_c' # close_in;

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c2'_src = res_c' # open_rid { null_rid with
					  rid_system = Some "t_a.dat";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c2' = Lazy.force lex_c2'_src.lsrc_lexbuf in
  assert(nextchar lex_c2' = Some 'a');
  res_c' # close_in;

  res_c # close_in;

  true
;;


let t032() =
  (* Combine a mixed PUBLIC/SYSTEM ID catalog with "resolve_as_file" *)
  let file_pwd = "file://" ^ Sys.getcwd() ^ "/" in

  let res_a = 
    new lookup_id_as_file
      [ Public("foo", file_pwd ^ "foo"), "t_a.dat" ] in

  let res_b =
    new resolve_as_file
      ~base_url_defaults_to_cwd:false
      () in

  let res_c =
    new norm_system_id
      (new combine [ res_a; res_b ]) in

  res_c # init_rep_encoding `Enc_iso88591;
  res_c # init_warner None (new drop_warnings);

  let lex_c1_src = res_c # open_rid { null_rid with
					rid_public = Some "foo";
					rid_system_base = Some file_pwd;
				   } in
  let lex_c1 = Lazy.force lex_c1_src.lsrc_lexbuf in
  assert(nextchar lex_c1 = Some 'a');

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c1'_src = res_c' # open_rid { null_rid with
					  rid_public = Some "foo";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c1' = Lazy.force lex_c1'_src.lsrc_lexbuf in
  assert(nextchar lex_c1' = Some 'a');
  res_c' # close_in;

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c1'_src = res_c' # open_rid { null_rid with
					  rid_system = Some "t_b.dat";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c1' = Lazy.force lex_c1'_src.lsrc_lexbuf in
  res_c' # close_in;

  res_c # close_in;

  let lex_c2_src = res_c # open_rid { null_rid with
					rid_system = Some "t_b.dat";
					rid_system_base = Some file_pwd;
				   } in
  let lex_c2 = Lazy.force lex_c2_src.lsrc_lexbuf in
  assert(nextchar lex_c2 = Some 'b');

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c2'_src = res_c' # open_rid { null_rid with
					  rid_public = Some "foo";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c2' = Lazy.force lex_c2'_src.lsrc_lexbuf in
  assert(nextchar lex_c2' = Some 'a');
  res_c' # close_in;

  (* This is expected to work: *)
  let res_c' = res_c # clone in
  let lex_c2'_src = res_c' # open_rid { null_rid with
					  rid_system = Some "foo";
					  rid_system_base = 
					    res_c#active_id.rid_system;
				      } in
  let lex_c2' = Lazy.force lex_c2'_src.lsrc_lexbuf in
  assert(nextchar lex_c2' = Some 'a');
  res_c' # close_in;

  res_c # close_in;

  true
;;

(**********************************************************************)

let test f n =
  try
    print_string ("Rewritten Reader test " ^ n);
    flush stdout;
    if f() then
      print_endline " ok"
    else
      print_endline " FAILED!!!!";
  with
      error ->
        print_endline (" FAILED: " ^ string_of_exn error)
;;

test t001 "001";;
test t010 "010";;
test t011 "011";;
test t020 "020";;
test t021 "021";;
test t022 "022";;
test t023 "023";;
test t024 "024";;
test t025 "025";;
test t030 "030";;
test t031 "031";;
test t032 "032";;

