open Pxp_reader;;
open Pxp_types;;
open Minilex;;  (* defines [nextchar] *)

let lex_next_iso88591 lsrc =
  (* Returns code point of next char, assumes rep_encoding = ISO-8859-1 *)
  let lexbuf =  Lazy.force lsrc.lsrc_lexbuf in
  match nextchar lexbuf with
      None -> None
    | Some x -> Some(Char.code x)
;;

let lex_next_s lsrc =
  (* Returns next char or next partial char as string, encoded in
   * rep encoding
   *)
  let lexbuf =  Lazy.force lsrc.lsrc_lexbuf in
  match nextchar lexbuf with
      None -> ""
    | Some x -> String.make 1 x
;;

let ulex_next lsrc =
  (* Returns code point of next char *)
  let ulb = Lazy.force lsrc.lsrc_unicode_lexbuf in
  try
    if ulb.ULB.ulb_chars_len = 0 then
      ULB.refill ulb;  (* may raise End_of_file *)
    assert(ulb.ULB.ulb_chars_len > 0);
    let code = ulb.ULB.ulb_chars.(0) in
    (* print_endline (" CODE: " ^ string_of_int code); *)
    ULB.delete 1 ulb;
    Some code
  with
      End_of_file -> (* print_endline " EOF"; *) None
;;

let ulex_next_s lsrc =
  (* Returns next char as UTF-8 string *)
  let ulb = Lazy.force lsrc.lsrc_unicode_lexbuf in
  try
    if ulb.ULB.ulb_chars_len = 0 then
      ULB.refill ulb;  (* may raise End_of_file *)
    assert(ulb.ULB.ulb_chars_len > 0);
    let s = ULB.utf8_sub_string 0 1 ulb in
    ULB.delete 1 ulb;
(*
    for i = 0 to String.length s - 1 do
      print_string (string_of_int (Char.code s.[i]) ^ " ");
    done;
    print_endline "";
*)
    s
  with
      End_of_file -> (* print_endline "EOF"; *) ""
;;


let make_channel s =
  (* Returns a channel reading the bytes from the string s *)
  let rd, wr = Unix.pipe() in
  let ch_rd = Unix.in_channel_of_descr rd in
  let ch_wr = Unix.out_channel_of_descr wr in
  ignore
    (Thread.create
       (fun () ->
          output_string ch_wr s;
          close_out ch_wr;
       )
       ()
    );
  ch_rd
;;

(**********************************************************************)

let t001 next () =
  (* Reads from a string (without recoding it), checks the lexbuf size *)
  let s = "0123456789abc" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  let c = next lsrc in
  assert (c = Some(Char.code '0'));
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  let c = next lsrc in
  assert (c = Some(Char.code '9'));
  r # change_encoding "";
  let c = next lsrc in
  assert (c = Some(Char.code 'a'));
  ignore(next lsrc);
  let c = next lsrc in
  assert (c = Some(Char.code 'c'));
  let c = next lsrc in
  assert (c = None);
  r # close_in;
  true
;;


let t002 next () =
  (* Like t001, but reads from a channel *)
  let ch = make_channel "0123456789abc" in
  let r = new resolve_read_this_channel ch in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  let c = next lsrc in
  assert (c = Some(Char.code '0'));
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  ignore(next lsrc);
  let c = next lsrc in
  assert (c = Some(Char.code '9'));
  r # change_encoding "";
  let c = next lsrc in
  assert (c = Some(Char.code 'a'));
  ignore(next lsrc);
  let c = next lsrc in
  assert (c = Some(Char.code 'c'));
  let c = next lsrc in
  assert (c = None);
  r # close_in;
  true
;;


let t003 next_s () =
  (* Tests non-automatic encoding conversion from ISO-8859-1 to UTF-8 *)
  let s = "0«»°áàâãäÁÀÂÃÄéèêëíìîïÍÌÎÏóòôõøöÓÒÔÕØÖúùûüýÿÝßç¡¿ñÑ" in
  let r = new resolve_read_this_string ~fixenc:`Enc_iso88591 s in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  let c = ref (next_s lsrc) in
  assert (!c = "0");
  let u = ref "" in
  while !c <> "" do
    u := !u ^ !c;
    c := next_s lsrc
  done;
  r # close_in;
  !u = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145"
;;


let t004 next () =
  (* Tests non-automatic encoding conversion from UTF-8 to ISO-8859-1 *)
  let s = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145" in
  let r = new resolve_read_this_string ~fixenc:`Enc_utf8 s in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  let c = ref (next lsrc) in
  assert (!c = Some(Char.code '0'));
  let u = ref "" in
  while !c <> None do
    ( match !c with
	  Some x -> u := !u ^ String.make 1 (Char.chr x)
	| None   -> assert false
    );
    c := next lsrc
  done;
  r # close_in;
  !u = "0«»°áàâãäÁÀÂÃÄéèêëíìîïÍÌÎÏóòôõøöÓÒÔÕØÖúùûüýÿÝßç¡¿ñÑ"
;;


let t005 next () =
  (* Tests automatic encoding conversion from UTF-8 to ISO-8859-1 *)
  let s = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  let c = ref (next lsrc) in
  assert (!c = Some(Char.code '0'));
  let u = ref "" in
  while !c <> None do
    ( match !c with
	  Some x -> assert(x <= 255); u := !u ^ String.make 1 (Char.chr x)
	| None -> ()
    );
    c := next lsrc
  done;
  r # close_in;
  !u = "0«»°áàâãäÁÀÂÃÄéèêëíìîïÍÌÎÏóòôõøöÓÒÔÕØÖúùûüýÿÝßç¡¿ñÑ"
;;


let t006 next_s () =
  (* Tests automatic encoding conversion from UTF-16-BE to UTF-8 
   * This variant invokes change_encoding early.
   *)
  let s = "\254\255\0000\000«\000»\000°\000á\000à\000â\000ã\000ä\000Á\000À\000Â\000Ã\000Ä\000é\000è\000ê\000ë\000í\000ì\000î\000ï\000Í\000Ì\000Î\000Ï\000ó\000ò\000ô\000õ\000ø\000ö\000Ó\000Ò\000Ô\000Õ\000Ø\000Ö\000ú\000ù\000û\000ü\000ý\000ÿ\000Ý\000ß\000ç\000¡\000¿\000ñ\000Ñ" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  let c = ref (next_s lsrc) in
  assert (!c = "0");
  r # change_encoding "";
  let u = ref "" in
  while !c <> "" do
    u := !u ^ !c;
    c := next_s lsrc
  done;
  r # close_in;
  !u = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145"
;;


let t007 next_s () =
  (* Tests automatic encoding conversion from UTF-16-BE to UTF-8 
   * This variant does not invoke change_encoding
   *)
  let s = "\254\255\0000\000«\000»\000°\000á\000à\000â\000ã\000ä\000Á\000À\000Â\000Ã\000Ä\000é\000è\000ê\000ë\000í\000ì\000î\000ï\000Í\000Ì\000Î\000Ï\000ó\000ò\000ô\000õ\000ø\000ö\000Ó\000Ò\000Ô\000Õ\000Ø\000Ö\000ú\000ù\000û\000ü\000ý\000ÿ\000Ý\000ß\000ç\000¡\000¿\000ñ\000Ñ" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  let c = ref (next_s lsrc) in
  assert (!c = "0");
  let u = ref "" in
  while !c <> "" do
    u := !u ^ !c;
    c := next_s lsrc
  done;
  r # close_in;
  !u = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145"
;;

let t008 next_s () =
  (* Invokes change_encoding in the middle *)
  let s = "0\194\171«" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in Anonymous in
  assert(next_s lsrc = "0");
  let u = ref "" in
  while String.length !u < 2 do
    let s = next_s lsrc in
    assert(s <> "");
    u := !u ^ s
  done;
  assert(!u = "\194\171");  (* Must be UTF-8 *)
  r # change_encoding "ISO-8859-1";
  u := "";
  while String.length !u < 2 do
    let s = next_s lsrc in
    assert(s <> "");
    u := !u ^ s
  done;
  assert(!u = "\194\171");  (* Must be UTF-8 *)
  true
;;


(**********************************************************************)

let t100 next_s () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let cwd = Sys.getcwd() in
  let lsrc = r # open_in (System ("file://localhost" ^ cwd ^ "/t100.dat")) in
  let c = next_s lsrc in
  assert (c = "0");
  for i = 1 to 8 do
    ignore(next_s lsrc);
  done;
  let c = next_s lsrc in
  assert (c = "9");
  r # close_in;
  true
;;

let t101 next_s () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let cwd = Sys.getcwd() in
  let lsrc = r # open_in (System ("//localhost" ^ cwd ^ "/t100.dat")) in
  let c = next_s lsrc in
  assert (c = "0");
  for i = 1 to 8 do
    ignore(next_s lsrc);
  done;
  let c = next_s lsrc in
  assert (c = "9");
  r # close_in;
  true
;;

let t102 next_s () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let cwd = Sys.getcwd() in
  let lsrc = r # open_in (System (cwd ^ "/t100.dat")) in
  let c = next_s lsrc in
  assert (c = "0");
  for i = 1 to 8 do
    ignore(next_s lsrc);
  done;
  let c = next_s lsrc in
  assert (c = "9");
  r # close_in;
  true
;;

let t103 next_s () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in (System "t100.dat") in
  let c = next_s lsrc in
  assert (c = "0");
  for i = 1 to 8 do
    ignore(next_s lsrc);
  done;
  let c = next_s lsrc in
  assert (c = "9");
  r # close_in;
  true
;;

(**********************************************************************)

let t110 () =
  (* Checks whether relative URLs are properly handled *)
  (* Note: Not tested for unicode_lexbuf, no need for that. *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner None (new drop_warnings);
  let lsrc = r # open_in (System "t100.dat") in
  let lb = Lazy.force lsrc.lsrc_lexbuf in
  let c = nextchar lb in
  assert (c = Some '0');
  for i = 1 to 8 do
    ignore(nextchar lb);
  done;
  let r' = r # clone in
  let lsrc' = r' # open_in (System "t100.dat") in
  let lb' = Lazy.force lsrc'.lsrc_lexbuf in
  let c = nextchar lb' in
  assert (c = Some '0');
  for i = 1 to 8 do
    ignore(nextchar lb');
  done;
  let c = nextchar lb' in
  assert (c = Some '9');
  r' # close_in;
  let c = nextchar lb in
  assert (c = Some '9');
  r # close_in;
  true
;;

(**********************************************************************)
(* Tests whether the encoding handling of System IDs is okay *)

let t200 () =
  (* Check the technique for the following tests:
   * [Checks also 'combine' to some extent.)
   *)
  let r1 = new resolve_read_this_string
	     ~id:(System "b.xml")
	     ~fixenc:`Enc_iso88591
	     "ae" in
  let r2 = new resolve_read_this_string
	     ~id:(System "a.xml")
	     ~fixenc:`Enc_iso88591
	     "<!DOCTYPE a [ <!ELEMENT a ANY> <!ENTITY ae SYSTEM 'b.xml'> ]> <a>&ae;</a>" in
  let r = new combine [ r1; r2 ] in
  (* It should now be possible to resolve &ae; *)
  let _ =
    Pxp_yacc.parse_document_entity 
      { Pxp_yacc.default_config with Pxp_yacc.encoding = `Enc_iso88591 }
      (Pxp_yacc.ExtID(System "a.xml", r))
      Pxp_yacc.default_spec
  in
  true
;;


let t201 () =
  (* Check that System IDs are converted to UTF-8. rep_encoding = ISO-8859-1 *)
  let r1 = new resolve_read_this_string
	     ~id:(System "\195\164.xml")      (* This is an UTF-8 "ä"! *)
	     ~fixenc:`Enc_iso88591
	     "ae" in
  let r2 = new resolve_read_this_string
	     ~id:(System "a.xml")
	     ~fixenc:`Enc_iso88591
	     "<!DOCTYPE a [ <!ELEMENT a ANY> <!ENTITY ae SYSTEM 'ä.xml'> ]> <a>&ae;</a>" in
  let r = new combine [ r1; r2 ] in
  (* It should now be possible to resolve &ae; *)
  let _ =
    Pxp_yacc.parse_document_entity 
      { Pxp_yacc.default_config with Pxp_yacc.encoding = `Enc_iso88591 }
      (Pxp_yacc.ExtID(System "a.xml", r))
      Pxp_yacc.default_spec
  in
  true
;;


let t202 () =
  (* Check that System IDs are converted to UTF-8. rep_encoding = UTF-8 *)
  let r1 = new resolve_read_this_string
	     ~id:(System "\195\164.xml")
	     ~fixenc:`Enc_iso88591
	     "ae" in
  let r2 = new resolve_read_this_string
	     ~id:(System "a.xml")
	     ~fixenc:`Enc_iso88591
	     "<!DOCTYPE a [ <!ELEMENT a ANY> <!ENTITY ae SYSTEM 'ä.xml'> ]> <a>&ae;</a>" in
  let r = new combine [ r1; r2 ] in
  (* It should now be possible to resolve &ae; *)
  let _ =
    Pxp_yacc.parse_document_entity 
      { Pxp_yacc.default_config with Pxp_yacc.encoding = `Enc_utf8 }
      (Pxp_yacc.ExtID(System "a.xml", r))
      Pxp_yacc.default_spec
  in
  true
;;

(**********************************************************************)

let test f n =
  try
    print_string ("Reader test " ^ n);
    flush stdout;
    if f() then
      print_endline " ok"
    else
      print_endline " FAILED!!!!";
  with
      error ->
	print_endline (" FAILED: " ^ string_of_exn error)
;;

test (t001 lex_next_iso88591) "001 lex";;
test (t001 ulex_next)         "001 ulex";;
test (t002 lex_next_iso88591) "002 lex";;
test (t002 ulex_next)         "002 ulex";;
test (t003 lex_next_s)        "003 lex";;
test (t003 ulex_next_s)       "003 ulex";;
test (t004 lex_next_iso88591) "004 lex";;
test (t004 ulex_next)         "004 ulex";;
test (t005 lex_next_iso88591) "005 lex";;
test (t005 ulex_next)         "005 ulex";;
test (t006 lex_next_s)        "006 lex";;
test (t006 ulex_next_s)       "006 ulex";;
test (t007 lex_next_s)        "007 lex";;
test (t007 ulex_next_s)       "007 ulex";;
test (t008 lex_next_s)        "008 lex";;
test (t008 ulex_next_s)       "008 ulex";;

test (t100 lex_next_s)        "100 lex";;
test (t100 ulex_next_s)       "100 ulex";;
test (t101 lex_next_s)        "101 lex";;
test (t101 ulex_next_s)       "101 ulex";;
test (t102 lex_next_s)        "102 lex";;
test (t102 ulex_next_s)       "102 ulex";;
test (t103 lex_next_s)        "103 lex";;
test (t103 ulex_next_s)       "103 ulex";;

test t110 "110";;

test t200 "200";;
test t201 "201";;
test t202 "202";;
