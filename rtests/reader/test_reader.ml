open Pxp_reader;;
open Pxp_types;;
open Minilex;;

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

let t001 () =
  (* Reads from a string (without recoding it), checks the lexbuf size *)
  let s = "0123456789abc" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner (new drop_warnings);
  let lb = r # open_in Anonymous in
  let c = nextchar lb in
  assert (c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  (* Note: the end of lb.lex_buffer is filled up, so lb.lex_curr_pos must
   * now be at the end of the buffer indicating that the buffer is now
   * empty.
   *)
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
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  r # change_encoding "";
  let c = nextchar lb in
  assert (c = Some 'a');
  assert (lb.Lexing.lex_curr_pos < lb.Lexing.lex_buffer_len);
  ignore(nextchar lb);
  let c = nextchar lb in
  assert (c = Some 'c');
  let c = nextchar lb in
  assert (c = None);
  r # close_in;
  true
;;


let t002 () =
  (* Like t001, but reads from a channel *)
  let ch = make_channel "0123456789abc" in
  let r = new resolve_read_this_channel ch in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner (new drop_warnings);
  let lb = r # open_in Anonymous in
  let c = nextchar lb in
  assert (c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  (* Note: the end of lb.lex_buffer is filled up, so lb.lex_curr_pos must
   * now be at the end of the buffer indicating that the buffer is now
   * empty.
   *)
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
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  r # change_encoding "";
  let c = nextchar lb in
  assert (c = Some 'a');
  assert (lb.Lexing.lex_curr_pos < lb.Lexing.lex_buffer_len);
  ignore(nextchar lb);
  let c = nextchar lb in
  assert (c = Some 'c');
  let c = nextchar lb in
  assert (c = None);
  r # close_in;
  true
;;


let t003 () =
  (* Tests non-automatic encoding conversion from ISO-8859-1 to UTF-8 *)
  let s = "0«»°áàâãäÁÀÂÃÄéèêëíìîïÍÌÎÏóòôõøöÓÒÔÕØÖúùûüýÿÝßç¡¿ñÑ" in
  let r = new resolve_read_this_string ~fixenc:`Enc_iso88591 s in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let lb = r # open_in Anonymous in
  let c = ref (nextchar lb) in
  assert (!c = Some '0');
  assert (lb.Lexing.lex_curr_pos < lb.Lexing.lex_buffer_len);
  (* Note: because we initialize the resolver with ~fixenc, the resolver can
   * fill the buffer with more than one byte from the beginning.
   *)
  let u = ref "" in
  while !c <> None do
    ( match !c with
	  Some x -> u := !u ^ String.make 1 x
	| None -> ()
    );
    c := nextchar lb
  done;
  r # close_in;
  !u = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145"
;;


let t004 () =
  (* Tests non-automatic encoding conversion from UTF-8 to ISO-8859-1 *)
  let s = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145" in
  let r = new resolve_read_this_string ~fixenc:`Enc_utf8 s in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner (new drop_warnings);
  let lb = r # open_in Anonymous in
  let c = ref (nextchar lb) in
  assert (!c = Some '0');
  assert (lb.Lexing.lex_curr_pos < lb.Lexing.lex_buffer_len);
  (* Note: because we initialize the resolver with ~fixenc, the resolver can
   * fill the buffer with more than one byte from the beginning.
   *)
  let u = ref "" in
  while !c <> None do
    ( match !c with
	  Some x -> u := !u ^ String.make 1 x
	| None -> ()
    );
    c := nextchar lb
  done;
  r # close_in;
  !u = "0«»°áàâãäÁÀÂÃÄéèêëíìîïÍÌÎÏóòôõøöÓÒÔÕØÖúùûüýÿÝßç¡¿ñÑ"
;;


let t005 () =
  (* Tests automatic encoding conversion from UTF-8 to ISO-8859-1 *)
  let s = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_iso88591;
  r # init_warner (new drop_warnings);
  let lb = r # open_in Anonymous in
  let c = ref (nextchar lb) in
  assert (!c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  let u = ref "" in
  while !c <> None do
    ( match !c with
	  Some x -> u := !u ^ String.make 1 x
	| None -> ()
    );
    c := nextchar lb
  done;
  r # close_in;
  !u = "0«»°áàâãäÁÀÂÃÄéèêëíìîïÍÌÎÏóòôõøöÓÒÔÕØÖúùûüýÿÝßç¡¿ñÑ"
;;


let t006 () =
  (* Tests automatic encoding conversion from UTF-16-BE to UTF-8 
   * This variant invokes change_encoding early.
   *)
  let s = "\254\255\0000\000«\000»\000°\000á\000à\000â\000ã\000ä\000Á\000À\000Â\000Ã\000Ä\000é\000è\000ê\000ë\000í\000ì\000î\000ï\000Í\000Ì\000Î\000Ï\000ó\000ò\000ô\000õ\000ø\000ö\000Ó\000Ò\000Ô\000Õ\000Ø\000Ö\000ú\000ù\000û\000ü\000ý\000ÿ\000Ý\000ß\000ç\000¡\000¿\000ñ\000Ñ" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let lb = r # open_in Anonymous in
  let c = ref (nextchar lb) in
  assert (!c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  r # change_encoding "";
  let u = ref "" in
  while !c <> None do
    ( match !c with
	  Some x -> u := !u ^ String.make 1 x
	| None -> ()
    );
    c := nextchar lb
  done;
  r # close_in;
  !u = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145"
;;


let t007 () =
  (* Tests automatic encoding conversion from UTF-16-BE to UTF-8 
   * This variant does not invoke change_encoding
   *)
  let s = "\254\255\0000\000«\000»\000°\000á\000à\000â\000ã\000ä\000Á\000À\000Â\000Ã\000Ä\000é\000è\000ê\000ë\000í\000ì\000î\000ï\000Í\000Ì\000Î\000Ï\000ó\000ò\000ô\000õ\000ø\000ö\000Ó\000Ò\000Ô\000Õ\000Ø\000Ö\000ú\000ù\000û\000ü\000ý\000ÿ\000Ý\000ß\000ç\000¡\000¿\000ñ\000Ñ" in
  let r = new resolve_read_this_string s in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let lb = r # open_in Anonymous in
  let c = ref (nextchar lb) in
  assert (!c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  let u = ref "" in
  while !c <> None do
    ( match !c with
	  Some x -> u := !u ^ String.make 1 x
	| None -> ()
    );
    c := nextchar lb
  done;
  r # close_in;
  !u = "0\194\171\194\187\194\176\195\161\195\160\195\162\195\163\195\164\195\129\195\128\195\130\195\131\195\132\195\169\195\168\195\170\195\171\195\173\195\172\195\174\195\175\195\141\195\140\195\142\195\143\195\179\195\178\195\180\195\181\195\184\195\182\195\147\195\146\195\148\195\149\195\152\195\150\195\186\195\185\195\187\195\188\195\189\195\191\195\157\195\159\195\167\194\161\194\191\195\177\195\145"
;;

(**********************************************************************)

let t100 () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let cwd = Sys.getcwd() in
  let lb = r # open_in (System ("file://localhost" ^ cwd ^ "/t100.dat")) in
  let c = nextchar lb in
  assert (c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  (* Note: the end of lb.lex_buffer is filled up, so lb.lex_curr_pos must
   * now be at the end of the buffer indicating that the buffer is now
   * empty.
   *)
  for i = 1 to 8 do
    ignore(nextchar lb);
  done;
  let c = nextchar lb in
  assert (c = Some '9');
  r # close_in;
  true
;;

let t101 () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let cwd = Sys.getcwd() in
  let lb = r # open_in (System ("//localhost" ^ cwd ^ "/t100.dat")) in
  let c = nextchar lb in
  assert (c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  (* Note: the end of lb.lex_buffer is filled up, so lb.lex_curr_pos must
   * now be at the end of the buffer indicating that the buffer is now
   * empty.
   *)
  for i = 1 to 8 do
    ignore(nextchar lb);
  done;
  let c = nextchar lb in
  assert (c = Some '9');
  r # close_in;
  true
;;

let t102 () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let cwd = Sys.getcwd() in
  let lb = r # open_in (System (cwd ^ "/t100.dat")) in
  let c = nextchar lb in
  assert (c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  (* Note: the end of lb.lex_buffer is filled up, so lb.lex_curr_pos must
   * now be at the end of the buffer indicating that the buffer is now
   * empty.
   *)
  for i = 1 to 8 do
    ignore(nextchar lb);
  done;
  let c = nextchar lb in
  assert (c = Some '9');
  r # close_in;
  true
;;

let t103 () =
  (* Reads from a file without recoding it *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let lb = r # open_in (System "t100.dat") in
  let c = nextchar lb in
  assert (c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  (* Note: the end of lb.lex_buffer is filled up, so lb.lex_curr_pos must
   * now be at the end of the buffer indicating that the buffer is now
   * empty.
   *)
  for i = 1 to 8 do
    ignore(nextchar lb);
  done;
  let c = nextchar lb in
  assert (c = Some '9');
  r # close_in;
  true
;;

(**********************************************************************)

let t110 () =
  (* Checks whether relative URLs are properly handled *)
  let r = new resolve_as_file () in
  r # init_rep_encoding `Enc_utf8;
  r # init_warner (new drop_warnings);
  let lb = r # open_in (System "t100.dat") in
  let c = nextchar lb in
  assert (c = Some '0');
  assert (lb.Lexing.lex_curr_pos = lb.Lexing.lex_buffer_len);
  (* Note: the end of lb.lex_buffer is filled up, so lb.lex_curr_pos must
   * now be at the end of the buffer indicating that the buffer is now
   * empty.
   *)
  for i = 1 to 8 do
    ignore(nextchar lb);
  done;
  let r' = r # clone in
  let lb' = r' # open_in (System "t100.dat") in
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

test t001 "001";;
test t002 "002";;
test t003 "003";;
test t004 "004";;
test t005 "005";;
test t006 "006";;
test t007 "007";;

test t100 "100";;
test t101 "101";;
test t102 "102";;
test t103 "103";;

test t110 "110";;

test t200 "200";;
test t201 "201";;
test t202 "202";;
