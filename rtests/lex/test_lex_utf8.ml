(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_lexer_types

let mk_utf8_string l =
  let n = List.length l in
  let s = String.create (2*n) in
  for k = 0 to n-1 do
    let v = List.nth l k in
    s.[2*k]   <- Char.chr(v land 0xff);
    s.[2*k+1] <- Char.chr(v lsr 8);
  done;
  Netconversion.recode_string `Enc_utf16_le `Enc_utf8 s
;;

let scan s =
  let lbuf = Lexing.from_string s in
  let scanner = (Pxp_lexers.get_lexer_set `Enc_utf8).scan_name_string in
  let rec scan_rest () =
    match scanner lbuf with
	Eof -> []
      | tok -> tok :: scan_rest()
  in
  scan_rest()
;;

let dotest name f =
  print_string ("Test " ^ name ^ ": ");
  flush stdout;
  try
    if f () then
      print_endline "OK"
    else
      print_endline "FAILED (returns false)"
  with
      ex ->
        print_endline ("FAILED (exception " ^ Printexc.to_string ex ^ ")")
;;

(**********************************************************************)

let name1 = [ 0x03d0; 0x03d1; 0x0958; 0x0041; 0x0A05 ];;
let name2 = [ 0x10d0; 0x11ba; 0x2126; 0xAc00; 0xac01 ];;
let name3 = [ 0xd700; 0xd701; 0x3105; 0x1f5d; 0x114c ];;

let s_name1 = mk_utf8_string name1 ;;
let s_name2 = mk_utf8_string name2 ;;
let s_name3 = mk_utf8_string name3 ;;

let ideo1 = [ 0x4e00; 0x3007; 0x9fa4 ];;
let ideo2 = [ 0x3021; 0x3026; 0x3027 ];;

let s_ideo1 = mk_utf8_string ideo1 ;;
let s_ideo2 = mk_utf8_string ideo2 ;;

let digit1 = [ 0x0030; 0x0031 ];;
let digit2 = [ 0x09e6; 0x09ef ];;
let digit3 = [ 0x30fc; 0x30fe ];;

let s_digit1 = mk_utf8_string digit1 ;;
let s_digit2 = mk_utf8_string digit2 ;;
let s_digit3 = mk_utf8_string digit3 ;;

let other1_1 = [ 0x00f7 ];;
let other1_2 = [ 0x0a00 ];;
let other1 = other1_1 @ other1_2 ;;
let other2_1 = [ 0x1000 ];;
let other2_2 = [ 0x2000 ];;
let other2 = other2_1 @ other2_2 ;;

let s_other1_1 = mk_utf8_string other1_1 ;;
let s_other1_2 = mk_utf8_string other1_2 ;;
let s_other1 = mk_utf8_string other1 ;;
let s_other2_1 = mk_utf8_string other2_1 ;;
let s_other2_2 = mk_utf8_string other2_2 ;;
let s_other2 = mk_utf8_string other2 ;;

let t_name1 () =
  scan s_name1 = [ Name s_name1 ]
;;

let t_name2 () =
  scan s_name2 = [ Name s_name2 ]
;;

let t_name3 () =
  scan s_name3 = [ Name s_name3 ]
;;

let t_digit1 () =
  scan s_digit1 = [ Nametoken s_digit1 ]
;;

let t_digit2 () =
  scan s_digit2 = [ Nametoken s_digit2 ]
;;

let t_digit3 () =
  scan s_digit3 = [ Nametoken s_digit3 ]
;;

let t_name1digit1 () =
  scan (s_name1 ^ s_digit1) = [ Name (s_name1 ^ s_digit1) ]
;;

let t_name2digit2 () =
  scan (s_name2 ^ s_digit2) = [ Name (s_name2 ^ s_digit2) ]
;;

let t_name3digit3 () =
  scan (s_name3 ^ s_digit3) = [ Name (s_name3 ^ s_digit3) ]
;;

let t_name1ideo1 () =
  scan (s_name1 ^ s_ideo1) = [ Name (s_name1 ^ s_ideo1) ]
;;

let t_name2ideo2 () =
  scan (s_name2 ^ s_ideo2) = [ Name (s_name2 ^ s_ideo2) ]
;;

let t_digit1name1 () =
  scan (s_digit1 ^ s_name1) = [ Nametoken (s_digit1 ^ s_name1) ]
;;

let t_digit2name2 () =
  scan (s_digit2 ^ s_name2) = [ Nametoken (s_digit2 ^ s_name2) ]
;;

let t_digit3name3 () =
  scan (s_digit3 ^ s_name3) = [ Nametoken (s_digit3 ^ s_name3) ]
;;

let t_other1 () =
  scan s_other1 = [ CharData s_other1_1; CharData s_other1_2 ]
;;

let t_other2 () =
  scan s_other2 = [ CharData s_other2_1; CharData s_other2_2 ]
;;

let t_name1other1name2 () =
  scan (s_name1 ^ s_other1 ^ s_name2 ) =
  [ Name s_name1; CharData s_other1_1; CharData s_other1_2; Name s_name2 ]
;;

let t_name2other2name3 () =
  scan (s_name2 ^ s_other2 ^ s_name3 ) =
  [ Name s_name2; CharData s_other2_1; CharData s_other2_2; Name s_name3 ]
;;

let t_digit1other1digit2 () =
  scan (s_digit1 ^ s_other1 ^ s_digit2 ) =
  [ Nametoken s_digit1; 
    CharData s_other1_1; CharData s_other1_2; 
    Nametoken s_digit2 ]
;;

let t_digit2other2digit3 () =
  scan (s_digit2 ^ s_other2 ^ s_digit3 ) =
  [ Nametoken s_digit2; 
    CharData s_other2_1; CharData s_other2_2; 
    Nametoken s_digit3 ]
;;

let t_illegal1 () =
  let chr x = String.make 1 (Char.chr x) in
  try
    (* The wrong way to encode 0x00: *)
    let _ = scan (chr 0b11000000 ^ chr 0b10000000) in
    false
  with
      Netconversion.Malformed_code ->
	true
;;

let t_illegal2 () =
  let chr x = String.make 1 (Char.chr x) in
  try
    (* The wrong way to encode 0x40: *)
    let _ = scan (chr 0b11000001 ^ chr 0b10000000) in
    false
  with
      Netconversion.Malformed_code ->
	true
;;

let t_illegal3 () =
  let chr x = String.make 1 (Char.chr x) in
  try
    (* The wrong way to encode 0x00: *)
    let _ = scan (chr 0b11100000 ^ chr 0b10000000 ^ chr 0b10000000) in
    false
  with
      Netconversion.Malformed_code ->
	true
;;

let t_illegal4 () =
  let chr x = String.make 1 (Char.chr x) in
  try
    (* The wrong way to encode 0x0700: *)
    let _ = scan (chr 0b11100000 ^ chr 0b10011100 ^ chr 0b10000000) in
    false
  with
      Netconversion.Malformed_code ->
	true
;;

let t_illegal5 () =
  let chr x = String.make 1 (Char.chr x) in
  try
    (* The wrong way to encode 0x00: *)
    let _ = scan (chr 0b11110000 ^ chr 0b10000000 ^ chr 0b10000000 ^ chr 0b10000000) in
    false
  with
      Netconversion.Malformed_code ->
	true
;;

let t_illegal6 () =
  let chr x = String.make 1 (Char.chr x) in
  try
    (* The wrong way to encode 0x0700: *)
    let _ = scan (chr 0b11110000 ^ chr 0b10000000 ^ chr 0b10011100 ^ chr 0b10000000) in
    false
  with
      Netconversion.Malformed_code ->
	true
;;


let t_illegal7 () =
  let chr x = String.make 1 (Char.chr x) in
  try
    (* The wrong way to encode 0x7000: *)
    let _ = scan (chr 0b11110000 ^ chr 0b10000111 ^ chr 0b10000000 ^ chr 0b10000000) in
    false
  with
      Netconversion.Malformed_code ->
	true
;;

(**********************************************************************)

dotest "t_name1" t_name1;;
dotest "t_name2" t_name2;;
dotest "t_name3" t_name3;;

dotest "t_digit1" t_digit1;;
dotest "t_digit2" t_digit2;;
dotest "t_digit3" t_digit3;;

dotest "t_name1digit1" t_name1digit1;;
dotest "t_name2digit2" t_name2digit2;;
dotest "t_name3digit3" t_name3digit3;;

dotest "t_name1ideo1" t_name1ideo1;;
dotest "t_name2ideo2" t_name2ideo2;;

dotest "t_digit1name1" t_digit1name1;;
dotest "t_digit2name2" t_digit2name2;;
dotest "t_digit3name3" t_digit3name3;;

dotest "t_other1" t_other1;;
dotest "t_other2" t_other2;;

dotest "t_name1other1name2" t_name1other1name2;;
dotest "t_name2other2name3" t_name2other2name3;;

dotest "t_digit1other1digit2" t_digit1other1digit2;;
dotest "t_digit2other2digit3" t_digit2other2digit3;;

dotest "t_illegal1" t_illegal1;;
dotest "t_illegal2" t_illegal2;;
dotest "t_illegal3" t_illegal3;;
dotest "t_illegal4" t_illegal4;;
dotest "t_illegal5" t_illegal5;;
dotest "t_illegal6" t_illegal6;;
dotest "t_illegal7" t_illegal7;;

