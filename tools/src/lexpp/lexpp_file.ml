(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Printf;;

let section_re =
  Netstring_str.regexp "^[(][*][ \t]*\\[\\([A-Za-z0-9_-]+\\)\\][ \t]*[*][)]";;

let read_sections filename =
  let f = open_in filename in
  printf "[reading %s]\n" filename; flush stdout;
  let current_section = ref None in
  let current_data = Buffer.create 1000 in
  let sections = ref [] in
  let save_section() =
    match !current_section with
	None -> ()
      | Some s ->
	  sections := (s, Buffer.contents current_data) :: !sections;
	  current_section := None;
  in
  try
    while true do
      let line = input_line f in
      match Netstring_str.string_match section_re line 0 with
	  Some mtch ->
	    let section_name = Netstring_str.matched_group mtch 1 line in
	    (* save old section: *)
	    save_section();
	    (* begin new section: *)
	    current_section := Some section_name;
	    Buffer.clear current_data;
	| None ->
	    Buffer.add_string current_data line;
	    Buffer.add_char current_data '\n';
    done;
    assert false
  with
      End_of_file ->
	close_in f;
	save_section();
	List.rev !sections
;;


let parse_char_classes s =
  Uni_parser.main Uni_lexer.token (Lexing.from_string s)
;;


(* The following printing functions have originally been written by Claudio
 * Sacerdoti Coen.
 *)

(* padded_string_of_int i returns the string representing the        *)
(* integer i (i < 256) using exactly 3 digits (example: 13 -> "013") *)

let padded_string_of_int i =
 if i < 10 then
  "00" ^ string_of_int i
 else if i < 100 then
  "0" ^ string_of_int i
 else
  string_of_int i
;;

(* Two functions useful to print a definition *)

let rec print_disjunction ?(first = true) out =
 function
    [] ->
      if first then output_string out " ['b'-'a' (*empty*) ] "
  | he::tl ->
     if not first then output_string out " | " ;
     print_re out he ;
     print_disjunction ~first:false out tl

and print_re out =
 function
    Uni_types.Char i -> output_string out ("'\\" ^ padded_string_of_int i ^ "'")
  | Uni_types.Interval (l,u) ->
      output_string out ("['\\" ^ padded_string_of_int l ^ "'-'\\" ^
			 padded_string_of_int u ^ "']")
  | Uni_types.Identifier i -> output_string out i
  | Uni_types.Concat rell ->
     let foo rel =
      if List.length rel > 1 then
       (output_string out "(" ; print_disjunction out rel ;
	output_string out ")")
      else
       print_disjunction out rel
     in
      List.iter foo rell
;;

(* print_definition prints a definition in the format expected by ocamllex *)

let print_definition out { Uni_types.id = id ; Uni_types.rel = rel } =
 output_string out ("let " ^ id ^ " =\n   ") ;
 print_disjunction out rel ;
 output_string out "\n\n"
;;

