#! /bin/sh
# (*
exec ocamlfattop "$0"
*) directory ".";;

open Str;;

let name_re = regexp "(\\*\\$[ \t]*\\([a-zA-Z.-]*\\)[ \t]*\\*)";;
let subst_re = regexp "[<>&'>]";;

let begin_entity name =
  "<!ENTITY " ^  name ^ " '";;

let end_entity () =
  "'>\n"
;;


let text = ref "" in
let within_entity = ref false in
try
  while true do
    let line = read_line() in
    if string_match name_re line 0 then begin
      let name = matched_group 1 line in
      if !within_entity then
	text := !text ^ "\n" ^ end_entity();
      within_entity := false;
      if name <> "-" then begin
	text := !text ^ begin_entity name;
	within_entity := true
      end
    end
    else
      if !within_entity then begin
	let line' =
	  global_substitute subst_re 
	    (fun s ->
	       let s' = matched_group 0 s in
	       match s' with
		   "<" -> "&lt;"
		 | ">" -> "&gt;"
		 | "&" -> "&amp;"
		 | "'" -> "&apos;"
		 | _ -> assert false)
	    line
	in
	text := !text ^ "\n" ^ line'
      end
  done;
with End_of_file ->
  if !within_entity then
    text := !text ^ "\n" ^ end_entity();
  print_string !text
;;
