(* $Id: test_canonxml.ml,v 1.1 2000/04/30 20:13:01 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Markup_document;;
open Markup_yacc;;
open Markup_types;;

let error_happened = ref false;;

let rec prerr_error e =
  match e with
      Markup_types.At(where,what) ->
	prerr_endline where;
	prerr_error what
    | _ ->
	prerr_endline (Printexc.to_string e)
;;


let output_utf8 s =
  for i = 0 to String.length s - 1 do
    let c = Char.code(s.[i]) in
    if c <= 127 then
      print_char(Char.chr(c))
    else begin
      print_char(Char.chr(0xc0 lor (c lsr 6)));
      print_char(Char.chr(0x80 lor (c land 0x3f)));
    end
  done
;;


let re = Str.regexp "[&<>\"\009\010\013]";;

let escaped s =
  Str.global_substitute 
    re
    (fun _ ->
       match Str.matched_string s with
	   "&"    -> "&amp;"
	 | "<"    -> "&lt;"
	 | ">"    -> "&gt;"
	 | "\""   -> "&quot;"
	 | "\009" -> "&#9;"
	 | "\010" -> "&#10;"
	 | "\013" -> "&#13;"
	 | _      -> assert false
    )
    s
;;


let rec output_xml n =
  match n # node_type with
      T_element "-vr" ->
	n # iter_nodes output_xml
    | T_element "-pi" ->
	let [ pi_name ] = n # pinstr_names in
	let [ pi ] = n # pinstr pi_name in
	output_utf8 "<?";
	output_utf8 (pi # target);
	output_utf8 " ";
	output_utf8 (pi # value);
	output_utf8 "?>";
    | T_element name ->
	output_utf8 "<";
	output_utf8 name;
	let sorted_attnames = 
	  Sort.list ( <= ) (n # attribute_names) in
	List.iter
	  (fun attname ->
	     match n # attribute attname with
		 Value v ->
		   output_utf8 " ";
		   output_utf8 attname;
		   output_utf8 "=\"";
		   output_utf8 (escaped v);
		   output_utf8 "\"";
	       | Valuelist vl ->
		   let v = String.concat " " vl in
		   output_utf8 " ";
		   output_utf8 attname;
		   output_utf8 "=\"";
		   output_utf8 (escaped v);
		   output_utf8 "\"";
	       | Implied_value -> 
		   ()
	  )
	  sorted_attnames;
	output_utf8 ">";
	n # iter_nodes output_xml;
	output_utf8 "</";
	output_utf8 name;
	output_utf8 ">";
    | T_data ->
	let v = n # data in
	output_utf8 (escaped v)
;;


let parse debug wf filename =
  let dom = 
    let d = Hashtbl.create 2 in
    let e = new element_impl default_extension in
    e # keep_always_whitespace_mode;
    Hashtbl.add d T_data (new data_impl default_extension "");
    { map = d;
      default_element = e
    }
  in
  try 
    let tree =
      (if wf then parse_wf_entity else parse_document_entity)
	{ default_config with 
	    debugging_mode = debug;
	    processing_instructions_inline = true;
	    virtual_root = true;
        }
	(File filename)
	dom 
    in
    let s = default_config.warner # print_warnings in
    if s <> "" then prerr_endline s;
    default_config.warner # reset;
    output_xml (tree # root)
  with
      e ->
	let s = default_config.warner # print_warnings in
	if s <> "" then prerr_endline s;
	default_config.warner # reset;
	error_happened := true;
	prerr_error e
;;


let main() =
  let debug = ref false in
  let wf = ref false in
  let files = ref [] in
  Arg.parse
      [ "-d",   Arg.Set debug, "turn debugging mode on";
	"-wf",  Arg.Set wf,    "check only on well-formedness";
      ]
      (fun x -> files := x :: !files)
      "
usage: test_canonxml [options] file ...

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf) !files;
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: test_canonxml.ml,v $
 * Revision 1.1  2000/04/30 20:13:01  gerd
 * 	Initial revision.
 *
 * Revision 1.3  1999/11/09 22:27:30  gerd
 * 	The programs returns now an exit code of 1 if one of the
 * XML files produces an error.
 *
 * Revision 1.2  1999/09/01 23:09:56  gerd
 * 	Added the option -wf that switches to well-formedness checking
 * instead of validation.
 *
 * Revision 1.1  1999/08/14 22:20:53  gerd
 * 	Initial revision.
 *
 * 
 *)
