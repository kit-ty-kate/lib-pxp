(* $Id: validate.ml,v 1.3 1999/11/09 22:27:30 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Markup_document;;
open Markup_yacc;;
open Markup_types;;

let pr2 f a b = try f a b with Markup_types.At(where,what) -> print_endline where; print_endline (Printexc.to_string what); raise Not_found;;
let pr3 f a b c = try f a b c with Markup_types.At(where,what) -> print_endline where; print_endline (Printexc.to_string what); raise Not_found;;
let pr4 f a b c d = try f a b c d  with Markup_types.At(where,what) -> print_endline where; print_endline (Printexc.to_string what); raise Not_found;;
let pr5 f a b c d e = try f a b c d e  with Markup_types.At(where,what) -> print_endline where; print_endline (Printexc.to_string what); raise Not_found;;


let error_happened = ref false;;

let rec print_error e =
  match e with
      Markup_types.At(where,what) ->
	print_endline where;
	print_error what
    | _ ->
	print_endline (Printexc.to_string e)
;;


let parse debug wf filename =
  try 
    let _ =
      (if wf then parse_wf_entity else parse_document_entity)
	{ default_config with debugging_mode = debug }
	(ExtID (System filename))
	default_dom 
    in
    let s = default_config.warner # print_warnings in
    if s <> "" then print_endline s;
    default_config.warner # reset
  with
      e ->
	let s = default_config.warner # print_warnings in
	if s <> "" then print_endline s;
	default_config.warner # reset;
	error_happened := true;
	print_error e
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
usage: validate [options] file ...

- checks the validity of XML documents. See below for list of options.

<title>Markup! The XML parser for Objective Caml</title>

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf) !files;
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: validate.ml,v $
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
