(* $Id: validate.ml,v 1.5 2000/06/04 20:21:55 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Pxp_document;;
open Pxp_yacc;;
open Pxp_types;;

let error_happened = ref false;;

let print_error e =
  print_endline (string_of_exn e)
;;


let parse debug wf iso88591 filename =
  try 
    (* Parse the document: *)
    let doc =
      (if wf then parse_wf_entity else parse_document_entity)
	{ default_config with 
	    debugging_mode = debug;
	    encoding = if iso88591 then `Enc_iso88591 else `Enc_utf8;
        }
	(File filename)
	default_dom 
    in
    (* Check the uniqueness of IDs: *)
    doc # root # reset_finder;
    if not wf then (try ignore(doc # root # find "") with Not_found -> ());
    (* Print warnings that have been collected while parsing: *)
    let s = default_config.warner # print_warnings in
    if s <> "" then print_endline s;
    default_config.warner # reset
  with
      e ->
	(* Something went wrong. First print warning. *)
	let s = default_config.warner # print_warnings in
	if s <> "" then print_endline s;
	default_config.warner # reset;
	(* Print error; remember that there was an error *)
	error_happened := true;
	print_error e
;;


let main() =
  let debug = ref false in
  let wf = ref false in
  let iso88591 = ref false in
  let files = ref [] in
  Arg.parse
      [ "-d",   Arg.Set debug, "turn debugging mode on";
	"-wf",  Arg.Set wf,    "check only on well-formedness";
        "-iso-8859-1", Arg.Set iso88591, "use ISO-8859-1 as internal encoding instead of UTF-8";
      ]
      (fun x -> files := x :: !files)
      "
usage: validate [options] file ...

- checks the validity of XML documents. See below for list of options.

<title>PXP - The XML parser for Objective Caml</title>

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf !iso88591) !files;
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: validate.ml,v $
 * Revision 1.5  2000/06/04 20:21:55  gerd
 * 	Updated to new module names.
 *
 * Revision 1.4  2000/05/01 16:44:57  gerd
 * 	Added check for ID uniqueness.
 * 	Using new error formatter.
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
