(* $Id: validate.ml,v 1.1 1999/08/14 22:20:53 gerd Exp $
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


let rec print_error e =
  match e with
      Markup_types.At(where,what) ->
	print_endline where;
	print_error what
    | _ ->
	print_endline (Printexc.to_string e)
;;


let parse debug filename =
  try 
    let _ =
      parse_document_entity
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
	print_error e
;;


let main() =
  let debug = ref false in
  let files = ref [] in
  Arg.parse
      [ "-d", Arg.Set debug, "turn debugging mode on";
      ]
      (fun x -> files := x :: !files)
      "
usage: validate [options] file ...

- checks the validity of XML documents. See below for list of options.

<title>Markup! The XML parser for Objective Caml</title>

List of options:";
  files := List.rev !files;
  List.iter (parse !debug) !files
;;


main();;


(* ======================================================================
 * History:
 * 
 * $Log: validate.ml,v $
 * Revision 1.1  1999/08/14 22:20:53  gerd
 * 	Initial revision.
 *
 * 
 *)
