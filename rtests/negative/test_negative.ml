(* $Id: test_negative.ml,v 1.1 2000/05/01 15:58:50 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Markup_document;;
open Markup_yacc;;
open Markup_types;;

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
  let dom = 
    let d = Hashtbl.create 2 in
    let e = new element_impl default_extension in
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
        }
	(File filename)
	dom 
    in
    let s = default_config.warner # print_warnings in
    if s <> "" then prerr_endline s;
    default_config.warner # reset;
    print_endline "Parsed without error";
  with
      e ->
	let s = default_config.warner # print_warnings in
	if s <> "" then prerr_endline s;
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
usage: test_negative [options] file ...

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf) !files;
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: test_negative.ml,v $
 * Revision 1.1  2000/05/01 15:58:50  gerd
 * 	Initial revision.
 *
 * 
 *)
