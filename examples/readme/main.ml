(* $Id: main.ml,v 1.2 1999/08/23 16:54:19 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Markup_types
open Markup_document
open Markup_yacc


let rec print_error e =
  match e with
      Markup_types.At(where,what) ->
        prerr_endline where;
        print_error what
    | _ ->
        prerr_endline (Printexc.to_string e)
;;


let run f a =
  try f a with
      e -> print_error e
;;


let convert_to_html filename =
  (* read in style definition *)
  let document =
    parse_document_entity
      default_config
      (File filename)
      To_html.tag_map
  in
  let root = document # root in
  let store = new To_html.store in
  root # extension # to_html store stdout
;;


let convert_to_text filename =
  (* read in style definition *)
  let document =
    parse_document_entity
      default_config
      (File filename)
      To_text.tag_map
  in
  let root = document # root in
  let store = new To_text.store in
  let box = new To_text.box 79 79 in
  root # extension # to_box store box;
  box # output 0 0 stdout
;;


let main() =
  let want_html = ref false in
  let want_text = ref false in
  let filename = ref None in
  Arg.parse
      [ "-html", Arg.Set want_html, 
	      "  convert file to html";
	"-text", Arg.Set want_text,
	      "  convert file to text";
      ]
      (fun s -> 
	 match !filename with
	     None -> filename := Some s
	   | Some _ ->
	       raise (Arg.Bad "Multiple arguments not allowed."))
      "usage: readme [ -text | -html ] input.xml >output";
  let fn =
    match !filename with
	None -> 
	  prerr_endline "readme: no input";
	  exit 1
      | Some s -> s
  in
  match !want_html, !want_text with
      true, false ->
	run convert_to_html fn
    | false, true ->
	run convert_to_text fn
    | _ ->
	prerr_endline ("readme: Please select exactly one output format")
;;

main();;

(* ======================================================================
 * History:
 *
 * $Log: main.ml,v $
 * Revision 1.2  1999/08/23 16:54:19  gerd
 * 	Minor changes.
 *
 * Revision 1.1  1999/08/22 22:29:32  gerd
 * 	Initial revision.
 *
 *)
