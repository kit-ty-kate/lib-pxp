(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_document
open Pxp_tree_parser


let rec print_error e =
  prerr_endline(string_of_exn e)
;;


let run f a =
  try f a with
      e -> print_error e
;;


let convert_to_html filename =
  (* read in style definition *)
  let document =
    parse_document_entity
      { default_config with encoding = `Enc_iso88591 }
      (from_file filename)
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
      (from_file filename)
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

