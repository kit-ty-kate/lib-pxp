(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* Read a record-list structure and print it *)
open Pxp_types;;
open Pxp_document;;
open Pxp_tree_parser;;

let print tree =
  iter_tree
    ~pre:
      (fun n ->
	 match n # node_type with
	     T_element "last-name" ->
	       print_endline ("Last name: " ^ n # data)
	   | T_element "first-name" ->
	       print_endline ("First name: " ^ n # data)
	   | T_element "phone" ->
	       print_endline ("Telephone number: " ^ n # data)
	   | _ ->
	       ())
    ~post:
      (fun n ->
	 match n # node_type with
	     T_element "record" -> 
	       print_newline()
	   | _ ->
	       ())
    tree
;;

let main() =
  try
    let dtd = 
      Pxp_dtd_parser.parse_dtd_entity default_config (from_file "record.dtd") in
    let tree = 
      parse_content_entity default_config (from_channel stdin) dtd default_spec in
    print tree
  with
      x ->
	prerr_endline(string_of_exn x);
	exit 1
;;


main();;

