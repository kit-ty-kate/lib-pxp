(* $Id: strip.ml,v 1.1 2001/06/04 20:09:58 gerd Exp $ *)

(* Tests strip_whitespace *)

open Pxp_types
open Pxp_yacc
open Pxp_document

let conf =
  { default_config with
      encoding = `Enc_utf8;
  }
;;

let dtd =
  parse_dtd_entity 
    conf
    (from_string 
       "<!ELEMENT x ANY>
        <!ATTLIST x xml:space (preserve|default) #IMPLIED>"
    )
;;

let spec = default_spec;;

let make_x ?(atts = []) subelements =
  let e = create_element_node 
	    ~att_values: atts
            spec dtd "x" [] in
  e # set_nodes subelements;
  e
;;

let make_text text =
  create_data_node spec dtd text
;;

let rec signature n =
  match n # node_type with
      T_element _ ->
	"[" ^ 
	String.concat " " (List.map signature (n # sub_nodes)) ^ 
	"]"
    | T_data ->
	"[" ^ n # data ^ "]"
    | _ ->
	""
;;


let dotest name f =
  print_string ("Test " ^ name ^ ": ");
  flush stdout;
  try
    if f () then
      print_endline "OK"
    else
      print_endline "FAILED (returns false)"
  with
      ex ->
        print_endline ("FAILED (exception " ^ Printexc.to_string ex ^ ")")
;;

(**********************************************************************)

let strip1 () =
  let strip_tree l r =
    let tree = make_text " A " in
    strip_whitespace ~left:l ~right:r tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[A ]") &&
  (strip_tree `Strip_one_lf `Disabled = "[ A ]") &&
  (strip_tree `Strip_seq `Disabled    = "[A ]") &&
  (strip_tree `Disabled `Strip_one    = "[ A]") &&
  (strip_tree `Disabled `Strip_one_lf = "[ A ]") &&
  (strip_tree `Disabled `Strip_seq    = "[ A]")
;;

let strip2 () =
  let strip_tree l r =
    let tree = make_text "\nA\n" in
    strip_whitespace ~left:l ~right:r tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[A\n]") &&
  (strip_tree `Strip_one_lf `Disabled = "[A\n]") &&
  (strip_tree `Strip_seq `Disabled    = "[A\n]") &&
  (strip_tree `Disabled `Strip_one    = "[\nA]") &&
  (strip_tree `Disabled `Strip_one_lf = "[\nA]") &&
  (strip_tree `Disabled `Strip_seq    = "[\nA]")
;;

let strip3 () =
  let strip_tree l r =
    let tree = make_text "  A  " in
    strip_whitespace ~left:l ~right:r tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[ A  ]") &&
  (strip_tree `Strip_one_lf `Disabled = "[  A  ]") &&
  (strip_tree `Strip_seq `Disabled    = "[A  ]") &&
  (strip_tree `Disabled `Strip_one    = "[  A ]") &&
  (strip_tree `Disabled `Strip_one_lf = "[  A  ]") &&
  (strip_tree `Disabled `Strip_seq    = "[  A]")
;;

let strip4 () =
  let strip_tree l r =
    let tree = make_text "\n\nA\n\n" in
    strip_whitespace ~left:l ~right:r tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[\nA\n\n]") &&
  (strip_tree `Strip_one_lf `Disabled = "[\nA\n\n]") &&
  (strip_tree `Strip_seq `Disabled    = "[A\n\n]") &&
  (strip_tree `Disabled `Strip_one    = "[\n\nA\n]") &&
  (strip_tree `Disabled `Strip_one_lf = "[\n\nA\n]") &&
  (strip_tree `Disabled `Strip_seq    = "[\n\nA]")
;;

let strip5 () =
  let strip_tree l r =
    let tree = make_x [ make_text " A "; make_x []; make_text " B "] in
    strip_whitespace ~left:l ~right:r tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[[A ] [] [ B ]]") &&
  (strip_tree `Strip_one_lf `Disabled = "[[ A ] [] [ B ]]") &&
  (strip_tree `Strip_seq `Disabled    = "[[A ] [] [ B ]]") &&
  (strip_tree `Disabled `Strip_one    = "[[ A ] [] [ B]]") &&
  (strip_tree `Disabled `Strip_one_lf = "[[ A ] [] [ B ]]") &&
  (strip_tree `Disabled `Strip_seq    = "[[ A ] [] [ B]]")
;;


let strip6 () =
  let strip_tree l r =
    let tree = make_x [ make_text " "; make_text " B "] in
    strip_whitespace ~left:l ~right:r ~delete_empty_nodes:false tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[[] [ B ]]") &&
  (strip_tree `Strip_one_lf `Disabled = "[[ ] [ B ]]") &&
  (strip_tree `Strip_seq `Disabled    = "[[] [ B ]]") &&
  (strip_tree `Disabled `Strip_one    = "[[ ] [ B]]") &&
  (strip_tree `Disabled `Strip_one_lf = "[[ ] [ B ]]") &&
  (strip_tree `Disabled `Strip_seq    = "[[ ] [ B]]")
;;

let strip7 () =
  let strip_tree l r =
    let tree = make_x [ make_text " "; make_text " B "] in
    strip_whitespace ~left:l ~right:r ~delete_empty_nodes:true tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[[ B ]]") &&
  (strip_tree `Strip_one_lf `Disabled = "[[ ] [ B ]]") &&
  (strip_tree `Strip_seq `Disabled    = "[[ B ]]") &&
  (strip_tree `Disabled `Strip_one    = "[[ ] [ B]]") &&
  (strip_tree `Disabled `Strip_one_lf = "[[ ] [ B ]]") &&
  (strip_tree `Disabled `Strip_seq    = "[[ ] [ B]]")
;;

let strip8 () =
  let strip_tree l r =
    let tree = make_x [ make_text " "; make_text " B "] in
    let outer = make_x ~atts:[ "xml:space", Value "preserve" ] [tree] in
    strip_whitespace ~left:l ~right:r ~delete_empty_nodes:true tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[[ ] [ B ]]") &&
  (strip_tree `Strip_one_lf `Disabled = "[[ ] [ B ]]") &&
  (strip_tree `Strip_seq `Disabled    = "[[ ] [ B ]]") &&
  (strip_tree `Disabled `Strip_one    = "[[ ] [ B ]]") &&
  (strip_tree `Disabled `Strip_one_lf = "[[ ] [ B ]]") &&
  (strip_tree `Disabled `Strip_seq    = "[[ ] [ B ]]")
;;

let strip9 () =
  let strip_tree l r =
    let tree = make_x ~atts:[ "xml:space", Value "default" ] 
                      [ make_text " "; make_text " B "] in
    let outer = make_x ~atts:[ "xml:space", Value "preserve" ] [tree] in
    strip_whitespace ~left:l ~right:r ~delete_empty_nodes:true tree;
    signature tree
  in
  (strip_tree `Strip_one `Disabled    = "[[ B ]]") &&
  (strip_tree `Strip_one_lf `Disabled = "[[ ] [ B ]]") &&
  (strip_tree `Strip_seq `Disabled    = "[[ B ]]") &&
  (strip_tree `Disabled `Strip_one    = "[[ ] [ B]]") &&
  (strip_tree `Disabled `Strip_one_lf = "[[ ] [ B ]]") &&
  (strip_tree `Disabled `Strip_seq    = "[[ ] [ B]]")
;;


(**********************************************************************)

dotest "strip1" strip1;;
dotest "strip2" strip2;;
dotest "strip3" strip3;;
dotest "strip4" strip4;;
dotest "strip5" strip5;;
dotest "strip6" strip6;;
dotest "strip7" strip7;;
dotest "strip8" strip8;;
dotest "strip9" strip9;;


