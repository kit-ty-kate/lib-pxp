(* $Id: modify.ml,v 1.2 2001/06/28 21:24:16 gerd Exp $ *)

(* Tests delete, insert, append *)

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
        <!ATTLIST x id CDATA #REQUIRED>"
    )
;;

let spec = default_spec;;

let make_x id subelements =
  let e = create_element_node 
	    ~att_values:[ "id", Value id ]
	    spec dtd "x" [] in
  e # set_nodes subelements;
  e
;;

let rec signature n =
  let subsigs =
    List.map signature (n # sub_nodes) in
  let k = ref 0 in
  List.iter
    (fun n ->
       assert(n # node_position = !k);
       incr k
    )
    (n # sub_nodes);
  "(" ^
  n # required_string_attribute "id" ^
  (if subsigs <> [] then " " else "") ^ 
  String.concat " " subsigs ^ 
  ")"
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

let append1 () =
  let tree =
    make_x "a" [ make_x "1" []; make_x "2" [] ] in
  tree # append_node (make_x "3" []);
  signature tree = "(a (1) (2) (3))"
;;

let delete1 () =
  let two = make_x "2" [] in
  let tree =
    make_x "a" [ make_x "1" []; two; make_x "3" [] ] in
  two # remove();
  signature tree = "(a (1) (3))"
;;

let delete2 () =
  let two = make_x "2" [] in
  let tree =
    make_x "a" [ make_x "1" []; two; make_x "3" [] ] in
  tree # remove_nodes ~pos:1 ~len:1 ();
  signature tree = "(a (1) (3))"
;;

let delete3 () =
  let tree =
    make_x "a" 
      [ make_x "1" []; make_x "2" []; make_x "3" []; make_x "4" [] ] in
  tree # remove_nodes ~pos:1 ~len:2 ();
  signature tree = "(a (1) (4))"
;;

let delete4 () =
  let tree =
    make_x "a" 
      [ make_x "1" []; make_x "2" []; make_x "3" []; make_x "4" [] ] in
  tree # remove_nodes ~pos:0 ~len:2 ();
  signature tree = "(a (3) (4))"
;;

let delete5 () =
  let tree =
    make_x "a" 
      [ make_x "1" []; make_x "2" []; make_x "3" []; make_x "4" [] ] in
  tree # remove_nodes ~pos:2 ~len:2 ();
  signature tree = "(a (1) (2))"
;;

let delete6 () =
  let tree =
    make_x "a" 
      [ make_x "1" []; make_x "2" []; make_x "3" []; make_x "4" [] ] in
  tree # remove_nodes ~pos:0 ~len:4 ();
  signature tree = "(a)"
;;

let insert1 () =
  let tree =
    make_x "a" [ make_x "1" []; make_x "2" [] ] in
  let inslist =
    [ make_x "b" [ make_x "3" []; make_x "4" [] ] ] in
  tree # insert_nodes ~pos:0 inslist;
  signature tree = "(a (b (3) (4)) (1) (2))"
;;

let insert2 () =
  let tree =
    make_x "a" [ make_x "1" []; make_x "2" [] ] in
  let inslist =
    [ make_x "3" []; make_x "4" [] ] in
  tree # insert_nodes ~pos:0 inslist;
  signature tree = "(a (3) (4) (1) (2))"
;;

let insert3 () =
  let tree =
    make_x "a" [ make_x "1" []; make_x "2" [] ] in
  let inslist =
    [ make_x "b" [ make_x "3" []; make_x "4" [] ] ] in
  tree # insert_nodes ~pos:1 inslist;
  signature tree = "(a (1) (b (3) (4)) (2))"
;;

let insert4 () =
  let tree =
    make_x "a" [ make_x "1" []; make_x "2" [] ] in
  let inslist =
    [ make_x "3" []; make_x "4" [] ] in
  tree # insert_nodes ~pos:1 inslist;
  signature tree = "(a (1) (3) (4) (2))"
;;

let insert5 () =
  let tree =
    make_x "a" [ make_x "1" []; make_x "2" [] ] in
  let inslist =
    [ make_x "b" [ make_x "3" []; make_x "4" [] ] ] in
  tree # insert_nodes ~pos:2 inslist;
  signature tree = "(a (1) (2) (b (3) (4)))"
;;

let insert6 () =
  let tree =
    make_x "a" [ make_x "1" []; make_x "2" [] ] in
  let inslist =
    [ make_x "3" []; make_x "4" [] ] in
  tree # insert_nodes ~pos:2 inslist;
  signature tree = "(a (1) (2) (3) (4))"
;;

(**********************************************************************)

dotest "append1" append1;;
dotest "delete1" delete1;;
dotest "delete2" delete2;;
dotest "delete3" delete3;;
dotest "delete4" delete4;;
dotest "delete5" delete5;;
dotest "delete6" delete6;;
dotest "insert1" insert1;;
dotest "insert2" insert2;;
dotest "insert3" insert3;;
dotest "insert4" insert4;;
dotest "insert5" insert5;;
dotest "insert6" insert6;;
