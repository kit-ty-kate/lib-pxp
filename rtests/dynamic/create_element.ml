(* $Id: create_element.ml,v 1.1 2001/06/03 14:25:49 gerd Exp $ *)

(* This test checks whether create_element processes the attribute list
 * correctly. The attributes are passed using the ~att_values argument, and
 * not by the main argument list. The latter is already implicitly tested
 * by the normal parser tests.
 *)

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
       "<!NOTATION m PUBLIC 'text/m'>
        <!NOTATION n PUBLIC 'text/n'>
        <!ENTITY e PUBLIC 'e' '' NDATA m>
        <!ENTITY f PUBLIC 'f' '' NDATA n>
        <!ELEMENT x ANY>
        <!ATTLIST x c_req CDATA #REQUIRED
                    c_def CDATA '42'
                    c_fix CDATA #FIXED 'Q'
                    c_imp CDATA #IMPLIED>
        <!ELEMENT y ANY>
        <!ATTLIST y c CDATA #IMPLIED
                    nm NMTOKEN #IMPLIED
                    nms NMTOKENS #IMPLIED
                    id ID #IMPLIED
                    enum (r|s|t) #IMPLIED
                    ent ENTITY #IMPLIED
                    ents ENTITIES #IMPLIED
                    nots NOTATION (m|n) #IMPLIED>
       ")
;;

let spec = default_spec;;

let sorted_atts e =
  let atts = e # attributes in
  List.sort (fun (a,_) (b,_) -> Pervasives.compare a b) atts
;;

let dotest name f =
  print_string ("Test " ^ name ^ ": ");
  flush stdout;
  try
    if f() then
      print_endline "OK"
    else
      print_endline "FAILED (returns false)"
  with
      ex ->
	print_endline ("FAILED (exception " ^ Printexc.to_string ex ^ ")")
;;


(**********************************************************************)
(* 00x: several possibilities to create x *)


let test001 () =
  let e = create_element_node 
	    ~att_values:["c_req", Value "rv"]
	    spec dtd "x" []
  in
  let atts = sorted_atts e in
  atts = ["c_def", Value "42";
	  "c_fix", Value "Q";
	  "c_imp", Implied_value;
	  "c_req", Value "rv"]
;;


let test002 () =
  let e = create_element_node 
	    ~att_values:["c_req", Value "rv"; "c_fix", Value "Q"]
	    spec dtd "x" []
  in
  let atts = sorted_atts e in
  atts = ["c_def", Value "42";
	  "c_fix", Value "Q";
	  "c_imp", Implied_value;
	  "c_req", Value "rv"]
;;


let test003 () =
  let e = create_element_node 
	    ~att_values:["c_req", Value "rv"; "c_imp", Implied_value]
	    spec dtd "x" []
  in
  let atts = sorted_atts e in
  atts = ["c_def", Value "42";
	  "c_fix", Value "Q";
	  "c_imp", Implied_value;
	  "c_req", Value "rv"]
;;


let test004 () =
  let e = create_element_node 
	    ~att_values:["c_req", Value "rv"; "c_def", Value "43"]
	    spec dtd "x" []
  in
  let atts = sorted_atts e in
  atts = ["c_def", Value "43";
	  "c_fix", Value "Q";
	  "c_imp", Implied_value;
	  "c_req", Value "rv"]
;;

(**********************************************************************)
(* 01x: several error conditions when creating x *)

let test010 () =
  (* Missing required att *)
  try
    let e = create_element_node 
	      ~att_values:[]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Required attribute `c_req' is missing") ->
	true
;;


let test011 () =
  (* Bad fixed att *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; "c_fix", Value "bad" ]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_fix' is fixed, but has here a different value") ->
	true
;;


let test012 () =
  (* Implied_value does not count as required value *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Implied_value]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_req' has Implied_value, but is declared as #REQUIRED") ->
	true
;;

let test013 () =
  (* Bad fixed att (Implied_value) *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; "c_fix", Implied_value ]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_fix' has Implied_value, but is not declared as #IMPLIED") ->
	true
;;


let test014 () =
  (* Implied_value not possible when default specified *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; "c_def", Implied_value ]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_def' has Implied_value, but is not declared as #IMPLIED") ->
	true
;;


let test015 () =
  (* Attributes must only occur once *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; "c_req", Value "rv" ]
	      spec dtd "x" []
    in
    false
  with
      WF_error("Attribute `c_req' occurs twice in element `x'") ->
	true
;;


let test016 () =
  (* Attributes must only occur once *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; 
			   "c_imp", Implied_value; "c_imp", Value "imp" ]
	      spec dtd "x" []
    in
    false
  with
      WF_error("Attribute `c_imp' occurs twice in element `x'") ->
	true
;;


let test017 () =
  (* Attributes must only occur once *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; 
			   "c_imp", Implied_value; "c_imp", Implied_value ]
	      spec dtd "x" []
    in
    false
  with
      WF_error("Attribute `c_imp' occurs twice in element `x'") ->
	true
;;


let test018 () =
  (* Attributes must only occur once *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; 
			   "c_imp", Implied_value ]
	      spec dtd "x" [ "c_imp", "X" ]
    in
    false
  with
      WF_error("Attribute `c_imp' occurs twice in element `x'") ->
	true
;;


let test019 () =
  (* Attributes must be declared *)
  try
    let e = create_element_node 
	      ~att_values:["c_req", Value "rv"; "foo", Value "y" ]
	      spec dtd "x" [ "c_imp", "X" ]
    in
    false
  with
      Validation_error("The following attributes are not declared: foo") ->
	true
;;

(**********************************************************************)
(* 1xx: several possibilities to create y, and several error conditions *)

let test100 () =
  let e = create_element_node 
	    ~att_values:[]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Implied_value;
	  "ents", Implied_value;
	  "enum", Implied_value;
	  "id", Implied_value;
	  "nm", Implied_value;
	  "nms", Implied_value;
	  "nots", Implied_value
	 ]
;;


let test101 () =
  (* CDATA pos *)
  let e = create_element_node 
	    ~att_values:[ "c", Value "X" ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Value "X";
	  "ent", Implied_value;
	  "ents", Implied_value;
	  "enum", Implied_value;
	  "id", Implied_value;
	  "nm", Implied_value;
	  "nms", Implied_value;
	  "nots", Implied_value
	 ]
;;


let test102 () =
  (* CDATA neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "c", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `c'") ->
	true
;;


let test103 () =
  (* ENTITY pos *)
  let e = create_element_node 
	    ~att_values:[ "ent", Value "e" ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Value "e";
	  "ents", Implied_value;
	  "enum", Implied_value;
	  "id", Implied_value;
	  "nm", Implied_value;
	  "nms", Implied_value;
	  "nots", Implied_value
	 ]
;;


let test104 () =
  (* ENTITY neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "ent", Value "g" ]
	      spec dtd "y" []
    in
    false
  with
      WF_error("Reference to undeclared general entity `g'") ->
	true
;;


let test105 () =
  (* ENTITY neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "ent", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `ent'") ->
	true
;;


let test106 () =
  (* ENTITY neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "ent", Value " e" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `ent' has leading or trailing whitespace") ->
	true
;;


let test107 () =
  (* ENTITIES pos *)
  let e = create_element_node 
	    ~att_values:[ "ents", Valuelist [ "e"; "f" ] ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Implied_value;
	  "ents", Valuelist [ "e"; "f" ];
	  "enum", Implied_value;
	  "id", Implied_value;
	  "nm", Implied_value;
	  "nms", Implied_value;
	  "nots", Implied_value
	 ]
;;


let test108 () =
  (* ENTITIES neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "ents", Valuelist [ "e"; "g" ] ]
	      spec dtd "y" []
    in
    false
  with
      WF_error("Reference to undeclared general entity `g'") ->
	true
;;


let test109 () =
  (* ENTITIES neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "ents", Value "X" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A non-list value cannot be assigned to attribute `ents'") ->
	true
;;


let test110 () =
  (* ENTITIES neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "ents", Valuelist [ "e"; "f " ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `ents' has leading or trailing whitespace") ->
	true
;;


let test111 () =
  (* enum pos *)
  let e = create_element_node 
	    ~att_values:[ "enum", Value "s" ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Implied_value;
	  "ents", Implied_value;
	  "enum", Value "s";
	  "id", Implied_value;
	  "nm", Implied_value;
	  "nms", Implied_value;
	  "nots", Implied_value
	 ]
;;


let test112 () =
  (* enum neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "enum", Value "q" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `enum' does not match one of the declared enumerator tokens") ->
	true
;;


let test113 () =
  (* enum neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "enum", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `enum' has leading or trailing whitespace") ->
	true
;;


let test114 () =
  (* enum neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "enum", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `enum'") ->
	true
;;


let test115 () =
  (* ID pos *)
  let e = create_element_node 
	    ~att_values:[ "id", Value "five" ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Implied_value;
	  "ents", Implied_value;
	  "enum", Implied_value;
	  "id", Value "five";
	  "nm", Implied_value;
	  "nms", Implied_value;
	  "nots", Implied_value
	 ]
;;


let test116 () =
  (* ID neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "id", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `id' has leading or trailing whitespace") ->
	true
;;


let test117 () =
  (* ID neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "id", Value "5" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `id' is lexically malformed") ->
	true
;;


let test118 () =
  (* ID neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "id", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `id'") ->
	true
;;


let test119 () =
  (* NMTOKEN pos *)
  let e = create_element_node 
	    ~att_values:[ "nm", Value "5" ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Implied_value;
	  "ents", Implied_value;
	  "enum", Implied_value;
	  "id", Implied_value;
	  "nm", Value "5";
	  "nms", Implied_value;
	  "nots", Implied_value
	 ]
;;


let test120 () =
  (* NMTOKEN neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nm", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nm' has leading or trailing whitespace") ->
	true
;;


let test121 () =
  (* NMTOKEN neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nm", Value "+5" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nm' is lexically malformed") ->
	true
;;


let test122 () =
  (* NMTOKEN neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nm", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `nm'") ->
	true
;;


let test123 () =
  (* NMTOKENS pos *)
  let e = create_element_node 
	    ~att_values:[ "nms", Valuelist [ "_six"; "5" ] ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Implied_value;
	  "ents", Implied_value;
	  "enum", Implied_value;
	  "id", Implied_value;
	  "nm", Implied_value;
	  "nms", Valuelist ["_six"; "5"];
	  "nots", Implied_value
	 ]
;;


let test124 () =
  (* NMTOKENS neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nms", Valuelist [ "x"; "t "] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nms' has leading or trailing whitespace") ->
	true
;;


let test125 () =
  (* NMTOKENS neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nms", Valuelist [ "5 6" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nms' is lexically malformed") ->
	true
;;


let test126 () =
  (* NMTOKENS neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nms", Value "X" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A non-list value cannot be assigned to attribute `nms'") ->
	true
;;


let test127 () =
  (* NOTATION pos *)
  let e = create_element_node 
	    ~att_values:[ "nots", Value "m" ]
	    spec dtd "y" []
  in
  let atts = sorted_atts e in
  atts = ["c", Implied_value;
	  "ent", Implied_value;
	  "ents", Implied_value;
	  "enum", Implied_value;
	  "id", Implied_value;
	  "nm", Implied_value;
	  "nms", Implied_value;
	  "nots", Value "m"
	 ]
;;


let test128 () =
  (* NOTATION neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nots", Value "q" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nots' does not match one of the declared notation names") ->
	true
;;


let test129 () =
  (* NOTATION neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nots", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nots' has leading or trailing whitespace") ->
	true
;;


let test130 () =
  (* NOTATION neg *)
  try
    let e = create_element_node 
	      ~att_values:[ "nots", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `nots'") ->
	true
;;

(**********************************************************************)

dotest "001" test001;;
dotest "002" test002;;
dotest "003" test003;;
dotest "004" test004;;

dotest "010" test010;;
dotest "011" test011;;
dotest "012" test012;;
dotest "013" test013;;
dotest "014" test014;;
dotest "015" test015;;
dotest "016" test016;;
dotest "017" test017;;
dotest "018" test018;;
dotest "019" test019;;

dotest "100" test100;;
dotest "101" test101;;
dotest "102" test102;;
dotest "103" test103;;
dotest "104" test104;;
dotest "105" test105;;
dotest "106" test106;;
dotest "107" test107;;
dotest "108" test108;;
dotest "109" test109;;
dotest "110" test110;;
dotest "111" test111;;
dotest "112" test112;;
dotest "113" test113;;
dotest "114" test114;;
dotest "115" test115;;
dotest "116" test116;;
dotest "117" test117;;
dotest "118" test118;;
dotest "119" test119;;
dotest "120" test120;;
dotest "121" test121;;
dotest "122" test122;;
dotest "123" test123;;
dotest "124" test124;;
dotest "125" test125;;
dotest "126" test126;;
dotest "127" test127;;
dotest "128" test128;;
dotest "129" test129;;
dotest "130" test130;;
