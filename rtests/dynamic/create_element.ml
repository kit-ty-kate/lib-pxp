(* $Id: create_element.ml,v 1.2 2001/06/04 18:54:36 gerd Exp $ *)

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
  List.sort ~cmp:(fun (a,_) (b,_) -> Pervasives.compare a b) atts
;;

let dotest name f creator =
  print_string ("Test " ^ name ^ ": ");
  flush stdout;
  try
    if f creator then
      print_endline "OK"
    else
      print_endline "FAILED (returns false)"
  with
      ex ->
	print_endline ("FAILED (exception " ^ Printexc.to_string ex ^ ")")
;;


(**********************************************************************)
(* 00x: several possibilities to create x *)


let test001 create =
  let e = create
	    ~att_values:["c_req", Value "rv"]
	    spec dtd "x" []
  in
  let atts = sorted_atts e in
  atts = ["c_def", Value "42";
	  "c_fix", Value "Q";
	  "c_imp", Implied_value;
	  "c_req", Value "rv"]
;;


let test002 create =
  let e = create
	    ~att_values:["c_req", Value "rv"; "c_fix", Value "Q"]
	    spec dtd "x" []
  in
  let atts = sorted_atts e in
  atts = ["c_def", Value "42";
	  "c_fix", Value "Q";
	  "c_imp", Implied_value;
	  "c_req", Value "rv"]
;;


let test003 create =
  let e = create
	    ~att_values:["c_req", Value "rv"; "c_imp", Implied_value]
	    spec dtd "x" []
  in
  let atts = sorted_atts e in
  atts = ["c_def", Value "42";
	  "c_fix", Value "Q";
	  "c_imp", Implied_value;
	  "c_req", Value "rv"]
;;


let test004 create =
  let e = create
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

let test010 create =
  (* Missing required att *)
  try
    let e = create
	      ~att_values:[]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Required attribute `c_req' is missing") 
    | Validation_error("Attribute `c_req' has Implied_value, but is declared as #REQUIRED") ->
	true
;;


let test011 create =
  (* Bad fixed att *)
  try
    let e = create
	      ~att_values:["c_req", Value "rv"; "c_fix", Value "bad" ]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_fix' is fixed, but has here a different value") ->
	true
;;


let test012 create =
  (* Implied_value does not count as required value *)
  try
    let e = create
	      ~att_values:["c_req", Implied_value]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_req' has Implied_value, but is declared as #REQUIRED") ->
	true
;;

let test013 create =
  (* Bad fixed att (Implied_value) *)
  try
    let e = create
	      ~att_values:["c_req", Value "rv"; "c_fix", Implied_value ]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_fix' has Implied_value, but is not declared as #IMPLIED") ->
	true
;;


let test014 create =
  (* Implied_value not possible when default specified *)
  try
    let e = create 
	      ~att_values:["c_req", Value "rv"; "c_def", Implied_value ]
	      spec dtd "x" []
    in
    false
  with
      Validation_error("Attribute `c_def' has Implied_value, but is not declared as #IMPLIED") ->
	true
;;


let test015 create =
  (* Attributes must only occur once *)
  try
    let e = create 
	      ~att_values:["c_req", Value "rv"; "c_req", Value "rv" ]
	      spec dtd "x" []
    in
    false
  with
      WF_error("Attribute `c_req' occurs twice in element `x'")
    | WF_error("Attribute `c_req' occurs twice") ->
	true
;;


let test016 create =
  (* Attributes must only occur once *)
  try
    let e = create 
	      ~att_values:["c_req", Value "rv"; 
			   "c_imp", Implied_value; "c_imp", Value "imp" ]
	      spec dtd "x" []
    in
    false
  with
      WF_error("Attribute `c_imp' occurs twice in element `x'")
    | WF_error("Attribute `c_imp' occurs twice") ->
	true
;;


let test017 create =
  (* Attributes must only occur once *)
  try
    let e = create 
	      ~att_values:["c_req", Value "rv"; 
			   "c_imp", Implied_value; "c_imp", Implied_value ]
	      spec dtd "x" []
    in
    false
  with
      WF_error("Attribute `c_imp' occurs twice in element `x'")
    | WF_error("Attribute `c_imp' occurs twice") ->
	true
;;


let test018 create =
  (* Attributes must only occur once *)
  try
    let e = create 
	      ~att_values:["c_req", Value "rv"; 
			   "c_imp", Implied_value ]
	      spec dtd "x" [ "c_imp", "X" ]
    in
    false
  with
      WF_error("Attribute `c_imp' occurs twice in element `x'")
    | WF_error("Attribute `c_imp' occurs twice") ->
	true
;;


let test019 create =
  (* Attributes must be declared *)
  try
    let e = create 
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

let test100 create =
  let e = create 
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


let test101 create =
  (* CDATA pos *)
  let e = create 
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


let test102 create =
  (* CDATA neg *)
  try
    let e = create 
	      ~att_values:[ "c", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `c'") ->
	true
;;


let test103 create =
  (* ENTITY pos *)
  let e = create 
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


let test104 create =
  (* ENTITY neg *)
  try
    let e = create 
	      ~att_values:[ "ent", Value "g" ]
	      spec dtd "y" []
    in
    false
  with
      WF_error("Reference to undeclared general entity `g'") ->
	true
;;


let test105 create =
  (* ENTITY neg *)
  try
    let e = create 
	      ~att_values:[ "ent", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `ent'") ->
	true
;;


let test106 create =
  (* ENTITY neg *)
  try
    let e = create 
	      ~att_values:[ "ent", Value " e" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `ent' has leading or trailing whitespace") ->
	true
;;


let test107 create =
  (* ENTITIES pos *)
  let e = create 
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


let test108 create =
  (* ENTITIES neg *)
  try
    let e = create 
	      ~att_values:[ "ents", Valuelist [ "e"; "g" ] ]
	      spec dtd "y" []
    in
    false
  with
      WF_error("Reference to undeclared general entity `g'") ->
	true
;;


let test109 create =
  (* ENTITIES neg *)
  try
    let e = create 
	      ~att_values:[ "ents", Value "X" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A non-list value cannot be assigned to attribute `ents'") ->
	true
;;


let test110 create =
  (* ENTITIES neg *)
  try
    let e = create 
	      ~att_values:[ "ents", Valuelist [ "e"; "f " ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `ents' has leading or trailing whitespace") ->
	true
;;


let test111 create =
  (* enum pos *)
  let e = create 
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


let test112 create =
  (* enum neg *)
  try
    let e = create 
	      ~att_values:[ "enum", Value "q" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `enum' does not match one of the declared enumerator tokens") ->
	true
;;


let test113 create =
  (* enum neg *)
  try
    let e = create 
	      ~att_values:[ "enum", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `enum' has leading or trailing whitespace") ->
	true
;;


let test114 create =
  (* enum neg *)
  try
    let e = create 
	      ~att_values:[ "enum", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `enum'") ->
	true
;;


let test115 create =
  (* ID pos *)
  let e = create 
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


let test116 create =
  (* ID neg *)
  try
    let e = create 
	      ~att_values:[ "id", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `id' has leading or trailing whitespace") ->
	true
;;


let test117 create =
  (* ID neg *)
  try
    let e = create 
	      ~att_values:[ "id", Value "5" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `id' is lexically malformed") ->
	true
;;


let test118 create =
  (* ID neg *)
  try
    let e = create 
	      ~att_values:[ "id", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `id'") ->
	true
;;


let test119 create =
  (* NMTOKEN pos *)
  let e = create 
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


let test120 create =
  (* NMTOKEN neg *)
  try
    let e = create 
	      ~att_values:[ "nm", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nm' has leading or trailing whitespace") ->
	true
;;


let test121 create =
  (* NMTOKEN neg *)
  try
    let e = create 
	      ~att_values:[ "nm", Value "+5" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nm' is lexically malformed") ->
	true
;;


let test122 create =
  (* NMTOKEN neg *)
  try
    let e = create 
	      ~att_values:[ "nm", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `nm'") ->
	true
;;


let test123 create =
  (* NMTOKENS pos *)
  let e = create 
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


let test124 create =
  (* NMTOKENS neg *)
  try
    let e = create 
	      ~att_values:[ "nms", Valuelist [ "x"; "t "] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nms' has leading or trailing whitespace") ->
	true
;;


let test125 create =
  (* NMTOKENS neg *)
  try
    let e = create 
	      ~att_values:[ "nms", Valuelist [ "5 6" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nms' is lexically malformed") ->
	true
;;


let test126 create =
  (* NMTOKENS neg *)
  try
    let e = create 
	      ~att_values:[ "nms", Value "X" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A non-list value cannot be assigned to attribute `nms'") ->
	true
;;


let test127 create =
  (* NOTATION pos *)
  let e = create 
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


let test128 create =
  (* NOTATION neg *)
  try
    let e = create 
	      ~att_values:[ "nots", Value "q" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nots' does not match one of the declared notation names") ->
	true
;;


let test129 create =
  (* NOTATION neg *)
  try
    let e = create 
	      ~att_values:[ "nots", Value " t" ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("Attribute `nots' has leading or trailing whitespace") ->
	true
;;


let test130 create =
  (* NOTATION neg *)
  try
    let e = create 
	      ~att_values:[ "nots", Valuelist [ "X"; "Y" ] ]
	      spec dtd "y" []
    in
    false
  with
      Validation_error("A list value cannot be assigned to attribute `nots'") ->
	true
;;

(**********************************************************************)

let test_series create =
  dotest "001" test001 create;
  dotest "002" test002 create;
  dotest "003" test003 create;
  dotest "004" test004 create;

  dotest "010" test010 create;
  dotest "011" test011 create;
  dotest "012" test012 create;
  dotest "013" test013 create;
  dotest "014" test014 create;
  dotest "015" test015 create;
  dotest "016" test016 create;
  dotest "017" test017 create;
  dotest "018" test018 create;
  dotest "019" test019 create;
  
  dotest "100" test100 create;
  dotest "101" test101 create;
  dotest "102" test102 create;
  dotest "103" test103 create;
  dotest "104" test104 create;
  dotest "105" test105 create;
  dotest "106" test106 create;
  dotest "107" test107 create;
  dotest "108" test108 create;
  dotest "109" test109 create;
  dotest "110" test110 create;
  dotest "111" test111 create;
  dotest "112" test112 create;
  dotest "113" test113 create;
  dotest "114" test114 create;
  dotest "115" test115 create;
  dotest "116" test116 create;
  dotest "117" test117 create;
  dotest "118" test118 create;
  dotest "119" test119 create;
  dotest "120" test120 create;
  dotest "121" test121 create;
  dotest "122" test122 create;
  dotest "123" test123 create;
  dotest "124" test124 create;
  dotest "125" test125 create;
  dotest "126" test126 create;
  dotest "127" test127 create;
  dotest "128" test128 create;
  dotest "129" test129 create;
  dotest "130" test130 create;
  ()
;;

print_endline "Series: create_element, early validation";
test_series 
  (fun ~att_values spec dtd name atts ->
     create_element_node ~att_values spec dtd name atts);

print_endline "Series: create_element, deferred validation";
test_series
  (fun ~att_values spec dtd name atts ->
     let e =
       try
	 create_element_node 
	   ~att_values
	   ~valcheck:false
	   spec dtd name atts
       with
	 | WF_error _ as e ->
	     (* Well-formedness errors will be raised here *)
	     raise e
	 | e ->
	     (* Validation errors must not happen here. So catch them
	      * and report them.
	      *)
	     failwith ("Early exception: " ^ Printexc.to_string e)
     in
     e # complement_attlist();
     e # validate_attlist();
     e
  );
()
;;
