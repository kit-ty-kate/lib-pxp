(* $Id$ *)

(* Compare several ways of XML aggregation, and check their symmetry *)

open Pxp_types
open Pxp_tree_parser
open Pxp_ev_parser
open Pxp_document
open Pxp_dtd
open Pxp_dtd_parser

let dotest name f creator =
  print_string ("Test " ^ name ^ ": ");
  flush stdout;
  try
    if f creator then
      print_endline "OK"
    else
      print_endline "FAILED (returns false)"
  with
    | Failure msg ->
        print_endline ("FAILED: " ^ msg)
    | ex ->
        print_endline ("FAILED (exception " ^ string_of_exn ex ^ ")")
;;


let xml_string_no_decl = 
  "<?pi1 v1 v2?>" ^
  "<s:root xmlns:s='samplespace' a1='u1'>" ^
  "<!-- Comment -->" ^
  "<?pi2 v1 v2?>" ^
  "DATA1" ^
  "<s:sub a2='u2'>" ^
  "DATA2" ^
  "</s:sub>" ^
  "DATA3" ^
  "<?pi3 v1 v2?>" ^
  "</s:root>" ^
  "<?pi4 v1 v2?>" ^
  "<!-- Comment -->"
;;

let xml_string =
  "<?xml version='1.0'?>" ^ xml_string_no_decl ;;

let event_list ?(del_pinstr_member=true) ?(keep_positions = true) generator =
  let rec unroll() =
    match generator() with
	None -> []
      | Some (E_pinstr_member(_,_,_)) when del_pinstr_member -> unroll()
      | Some (E_position(e,l,c)) when not keep_positions -> unroll()
      | Some e -> e :: unroll()
  in
  unroll()
;;


let print_event =
  function 
      E_start_doc (version, _) ->
	"E_start_doc version=" ^ version
    | E_end_doc litname ->
	"E_end_doc litname=" ^ litname
    | E_start_tag (name, atts, scope, _) ->
	let s = match scope with Some sobj -> sobj # effective_declaration | None -> [] in
	"E_start_tag name=" ^ name ^ ",atts=(" ^
	(String.concat " " (List.map (fun (n,v) -> n ^ "=" ^ v) atts)) ^ 
	") scope=(" ^
	(String.concat " " (List.map (fun (n,v) -> n ^ "=" ^ v) s)) ^ ")"
    | E_end_tag (name, _) ->
	"E_end_tag name=" ^ name
    | E_char_data data ->
	"E_char_data data=\"" ^ String.escaped data ^ "\""
    | E_pinstr (target, value, _) ->
	"E_pinstr target=" ^ target ^ ",value=" ^ value
    | E_pinstr_member (target, value, _) ->
	"E_pinstr_member target=" ^ target ^ ",value=" ^ value
    | E_comment data ->
	"E_comment data=\"" ^ String.escaped data ^ "\""
    | E_start_super ->
	"E_start_super"
    | E_end_super ->
	"E_end_super"
    | E_position (e,l,c) ->
	"E_position e=" ^ e ^ ",l=" ^ string_of_int l ^ ",c=" ^
	string_of_int c
    | E_error e ->
	"E_error error=" ^ string_of_exn e
    | E_end_of_stream ->
	"E_end_of_stream"
;;


let sort_atts l =
  List.sort (fun (n1,v1) (n2,v2) -> Pervasives.compare n1 n2) l
;;


let sort_scope =
  function
      Some s -> sort_atts s#effective_declaration
    | None   -> []
;;


let rec compare_event_lists l1 l2 =
  match l1, l2 with
      (e1::l1'), (e2::l2') ->
	let ok =
	  match e1,e2 with
	      E_start_doc(version1,dtd1), 
	      E_start_doc(version2,dtd2) ->
		version1 = version2  (* DTDs not compared *)
	    | E_end_doc(litname1), 
	      E_end_doc(litname2) ->
		litname1 = litname2
	    | E_start_tag(name1,atts1,scope1,ent1), 
	      E_start_tag(name2,atts2,scope2,ent2) ->
		(name1 = name2) && 
		(sort_atts atts1 = sort_atts atts2) &&
		(sort_scope scope1 = sort_scope scope2) 
		(* Entity IDs not compared *)
	    | E_end_tag(name1,ent1),
	      E_end_tag(name2,ent2) ->
		name1 = name2
	    | E_char_data(data1),
	      E_char_data(data2) ->
		data1 = data2
	    | E_pinstr(target1,value1,_),
	      E_pinstr(target2,value2,_) ->
		target1 = target2 && value1 = value2
	    | E_pinstr_member(target1,value1,_),
	      E_pinstr_member(target2,value2,_) ->
		true
		(* Don't compare; E_pinstr_member do not have a defined
		 * order
		 *)
	    | E_comment(data1),
	      E_comment(data2) ->
		data1 = data2
	    | E_start_super,
	      E_start_super ->
		true
	    | E_end_super,
	      E_end_super ->
		true
	    | E_position(ent1,line1,col1),
	      E_position(ent2,line2,col2) ->
		ent1 = ent2 && line1 = line2 && col1 = col2
	    | E_end_of_stream,
	      E_end_of_stream ->
		true
	    | _ ->
		false in
	if ok then
	  compare_event_lists l1' l2'
	else (
	  failwith("First event=" ^ print_event e1 ^ "\nSecond event=" ^
		   print_event e2)
	)
    | [], [] ->
	()
    | [], _ ->
	failwith "Second list longer than first"
    | _, [] ->
	failwith "First list longer than second"
;;


let compare_event_lists l1 l2 =
  try
    compare_event_lists l1 l2
  with
      Failure msg ->
	failwith (msg ^ "\nl1=" ^ 
		  String.concat "; " (List.map print_event l1) ^ 
		  "\nl2=" ^
		  String.concat "; " (List.map print_event l2))
;;


(* Parsing modes:
 * - N: namespaces disabled/enabled
 * - S: super root node disabled/enabled
 * - P: PI nodes disabled/enabled
 * - C: Comment nodes disabled/enabled
 *
 * Test setups:
 * (001) liquefy(tree_parser(xml_tree)) = event_parser(xml_tree)
 * (002) same as (001), but w/o document
 * (003) liquefy(solidify(event_parser(xml_tree))) = event_parser(xml_tree)
 * (004) same as (003), but w/o document
 * (3) event_parser(display(tree_parser(xml_tree))) = event_parser(xml_tree)
 *)

let mode_name name_l val_l =
  String.concat
    ","
    (List.map2
       (fun n v ->
	  if v then "+" ^ n else "-" ^ n
       )
       name_l
       val_l)
;;
  

let iter_modes f =
  let brange = [false; true] in
  let biter f = List.iter f brange in
  biter 
    (fun n ->
       biter
         (fun s ->
	    biter
	      (fun p ->
		 biter
		   (fun c ->
		      let name = 
			mode_name [ "N"; "S"; "P"; "C" ] [ n; s; p; c ] in
		      f name (n,s,p,c)
		   )
	      )
	 )
    )
;;


let test_setup001 (mode_N,mode_S,mode_P,mode_C) =
  let mng = new namespace_manager in
  let config =
    { default_config with
	enable_super_root_node = mode_S;
	enable_pinstr_nodes = mode_P;
	enable_comment_nodes = mode_C;
	drop_ignorable_whitespace = false;
	enable_namespace_processing = if mode_N then Some mng else None;
    } in
  let spec = if mode_N then default_namespace_spec else default_spec in
  let t = parse_wfdocument_entity config (from_string xml_string) spec in
  let l1 = event_list(liquefy (`Document t)) in
  let entmng = create_entity_manager 
		 ~is_document:true config (from_string xml_string) in
  let entry = `Entry_document[`Parse_xml_decl] in
  let l2 = event_list(create_pull_parser config entry entmng) in
  compare_event_lists l1 l2;
  true
;;


let test001() =
  iter_modes
    (fun name mode ->
       dotest ("001(" ^ name ^ ")") test_setup001 mode)
;;


let test_setup002 (mode_N,mode_S,mode_P,mode_C) =
  let mng = new namespace_manager in
  let config =
    { default_config with
	enable_super_root_node = mode_S;
	enable_pinstr_nodes = mode_P;
	enable_comment_nodes = mode_C;
	drop_ignorable_whitespace = false;
	enable_namespace_processing = if mode_N then Some mng else None;
    } in
  let spec = if mode_N then default_namespace_spec else default_spec in
  let t = parse_wfcontent_entity config (from_string xml_string_no_decl) spec in
  let l1 = event_list(liquefy (`Node t)) in
  let entmng = create_entity_manager 
		 ~is_document:false config (from_string xml_string_no_decl) in
  let entry = `Entry_content[] in
  let l2 = event_list(create_pull_parser config entry entmng) in
  compare_event_lists l1 l2;
  true
;;


let test002() =
  iter_modes
    (fun name mode ->
       dotest ("002(" ^ name ^ ")") test_setup002 mode)
;;


let test_setup003 (mode_N,mode_S,mode_P,mode_C) =
  let mng = new namespace_manager in
  let config =
    { default_config with
	enable_super_root_node = mode_S;
	enable_pinstr_nodes = mode_P;
	enable_comment_nodes = mode_C;
	drop_ignorable_whitespace = false;
	enable_namespace_processing = if mode_N then Some mng else None;
    } in
  let spec = if mode_N then default_namespace_spec else default_spec in
  let entry = `Entry_document[`Parse_xml_decl] in
  let entmng1 = create_entity_manager 
		 ~is_document:true config (from_string xml_string) in
  let l1 = event_list
	     (liquefy 
		(solidify config spec 
		   (create_pull_parser config entry entmng1))) in
  let entmng2 = create_entity_manager 
		 ~is_document:true config (from_string xml_string) in
  let l2 = event_list(create_pull_parser config entry entmng2) in
  compare_event_lists l1 l2;
  true
;;


let test003() =
  iter_modes
    (fun name mode ->
       dotest ("003(" ^ name ^ ")") test_setup003 mode)
;;


let test_setup004 (mode_N,mode_S,mode_P,mode_C) =
  let mng = new namespace_manager in
  let config =
    { default_config with
	enable_super_root_node = mode_S;
	enable_pinstr_nodes = mode_P;
	enable_comment_nodes = mode_C;
	drop_ignorable_whitespace = false;
	enable_namespace_processing = if mode_N then Some mng else None;
    } in
  let spec = if mode_N then default_namespace_spec else default_spec in
  let entry = `Entry_content[] in
  let entmng1 = create_entity_manager 
		 ~is_document:true config (from_string xml_string_no_decl) in
  let l1 = event_list
	     (liquefy 
		(solidify config spec 
		   (create_pull_parser config entry entmng1))) in
  let entmng2 = create_entity_manager 
		 ~is_document:false config (from_string xml_string_no_decl) in
  let l2 = event_list(create_pull_parser config entry entmng2) in
  compare_event_lists l1 l2;
  true
;;


let test004() =
  iter_modes
    (fun name mode ->
       dotest ("004(" ^ name ^ ")") test_setup004 mode)
;;


let test_setup005 (mode_N,mode_S,mode_P,mode_C) =
  let mng = new namespace_manager in
  let config =
    { default_config with
	enable_super_root_node = mode_S;
	enable_pinstr_nodes = mode_P;
	enable_comment_nodes = mode_C;
	drop_ignorable_whitespace = false;
	enable_namespace_processing = if mode_N then Some mng else None;
    } in
  let spec = if mode_N then default_namespace_spec else default_spec in
  let t = parse_wfdocument_entity config (from_string xml_string) spec in
  let buf = Buffer.create 500 in
  t # root # display (`Out_buffer buf) `Enc_iso88591;
  let entry = `Entry_document[`Parse_xml_decl] in
  let entmng1 = create_entity_manager 
		  ~is_document:true config (from_string (Buffer.contents buf)) in
  let l1 = event_list ~keep_positions:false 
	     (create_pull_parser config entry entmng1) in
  let entmng2 = create_entity_manager 
		  ~is_document:true config (from_string xml_string) in
  let l2 = event_list ~keep_positions:false 
	     (create_pull_parser config entry entmng2) in
  compare_event_lists l1 l2;
  true
;;


let test005() =
  iter_modes
    (fun name mode ->
       dotest ("005(" ^ name ^ ")") test_setup005 mode)
;;


test001();;
test002();;
test003();;
test004();;
test005();;
