(* $Id: pxp_marshal.ml,v 1.1 2000/09/17 00:10:31 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_document
open Pxp_dtd

type reconstruction_cmd =
    Start_data_node of string
  | Start_element_node of ( (string * int * int) option *     (* position *)
                            string *                          (* eltype *)
                            (string * string) list)           (* attributes *)
  | Start_super_root_node of ( (string * int * int) option )  (* position *)
  | Start_comment_node of ( (string * int * int) option *     (* position *)
                            string option )                   (* comment text *)
  | Start_pinstr_node of ( (string * int * int) option *      (* position *)
			   string *                           (* target *)
			   string )                           (* PI text *)
  | Add_pinstr of (string * string)                           (* target, text *)
  | End_node
  | DTD_string of (string * string) (* Encoding, The declarations as XML text *)
  | Document of string                                         (* XML version *)
;;

(* A node is represented as sequence of reconstruction_cmd values in the
 * following way:
 *
 * - Start_xxx_node ...
 *   Begins the representation of the node with type xxx
 * - Add_pinstr ...
 *   If necessary, one or several processing instructions (not for pinstr nodes)
 * - Now the sub nodes in order: Start_xxx_node ... End_node
 * - End_node
 *
 * A DTD is represented as a single DTD_string command.
 *
 * A Document is represented by:
 * - Document ...
 * - DTD_string ...
 * - The root node: Start_xxx_node ... End_node
 *)


let cmd_string cmd =
  match cmd with
      Start_data_node _ -> "Start_data_node"
    | Start_element_node (_,eltype,_) -> "Start_element_node " ^ eltype
    | Start_super_root_node _ -> "Start_super_root_node"
    | Start_comment_node (_,_) -> "Start_comment_node"
    | Start_pinstr_node (_,target,_) -> "Start_pinstr_node " ^ target
    | Add_pinstr (target,_) -> "Add_pinstr " ^ target
    | End_node -> "End_node"
    | DTD_string _ -> "DTD_string"
    | Document _ -> "Document"
;;


let subtree_to_cmd_sequence ?(omit_positions = false) ~f n = 
  let rec do_subtree n = (
    match n # node_type with
	T_data ->
	  f (Start_data_node (n#data));
	  f End_node;
      | T_element eltype ->
	  let pos = get_position n in
	  let atts =
	    (List.map
	       (function
		    n,Value s -> n,s
		  | n,Valuelist sl ->
		      n,String.concat " " sl
		  | _,Implied_value ->
		      assert false)
	    )
	    (List.filter 
	       (fun (_,a) -> a <> Implied_value) 
	       (n # attributes)
	    )
	  in
	  f (Start_element_node (pos, eltype, atts));
	  do_pinstr n;
	  do_subnodes n;
	  f End_node;
      | T_super_root ->
	  f (Start_super_root_node (get_position n));
	  do_pinstr n;
	  do_subnodes n;
	  f End_node;
      | T_pinstr target ->
	  let pos = get_position n in
	  let l = n # pinstr target in
	  (match l with
	       [ pi ] ->
		 f (Start_pinstr_node (pos, target, pi # value));
		 f End_node;
	     | _ ->
		 assert false
	  )
      | T_comment ->
	  let pos = get_position n in
	  f (Start_comment_node (pos, n # comment));
	  f End_node;
      | _ ->
	  assert false
  )
  and do_pinstr n =
    let names = n # pinstr_names in
    List.iter
      (fun name ->
	 let pinstrs = n # pinstr name in
	 List.iter
	   (fun pi ->
	      f (Add_pinstr (pi # target, pi # value));
	   )
	   pinstrs
      )
      names
  and do_subnodes n =
    n # iter_nodes do_subtree
  and get_position n =
    if omit_positions then
      None
    else
      let entity, line, column = n # position in
      if line = 0 then None else Some (entity,line,column)
  in
  do_subtree n
;;


let subtree_to_channel ?(omit_positions = false) ch n =
  subtree_to_cmd_sequence
    ~omit_positions:omit_positions
    (fun cmd -> 
       Marshal.to_channel ch cmd [ Marshal.No_sharing ]
    )
    n
;;


let subtree_from_cmd_sequence ~f dtd spec =
  let rec read_node first_cmd =
    let n =
      match first_cmd with
	  Start_data_node data ->
	    create_data_node spec dtd data
	| Start_element_node (pos, eltype, atts) ->
	    create_element_node
	      ?position:pos
	      spec
	      dtd
	      eltype
	      atts
	| Start_super_root_node pos ->
	    create_super_root_node ?position:pos spec dtd
	| Start_comment_node (pos, comment) ->
	    (match comment with
		 Some c ->
		   create_comment_node
		   ?position:pos
		     spec
		     dtd
		     c
	       | None ->
		   let cn = create_comment_node
			      ?position:pos
			      spec
			      dtd
			      "" in
		   cn # set_comment None;
		   cn
	    )
	| Start_pinstr_node (pos, target, value) ->
	    create_pinstr_node
	      ?position:pos
	      spec
	      dtd
	      (new proc_instruction target value (dtd # encoding))
	| _ ->
	    assert false
    in
    let rec add () =
      match f () with
	  (Start_data_node _ |
	   Start_element_node (_,_,_) |
	   Start_super_root_node _ |
	   Start_comment_node (_,_) |
	   Start_pinstr_node (_,_,_)) as cmd ->
	    (* Add a new sub node *)
	    let n' = read_node cmd in
	    n # add_node ~force:true n';
	    add()
	| Add_pinstr(target,value) ->
	    let pi = new proc_instruction target value (dtd # encoding) in
	    n # add_pinstr pi;
	    add()
	| End_node ->
	    ()
	| _ ->
	    failwith "Pxp_marshal.subtree_from_cmd_sequence"
    in
    add();
    n
  in
  read_node (f())
;;


let subtree_from_channel ch dtd spec =
  try
    subtree_from_cmd_sequence
      (fun () -> 
	 Marshal.from_channel ch)
      dtd 
      spec
  with
      End_of_file ->
	failwith "Pxp_marshal.subtree_from_channel"
;;


let document_to_cmd_sequence ?(omit_positions = false) ~f 
       (doc : 'ext Pxp_document.document) =
  f (Document (doc # xml_version));
  let dtd_buffer = Buffer.create 1000 in
  doc # dtd # write (Out_buffer dtd_buffer) (doc # encoding :> encoding) false; 
  f (DTD_string (Netconversion.string_of_encoding (doc # encoding :> encoding),
		 Buffer.contents dtd_buffer));
  subtree_to_cmd_sequence 
    ~omit_positions:omit_positions
    f
    (doc # root)
;;


let document_to_channel ?(omit_positions = false) ch doc =
  document_to_cmd_sequence
    ~omit_positions:omit_positions
    (fun cmd -> 
       Marshal.to_channel ch cmd [ Marshal.No_sharing ]
    )
    doc
;;


let document_from_cmd_sequence ~f config spec =
  let cmd1 = f() in
  let xml_version =
    match cmd1 with
	Document v -> v
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let cmd2 = f() in
  let enc, dtd_string =
    match cmd2 with
	DTD_string(e,s) -> e,s
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let dtd =
    Pxp_yacc.parse_dtd_entity 
      config
      (Pxp_yacc.from_string 
         ~fixenc:(Netconversion.encoding_of_string enc)
	 dtd_string) in
  let doc = new Pxp_document.document config.Pxp_yacc.warner in
  doc # init_xml_version xml_version;
  let root = subtree_from_cmd_sequence f dtd spec in
  doc # init_root root;
  doc
;;

let document_from_channel ch config spec =
  try
    document_from_cmd_sequence
      (fun () -> 
	 Marshal.from_channel ch)
      config
      spec
  with
      End_of_file ->
	failwith "Pxp_marshal.document_from_channel"
;;
	  

(* ======================================================================
 * History:
 * 
 * $Log: pxp_marshal.ml,v $
 * Revision 1.1  2000/09/17 00:10:31  gerd
 * 	Initial revision.
 *
 * 
 *)
