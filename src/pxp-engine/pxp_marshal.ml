(* $Id: pxp_marshal.ml,v 1.5 2001/06/27 23:33:53 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_document
open Pxp_dtd
open Pxp_aux
open Pxp_yacc

type reconstruction_cmd =
    Head of (string * bool)                         (* Encoding, standalone? *)
  | Start_data_node of string
  | Declare_element_exemplar of (int * string)             (* number, eltype *)
  | Declare_attribute of (int * string)                    (* number, name *)
  | Start_element_node of ( (string * int * int) option *  (* position *)
                            int *                          (* exemplar number *)
                            (int * att_value) list)        (* attributes *)
  | Start_super_root_node of ( (string * int * int) option )  (* position *)
  | Start_comment_node of ( (string * int * int) option *     (* position *)
                            string option )                   (* comment text *)
  | Start_pinstr_node of ( (string * int * int) option *      (* position *)
			   string *                           (* target *)
			   string )                           (* PI text *)
  | Add_pinstr of (string * string)                           (* target, text *)
  | End_node
  | DTD_string of (string *                                  (* root element *) 
		   dtd_id *
                   string)                    (* The declarations as XML text *)
  | Document of string                                         (* XML version *)
  | Root                                                         (* delimiter *)
  | Cmd_array of reconstruction_cmd array
  | Namespace_mapping of (string * string array)
    (* normprefix, URIs (the last is the primary URI) *)
;;

(* Note: Marshalling is new in PXP 1.1 (did not exist in 1.0). The type
 * reconstruction_cmd changed several times during the development of
 * this module.
 *)

(* A node is represented as sequence of reconstruction_cmd values in the
 * following way:
 *
 * - Head
 * - Start_xxx_node ...
 *   Begins the representation of the node with type xxx
 * - Add_pinstr ...
 *   If necessary, one or several processing instructions (not for pinstr nodes)
 * - Now the sub nodes in order: Start_xxx_node ... End_node
 * - End_node
 *
 * If namespaces are used, the namespace_manager is prepended to the first
 * (outermost) node of the tree. It is represented as a sequence of
 * Namespace_mapping commands:
 *
 * - Namespace_mapping ...
 * - Namespace_mapping ...
 * - ...
 *
 * The namespace_info objects are (currently) not marshalled.
 *
 * A DTD is represented as a single DTD_string command.
 *
 * A Document is represented by:
 * - Head
 * - Document ...
 * - DTD_string ...
 * - Add_pinstr ...
 * - Root
 * - The root node: Start_xxx_node ... End_node
 *)


let subtree_to_cmd_sequence_nohead ?(omit_positions = false) ~f:f0 n = 
  let m = 100 in
  let current_array = Array.create m End_node in
  let current_pos = ref 0 in
  let f cmd =
    if !current_pos < Array.length current_array then begin
      current_array.( !current_pos ) <- cmd;
      incr current_pos
    end
    else begin
      f0 (Cmd_array(Array.copy current_array));
      current_array.( 0 ) <- cmd;
      current_pos := 1;
    end
  in
  let finish() =
    f0 (Cmd_array(Array.sub current_array 0 !current_pos))
  in
  let next_ex_number = ref 0 in
  let ex_hash = Hashtbl.create 100 in
  let next_att_number = ref 0 in
  let att_hash = Hashtbl.create 100 in
  let rec do_subtree n = (
    match n # node_type with
	T_data ->
	  f (Start_data_node (n#data));
	  f End_node;
      | T_element eltype ->
	  let pos = get_position n in
	  let atts =
	    (* remove all Implied_value; use att_hash *)
	    List.map
	      (fun (name,a) ->
		 try (Hashtbl.find att_hash name, a)
		 with
		     Not_found ->
		       let nr = !next_att_number in
		       incr next_att_number;
		       Hashtbl.add att_hash name nr;
		       f (Declare_attribute(nr, name));
		       (nr, a)
	      )
	      (List.filter 
		 (fun (_,a) -> a <> Implied_value) 
		 (n # attributes)
	      )
	  in
	  let ex_nr =
	    try
	      Hashtbl.find ex_hash eltype 
	    with
		Not_found ->
		  let nr = !next_ex_number in
		  incr next_ex_number;
		  Hashtbl.add ex_hash eltype nr;
		  f (Declare_element_exemplar(nr,eltype));
		  nr
	  in
	  f (Start_element_node (pos, ex_nr, atts));
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
  let emit_namespace_mappings () =
    (* TODO: In general, too many mappings are emitted, even mappings 
     * which are unused in the processed subtree.
     * It would also be possible to emit a mapping just before it is
     * used. 
     *)
    try
      let mng = n # namespace_manager in
      mng # iter_namespaces
	(fun normprefix ->
	   let uris = Array.of_list (mng # get_uri_list normprefix) in
	   f (Namespace_mapping (normprefix, uris))
	);
      ()
    with
	Namespace_method_not_applicable _ ->
	  ()
  in
  emit_namespace_mappings();
  do_subtree n;
  finish()
;;


let subtree_to_cmd_sequence ?omit_positions f n = 
  let enc = Netconversion.string_of_encoding (n#encoding :> encoding) in
  let sa = n#dtd#standalone_declaration in
  f(Head(enc,sa));
  subtree_to_cmd_sequence_nohead ?omit_positions f n
;;


let subtree_to_channel ?(omit_positions = false) ch n =
  subtree_to_cmd_sequence
    ~omit_positions:omit_positions
    (fun cmd -> 
       Marshal.to_channel ch cmd [ Marshal.No_sharing ]
    )
    n
;;


let subtree_from_cmd_sequence_nohead ~f:f0 dtd spec =
  let current_array = ref( [| |] ) in
  let current_pos = ref 0 in
  let rec f() =
    if !current_pos < Array.length !current_array then begin
      incr current_pos;
      !current_array.( !current_pos - 1)
    end
    else begin
      let c = f0() in
      match c with
	  Cmd_array a ->
	    current_array := a;
	    current_pos := 0;
	    f()
	| _ ->
	    c
    end
  in
  let default = get_data_exemplar spec in
  let eltypes = ref (Array.create 100 ("",default)) in
  let atts = ref (Array.create 100 "") in
  let mng = new namespace_manager in
  let mng_found = ref false in
  let enable_mng, dest_mng = 
    try 
      true, dtd # namespace_manager
    with
	Namespace_method_not_applicable _ -> 
	  false, mng (* value does not matter *)
  in
  let map_nsprefix name =
    let p, l = namespace_split name in
    if p = "" then
      name
    else begin
      (* p is mapped to the primary URI of mng, and then mapped
       * to the normprefix of dest_mng
       *)
      try
	let primary_uri = mng # get_primary_uri p in
	let normprefix = 
	  dest_mng # lookup_or_add_namespace p primary_uri in
	normprefix ^ ":" ^ l
      with
	  Not_found ->
	    failwith "Pxp_marshal.subtree_from_cmd_sequence: Found an undeclared namespace prefix"
    end
  in
  let rec read_node dont_add first_cmd =
    let n =
      match first_cmd with
	  Start_data_node data ->
	    create_data_node spec dtd data
	| Declare_element_exemplar (nr, eltype) ->
	    if nr > Array.length !eltypes then begin
	      eltypes := 
	        Array.append !eltypes (Array.create 100 ("",default));
	    end;
	    let eltype' =
	      if !mng_found then
		(* Map the namespace prefix to a working normprefix *)
		map_nsprefix eltype
	      else
		eltype
	    in
	    !eltypes.(nr) <- (eltype',
			      get_element_exemplar spec eltype []);
	    read_node true (f())
	| Declare_attribute (nr, name) ->
	    if nr > Array.length !atts then begin
	      atts := 
	        Array.append !atts (Array.create 100 "");
	    end;
	    let name' =
	      if !mng_found then
		map_nsprefix name
	      else
		name
	    in
	    !atts.(nr) <- name';
	    read_node true (f())
	| Start_element_node (pos, nr, a) ->
	    let eltype, ex = !eltypes.(nr) in
(* -- saves 4% time, but questionable approach:
	    ex # create_element 
	      ?position:pos
	      dtd
	      (T_element eltype)
	      atts
*)
	    let a' =
	      List.map (fun (nr, v) -> !atts.(nr), v) a in
	    let e =
	      create_element_node
	        ~att_values: a'
	        ?position:pos
		spec
		dtd
		eltype
		[] in
	    e
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
	| Namespace_mapping (normprefix, uris) ->
	    if enable_mng then begin
	      let primary_uri = uris.( Array.length uris - 1 ) in
	      if normprefix <> "xml" then
		mng # add_namespace normprefix primary_uri;
	      for i=0 to Array.length uris - 2 do
		mng # add_uri normprefix uris.(i)
	      done;
	      mng_found := true;
	    end;
	    read_node true (f())
	| _ ->
	    assert false
    in
    let rec add () =
      match f () with
	  (Start_data_node _ |
	   Start_element_node (_,_,_) |
	   Start_super_root_node _ |
	   Start_comment_node (_,_) |
	   Start_pinstr_node (_,_,_) |
	   (* Declare_xxx is always followed by Start_element_node: *)
	   Declare_element_exemplar(_,_) |
	   Declare_attribute(_,_)) as cmd ->
	    (* Add a new sub node *)
	    let n' = read_node false cmd in
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
    if not dont_add then
      add();
    n
  in
  read_node false (f())
;;


let subtree_from_cmd_sequence ~f dtd spec =
  match f() with
      Head(enc_s,_) ->
	let enc = Netconversion.encoding_of_string enc_s in
	let rep_enc = 
	  match enc with
	      (#rep_encoding as x) -> x
	    | _ -> failwith "Pxp_marshal.subtree_from_cmd_sequence"
	in
	if dtd # encoding <> rep_enc then
	  failwith "Pxp_marshal.subtree_from_cmd_sequence";

	subtree_from_cmd_sequence_nohead ~f dtd spec

    | _ ->
	failwith "Pxp_marshal.subtree_from_cmd_sequence"
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
  let enc = Netconversion.string_of_encoding (doc # encoding :> encoding) in
  let sa = doc # dtd # standalone_declaration in
  f (Head (enc, sa));
  f (Document (doc # xml_version));
  let dtd_buffer = Buffer.create 1000 in
  doc # dtd # write (`Out_buffer dtd_buffer) (doc # encoding :> encoding) false; 
  let r = 
    match doc # dtd # root with
	None -> ""
      | Some x -> x
  in
  let id =
    match doc # dtd # id with
	None -> Internal
      | Some x -> x
  in
  f (DTD_string (r,
		 id,
		 Buffer.contents dtd_buffer));
  List.iter
    (fun pi_name ->
       List.iter
	 (fun pi ->
	    f (Add_pinstr (pi # target, pi # value))
	 )
	 (doc # pinstr pi_name)
    )
    (doc # pinstr_names);
  f Root;
  subtree_to_cmd_sequence_nohead
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


let document_from_cmd_sequence f config spec =
  let cmd0 = f() in
  let enc_s, sa =
    match cmd0 with
	Head(e,s) -> e,s
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let enc = Netconversion.encoding_of_string enc_s in
  let rep_enc =
    match enc with
        (#rep_encoding as x) -> x
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  if rep_enc <> config.encoding then
    failwith "Pxp_marshal.document_from_cmd_sequence";
  let cmd1 = f() in
  let xml_version =
    match cmd1 with
	Document v -> v
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let cmd2 = f() in
  let root_type, id, dtd_string =
    match cmd2 with
	DTD_string(r,i,s) -> r,i,s
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let dtd =
    Pxp_yacc.parse_dtd_entity 
      config
      (Pxp_yacc.from_string 
         ~fixenc:enc
	 dtd_string) in
  if root_type <> "" then dtd # set_root root_type;
  dtd # set_id id;
  dtd # set_standalone_declaration sa;
  let doc = new Pxp_document.document config.Pxp_yacc.warner in
  doc # init_xml_version xml_version;
  let cmd = ref (f()) in
  while !cmd <> Root do
    ( match !cmd with
	  Add_pinstr(target,value) ->
	    let pi = new proc_instruction target value rep_enc in
	    doc # add_pinstr pi
	| _ ->
	    failwith "Pxp_marshal.document_from_cmd_sequence"
    );
    cmd := f();
  done;
  let root = 
    subtree_from_cmd_sequence_nohead
      f dtd spec in
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
 * Revision 1.5  2001/06/27 23:33:53  gerd
 * 	Type output_stream is now a polymorphic variant
 *
 * Revision 1.4  2001/06/08 01:15:47  gerd
 * 	Moved namespace_manager from Pxp_document to Pxp_dtd. This
 * makes it possible that the DTD can recognize the processing instructions
 * <?pxp:dtd namespace prefix="..." uri="..."?>, and add the namespace
 * declaration to the manager.
 *
 * Revision 1.3  2001/06/07 22:46:15  gerd
 * 	Revised set of reconstruction commands:
 *  - Head: Contains the encoding
 *  - Declare_attributes: there is a name pool for attributes now
 *  - Start_element_node: uses att_value to represent attributes
 *  - DTD_string: contains the name of the root element, the DTD ID
 *  - Root: a delimiter before the root element begins
 *  - Namespace_mapping: represents the namespace manager
 * 	As the Head command contains the encoding, it is checked whether
 * the marshalling functions are called with the same encoding. Otherwise
 * the functions fail.
 * 	Fixed: The name of the root element, and the DTD ID are represented.
 * 	Namespaces are supported.
 * 	QA: This revision passes some regression tests (codewriter dir)
 *
 * Revision 1.2  2000/09/21 21:30:23  gerd
 * 	Optimization: repeated elements are created from
 * shared exemplars.
 *
 * Revision 1.1  2000/09/17 00:10:31  gerd
 * 	Initial revision.
 *
 * 
 *)
