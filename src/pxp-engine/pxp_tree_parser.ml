(* $Id: pxp_tree_parser.ml,v 1.4 2003/06/20 21:00:33 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_lexers
open Pxp_lexer_types
open Pxp_entity_manager
open Pxp_dtd
open Pxp_document
open Pxp_core_parser
open Pxp_aux


exception ID_not_unique

class type [ 'ext ] index =
object
  constraint 'ext = 'ext node #extension
  method add : string -> 'ext node -> unit
  method find : string -> 'ext node
end


class  [ 'ext ] hash_index =
object
  constraint 'ext = 'ext node #extension
  val ht = (Hashtbl.create 100 : (string, 'ext node) Hashtbl.t)
  method add s n =
    try
      ignore(Hashtbl.find ht s);
      raise ID_not_unique
    with
	Not_found ->
	  Hashtbl.add ht s n

  method find s = Hashtbl.find ht s
  method index = ht
end


class default_ext =
  object(self : 'self)
    method clone = self
    method node =
      (assert false : 'self node)
    method set_node (n : 'self node) =
      ()
  end
;;




let default_extension = new default_ext;;

let default_spec =
  make_spec_from_mapping
    ~super_root_exemplar:      (new super_root_impl default_extension)
    ~comment_exemplar:         (new comment_impl default_extension)
    ~default_pinstr_exemplar:  (new pinstr_impl default_extension)
    ~data_exemplar:            (new data_impl default_extension)
    ~default_element_exemplar: (new element_impl default_extension)
    ~element_mapping:          (Hashtbl.create 1)
    ()
;;


let default_namespace_spec =
  make_spec_from_mapping
    ~super_root_exemplar:      (new super_root_impl default_extension)
    ~comment_exemplar:         (new comment_impl default_extension)
    ~default_pinstr_exemplar:  (new pinstr_impl default_extension)
    ~data_exemplar:            (new data_impl default_extension)
    ~default_element_exemplar: (new namespace_element_impl default_extension)
    ~element_mapping:          (Hashtbl.create 1)
    ()
;;


(**********************************************************************)

class ['ext] tree_parser
  (init_doc:'ext document) init_dtd init_config init_spec
  transform_dtd id_index 
  =
  let make_pool_string = pool_string init_config.name_pool in
object (self)
  inherit core_parser init_dtd init_config (-1)

  val transform_dtd = transform_dtd
      (* A function transforming the DTD *)

  val id_index = (id_index : 'ext index option)
      (* The ID index or None *)

  val doc = init_doc
      (* The current document *)

  method doc = (doc : 'ext document)

  val elstack =
    let null_node = get_data_exemplar init_spec in
    let null_id = (null_node :> entity_id) in
    let null = (null_node, "", null_id) in
    (stack_create null : ('ext node * string * entity_id) array_stack)
       (* The element stack containing all open elements, i.e. elements that
	* have begun by a start tag but that have not been finished (end tag).
	* If the parser sees a start tag, it creates the element and pushes it
	* on top of this stack. If the parser recognizes an end tag, it pulls
	* one element from the stack and checks if it has the same name as
	* given with the end tag.
	*
	* At initialization time, a special element is pushed on the stack,
	* the so-called super root. It is always the bottommost
	* element of the stack, and serves as a guard.
	*)
    (* --- Initialize 'elstack': Push the super-root on the stack. *)
    (* (This is now done later, in the contents_start rule) *)

  method private current =
    (* Get the top element of the element stack *)
    try
      let (x,_,_) = stack_top elstack in x
    with
	Stack.Empty -> assert false
	  (* Not possible, because the super root is always the element
	   * at the bottom of the stack.
	   *)
	  
  val mutable root = None
    (* Contains the root element (topmost element) while it is being parsed
     * and after it has been parsed.
     * This variable is None before the root element is seen.
     *)

  method root = root

  val spec = init_spec
    (* A hashtable that contains exemplar objects for the various element
     * types. If an element is parsed, the exemplar is looked up and
     * "cloned" (by the "create" method)
     *)

  val mutable current_data = []
  val mutable current_string = ""
    (* Collect character data. *)

  method private save_data =
    (* Puts the material collected in 'current_data' into a new
     * node, and appends this node as new sub node to 'current'
     *)
    let add_node d =
      let cur = self # current in
      match cur # classify_data_node d with
	  CD_normal
	| CD_other ->
	    cur # append_node d
	| CD_empty ->
	    ()
	| CD_ignorable ->
	    if not config.drop_ignorable_whitespace then
	      cur # append_node d
	| CD_error e ->
	    raise e
    in
    match current_data with
	[] ->
	  if String.length current_string > 0 then
	    add_node (create_data_node spec dtd current_string);
	  current_string <- "";
      | [ str ] ->
	  (* assertion: current_string <> "" *)
	  let s = if str = "" then current_string else current_string ^ str in
	  add_node (create_data_node spec dtd s);
	  current_string <- "";
	  current_data <- []
      | _ ->
	  let accu = ref (String.length current_string) in
	  List.iter (fun s -> accu := !accu + String.length s) current_data;
	  let str = String.create !accu in
	  let pos = ref (!accu) in
	  List.iter
	    (fun s ->
	       let l = String.length s in
	       pos := !pos - l;
	       String.blit s 0 str !pos l
	    )
	    current_data;
	  String.blit current_string 0 str 0 (String.length current_string);
	  add_node (create_data_node spec dtd str);
	  current_string <- "";
	  current_data <- []

  val mutable init_done = false       (* element stack initialized? *)

  val mutable early_material = []     (* saved material before init_done *)

  (* Call the following methods for comments and processing instructions
   * that occur before the element stack is initialized
   *)
				 
  method private add_early_comment position c =
    assert(not init_done);
    early_material <- early_material @ [ position, `Comment c ]

  method private add_early_pinstr position pi =
    assert(not init_done);
    early_material <- early_material @ [ position, `PI pi ]

  method private add_early_pinstr_node position pi =
    assert(not init_done);
    early_material <- early_material @ [ position, `PI_node pi ]


  method private init_for_xml_body() =
    if not init_done then begin
      dtd <- transform_dtd dtd;
      
      (* Initialize the element stack: *)
      let super_root =
	if config.enable_super_root_node then begin
	  let sr = create_super_root_node spec dtd in
	  (* Add early_material to the super root node: *)
	  List.iter
	    (function
		 (p, `Comment c) ->
		   let node = create_comment_node ?position:p spec dtd c in
		   sr # append_node node
	       | (p, `PI pi) ->
		   sr # add_pinstr pi
	       | (p, `PI_node pi) ->
		   let node = create_pinstr_node ?position:p spec dtd pi in
		   sr # append_node node
	    )
	    early_material;
	  sr
	end
	else
	  (* because spec may not contain an exemplar for the super root: *)
	  create_no_node spec dtd
      in
      early_material <- [];
      (* Move the super root or the emulation to the stack: *)
      stack_push (super_root, "", (self :> entity_id)) elstack;
      init_done <- true;
    end

  (********************************* EVENTS *****************************)

  method private event_document_xmldecl xmldecl =
    let v, _, s = decode_doc_xml_pi (decode_xml_pi xmldecl) in
    check_version_num v;
    doc # init_xml_version v;
    let v = match s with
	None -> false
      | Some "yes" -> true
      | Some "no" -> false
      | _ -> raise (WF_error("Illegal 'standalone' declaration"))
    in
    if config.recognize_standalone_declaration then
      dtd # set_standalone_declaration v


  method private event_start_tag position name attlist emptiness tag_beg_entid =
    (* position: The position of the start tag
     * name: The name of the tag
     * attlist: The attribute list
     * emptiness: Whether this is an empty tag or not
     * tag_beg_entid: The entity_id of the start tag
     *)
    let d =
      (* The following "match" returns the new element node: *)
      match config.enable_namespace_processing with
	  None ->
	    (* Simple case: no namespaces *)
	    create_element_node
              ?name_pool_for_attribute_values:
	      (if config.enable_name_pool_for_attribute_values
	       then Some config.name_pool
	       else None)
              ?position:position
	      spec dtd name attlist

	| Some mng ->
	    (* If namespace processing is enabled, preprocess the attribute
	     * list:
	     *)
	    let (src_prefix, localname, norm_name0, norm_attlist0) =
	      self # push_src_norm_mapping mng name attlist in

	    let norm_name =
	      if config.enable_name_pool_for_element_types
	      then make_pool_string norm_name0
	      else norm_name0 in
	    
	    let norm_attlist =
	      List.map (fun (orig_prefix, localname, norm_name, value) ->
			  (norm_name, value)
		       ) norm_attlist0 in

	    let element =
	      create_element_node
                ?name_pool_for_attribute_values:
		(if config.enable_name_pool_for_attribute_values
		 then Some config.name_pool
		 else None)
                ?position:position
		spec dtd norm_name norm_attlist
	    in

	    if config.enable_namespace_info then begin
	      let info =
		new namespace_info_impl
		      src_prefix
		      element
		      ( ("!", default_normprefix) :: src_norm_mapping) in
	      element # set_namespace_info (Some info);
	    end;

	    element

    (* end of match *)
    in

    begin match id_index with
	None -> ()
      | Some idx ->
	  (* Put the ID attribute into the index, if present *)
	  begin try
	    let v = d # id_attribute_value in  (* may raise Not_found *)
	    idx # add v d                      (* may raise ID_not_unique *)
	  with
	      Not_found ->
		(* No ID attribute *)
		()
	    | ID_not_unique ->
		(* There is already an ID with the same value *)
		raise(Validation_error("ID not unique"))
	  end
    end;

    if n_tags_open = 0 then begin
      if root = None then begin
	(* We have found the begin tag of the root element. *)
	if config.enable_super_root_node then begin
	  (* The user wants the super root instead of the real root.
	   * The real root element becomes the child of the VR.
	   *)
	  (* Assertion: self # current is the super root *)
	  assert (self # current # node_type = T_super_root);
	  root <- Some (self # current);
	  self # current # append_node d;
	  doc # init_root (self # current) name;
	end
	else begin
	  (* Normal behaviour: The user wants to get the real root. *)
	  root <- Some d;
	  doc # init_root d name;
	end;
      end
      else
	(* We have found a second topmost element. This is illegal. *)
	raise(WF_error("Document must consist of only one toplevel element"))
    end
    else begin
      (* We have found some inner begin tag. *)
      self # save_data;        (* Save outstanding data material first *)
      self # current # append_node d
    end;

    if emptiness then begin
      (* An empty tag like <a/>. *)
      if not config.disable_content_validation then
	d # validate_contents ~use_dfa:config.validate_by_dfa ~check_data_nodes:false ();
      if config.enable_namespace_processing <> None then
	self # pop_src_norm_mapping()
    end
    else begin
      (* A non-empty tag. *)
      stack_push (d, name, tag_beg_entid) elstack;
    end;


  method private event_end_tag name tag_end_entid =
    (* name: The name of the end tag 
     * tag_end_entid: The entity_id of the end tag
     *)

    self # save_data;        (* Save outstanding data material first *)
      
    let x, x_name, tag_beg_entid = stack_pop elstack in
    if config.enable_namespace_processing <> None then
      self # pop_src_norm_mapping();
    if name <> x_name then begin
      let x_entname, x_line, x_col = x # position in
      raise(WF_error("End tag `" ^ name ^
		     "' does not match start tag `" ^ x_name ^ "'" ^
		     (if x_line = 0 then "" else
			" (was at line " ^ string_of_int x_line ^
			", position " ^ string_of_int x_col ^ ")" )));
    end;
    if tag_beg_entid != tag_end_entid then
      raise(WF_error("End tag `" ^ name ^
		     "' not in the same entity as the start tag `" ^
		     x_name ^ "'"));
    if not config.disable_content_validation then
      x # validate_contents ~use_dfa:config.validate_by_dfa ~check_data_nodes:false ();
    

  method private event_char_data data =           (* formerly: collect_data *)
    (* data: The parsed character data *)
    (* We collect the chardata material until the next end tag is
     * reached. Then the collected material will concatenated and
     * stored as a single T_data node (see method event_end_tag above)
     * using save_data.
     *)
    if String.length current_string = 0 then
      current_string <- data
    else
      current_data <- data :: current_data

  method private event_pinstr position target value =
    (* position: The position of the processing instruction
     * target: The name following <?
     * value: The string following the name
     *)
    let pinstr = new proc_instruction target value config.encoding in
    if n_tags_open = 0 && not config.enable_super_root_node
    then
      doc # add_pinstr pinstr
    else begin
      (* Special case: if processing instructions are processed inline,
       * they are wrapped into T_pinstr nodes.
       *)
      if config.enable_pinstr_nodes then begin
	if init_done then begin
	  self # save_data;        (* Save outstanding data material first *)
	  let wrapper = create_pinstr_node
			  ?position:position spec dtd pinstr in
	  self # current # append_node wrapper;
	end
	else self # add_early_pinstr_node position pinstr
      end
      else
	(* Normal behaviour: Add the PI to the parent element. *)
	if init_done then
	  self # current # add_pinstr pinstr
	else
	  self # add_early_pinstr position pinstr
    end


  method private event_comment position mat =
    (* position: where the comment occurs
     * mat: List of Comment_material tokens 
     *)
    if config.enable_comment_nodes then begin
      let comment_text = String.concat "" mat in
      if init_done then begin
	self # save_data;        (* Save outstanding data material first *)
	let wrapper = create_comment_node
		        ?position:position spec dtd comment_text in
	self # current # append_node wrapper;
      end
      else
	self # add_early_comment position comment_text
    end


  method private sub_parser () =
    let pobj =
      new tree_parser
	(new document ?swarner:config.swarner config.warner config.encoding)
	dtd
	config
	spec
	(fun x -> x)
	None
    in
    (pobj :> core_parser)


end

(**********************************************************************)

let idref_pass id_index root =
  let error t att value =
    let name =
      match t # node_type with
	  T_element name -> name
	| _ -> assert false
    in
    let text =
      "Attribute `" ^ att ^ "' of element `" ^ name ^
      "' refers to unknown ID `" ^ value ^ "'" in
    let pos_ent, pos_line, pos_col = t # position in
    if pos_line = 0 then
      raise(Validation_error text)
    else
      raise(At("In entity " ^ pos_ent ^ " at line " ^
	       string_of_int pos_line ^ ", position " ^ string_of_int pos_col ^
	       ":\n",
	       Validation_error text))
  in

  let rec check_tree t =
    let idref_atts = t # idref_attribute_names in
    List.iter
      (fun att ->
	 match t # attribute att with
	     Value s ->
	       begin try ignore(id_index # find s) with
		   Not_found ->
		     error t att s
	       end
	   | Valuelist l ->
	       List.iter
		 (fun s ->
		    try ignore(id_index # find s) with
			Not_found ->
			  error t att s
		 )
		 l
	   | Implied_value -> ()
      )
      idref_atts;
    List.iter check_tree (t # sub_nodes)
  in
  check_tree root
;;


exception Return_DTD of dtd;;
  (* Used by extract_dtd_from_document_entity to jump out of the parser *)


let call_tree_parser ~configuration:cfg
                ~source:src
		~dtd
		~document:doc
		~specification:spec
		~transform_dtd
                ~(id_index : 'ext #index option)
		~use_document_entity
                ~entry
		~init_lexer =
  let r, en =
    open_source cfg src use_document_entity dtd in
  let pobj =
    new tree_parser
      doc
      dtd
      cfg
      spec
      transform_dtd
      (id_index :> 'ext index option)
  in
  let mgr = new entity_manager en dtd in
  let gen_att_events = cfg.escape_attributes <> None in
  en # open_entity ~gen_att_events true init_lexer;
  begin try
    let context = make_context mgr in
    pobj # parse context entry;
    if en # is_open then ignore(en # close_entity);
  with
      Return_DTD d ->
	ignore(en # close_entity);
	raise(Return_DTD d)
    | Failure "Invalid UTF-8 stream" ->
	(* raised by the wlex-generated lexers only: map to Malformed_code *)
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	raise (At(pos, Netconversion.Malformed_code))
    | error ->
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	raise (At(pos, error))
  end;
  if cfg.idref_pass then begin
    match id_index with
	None -> ()
      | Some idx ->
	  ( match pobj # root with
		None -> ()
	      | Some root ->
		  idref_pass idx root;
	  )
  end;
  pobj



(* === Old definition, no longer used

let parse_dtd_entity cfg src =
  (* Parse a DTD given as separate entity. *)
  let dtd = new dtd cfg.warner cfg.encoding in
  ( match cfg.enable_namespace_processing with
	Some mng -> dtd # set_namespace_manager mng
      | None     -> ()
  );
  let doc = new document cfg.warner cfg.encoding in
  let pobj =
    call_tree_parser
      ~configuration:cfg
      ~source:src
      ~dtd:dtd
      ~document:doc
      ~specification:default_spec
      ~transform_dtd:(fun x -> x)  (* Do not transform the DTD *)
      ~id_index: None
      ~use_document_entity:false
      ~entry:(`Entry_declarations [`Extend_dtd_fully])
                                   (* Entry point of the grammar *)
      ~init_lexer:Declaration      (* The initially used lexer *)
  in
  dtd # validate;
  if cfg.accept_only_deterministic_models then dtd # only_deterministic_models;
  dtd
;;

=== *)



let parse_content_entity ?id_index cfg src dtd spec =
  (* Parse an element given as separate entity *)
  dtd # validate;            (* ensure that the DTD is valid *)
  if cfg.accept_only_deterministic_models then dtd # only_deterministic_models;
  let doc = new document ?swarner:cfg.swarner cfg.warner cfg.encoding in
  let pobj =
    call_tree_parser
      ~configuration:cfg
      ~source:src
      ~dtd:dtd
      ~document:doc
      ~specification:spec
      ~transform_dtd:(fun x -> x)  (* Do not transform the DTD *)
      ~id_index:(id_index :> 'ext index option)
      ~use_document_entity:false
      ~entry:(`Entry_content [])   (* Entry point of the grammar *)
      ~init_lexer:Content          (* The initially used lexer *)
  in
  match pobj # root with
      Some r -> r
    | None -> raise(WF_error("No root element"))
;;


let parse_wfcontent_entity cfg src spec =
  let dtd = new dtd ?swarner:cfg.swarner cfg.warner cfg.encoding in
  (* Instead of dtd # allow_arbitrary, because the processing instruction
   * survives marshalling:
   *)
  dtd # add_pinstr
    (new proc_instruction
       "pxp:dtd"
       "optional-element-and-notation-declarations"
       cfg.encoding);
  ( match cfg.enable_namespace_processing with
	Some mng -> dtd # set_namespace_manager mng
      | None     -> ()
  );
  let doc = new document ?swarner:cfg.swarner cfg.warner cfg.encoding in
  let pobj =
    call_tree_parser
      ~configuration:cfg
      ~source:src
      ~dtd:dtd
      ~document:doc
      ~specification:spec
      ~transform_dtd:(fun x -> x)  (* Do not transform the DTD *)
      ~id_index:None
      ~use_document_entity:false
      ~entry:(`Entry_content [])   (* Entry point of the grammar *)
      ~init_lexer:Content          (* The initially used lexer *)
  in
  match pobj # root with
      Some r -> r
    | None -> raise(WF_error("No root element"))
;;


let iparse_document_entity ?(transform_dtd = (fun x -> x))
                           ?id_index
                           cfg0 src spec p_wf =
  (* Parse an element given as separate entity *)
  (* p_wf: 'true' if in well-formedness mode, 'false' if in validating mode *)
  let cfg = { cfg0 with
		recognize_standalone_declaration =
                   cfg0.recognize_standalone_declaration && (not p_wf)
            } in
  let dtd = new dtd ?swarner:cfg.swarner cfg.warner cfg.encoding in
  if p_wf then begin
    (* Instead of dtd # allow_arbitrary, because the processing instruction
     * survives marshalling:
     *)
    dtd # add_pinstr
      (new proc_instruction
	 "pxp:dtd"
	 "optional-element-and-notation-declarations"
	 cfg.encoding);
  end;
  ( match cfg.enable_namespace_processing with
	Some mng -> dtd # set_namespace_manager mng
      | None     -> ()
  );
  let doc = new document ?swarner:cfg.swarner cfg.warner cfg.encoding in
  let entry_flags =
    (if p_wf then [] else [`Extend_dtd_fully]) @
    [`Parse_xml_decl]
  in
  let pobj =
    call_tree_parser
      ~configuration:cfg
      ~source:src
      ~dtd:dtd
      ~document:doc
      ~specification:spec
      ~transform_dtd:(fun dtd ->
			let dtd' = transform_dtd dtd in
			if cfg.accept_only_deterministic_models then
			  dtd' # only_deterministic_models;
			dtd')

      ~id_index:(id_index :> 'ext index option)
      ~use_document_entity:true
      ~entry:(`Entry_document entry_flags)   (* Entry point of the grammar *)
      ~init_lexer:Document         (* The initially used lexer *)
  in
  pobj # doc
;;


let parse_document_entity ?(transform_dtd = (fun x -> x))
                          ?id_index
                          cfg src spec =
  iparse_document_entity
    ~transform_dtd:transform_dtd
    ?id_index:(id_index : 'ext #index option :> 'ext index option)
    cfg src spec false;;

let parse_wfdocument_entity ?(transform_dtd = (fun x -> x))
                            cfg src spec =
  iparse_document_entity ~transform_dtd cfg src spec true;;





(* === Old definition, no longer used

let extract_dtd_from_document_entity cfg src =
  let transform_dtd dtd = raise (Return_DTD dtd) in
  try
    let doc = parse_document_entity
		~transform_dtd:transform_dtd
		cfg
		src
		default_spec in
    (* Should not happen: *)
    doc # dtd
  with
      Return_DTD dtd ->
	(* The normal case: *)
	dtd
;;

=== *)

(* ======================================================================
 * History:
 * 
 * $Log: pxp_tree_parser.ml,v $
 * Revision 1.4  2003/06/20 21:00:33  gerd
 * 	Moved events to Pxp_types.
 * 	Implementation of namespaces in event-based parsers.
 *
 * Revision 1.3  2003/06/20 19:41:39  gerd
 * 	Added ~transform_dtd to parse_wfdocument_entity.
 *
 * Revision 1.2  2003/06/20 15:14:14  gerd
 * 	Introducing symbolic warnings, expressed as polymorphic
 * variants
 *
 * Revision 1.1  2003/06/15 18:18:34  gerd
 * 	Initial revision
 *
 * 
 *)
