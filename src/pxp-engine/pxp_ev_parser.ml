(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_lexers
open Pxp_lexer_types
open Pxp_entity_manager
open Pxp_dtd
open Pxp_core_parser
open Pxp_aux


(* The subclass event_parser for the event-based interface: *)

class event_parser init_dtd init_config init_event_handler 
                   init_want_start_doc init_pull_counter lit_root =
  let null_entity_id = Pxp_dtd.Entity.create_entity_id() in
object (self)
  inherit core_parser init_dtd init_config init_pull_counter

  val event_handler = init_event_handler
  val want_start_doc = init_want_start_doc

  val mutable init_done = false       (* element stack initialized? *)

  val mutable xml_version = "1.0"
  val mutable xml_standalone = false

  val mutable ep_root_element_seen = false
  val mutable ep_elstack = stack_create (None,"","",null_entity_id)
  val mutable ep_early_events = []


  method root_element_seen = ep_root_element_seen


  method check_whether_root_is_closed() =
    assert(ep_root_element_seen);
    if n_tags_open <> 0 then
      raise(WF_error("Root element is not closed"));


  method private init_for_xml_body _ =
    if not init_done then begin
      if config.recognize_standalone_declaration then
	dtd # set_standalone_declaration xml_standalone;
      if want_start_doc then
	event_handler (E_start_doc(xml_version,
				   dtd));
      if config.enable_super_root_node then
	event_handler E_start_super;
      (* Init namespace processing, if necessary: *)
      ( match config.enable_namespace_processing with
	    None -> ()
	  | Some mng -> self # init_ns_processing mng
      );
      init_done <- true;
      List.iter event_handler ep_early_events;
      ep_early_events <- [];
    end

  (********************************* EVENTS *****************************)

  method private event_document_xmldecl xmldecl =
    let v, _, s = decode_doc_xml_pi (decode_xml_pi xmldecl) in
    check_version_num v;
    xml_version <- v;
    let v = match s with
	None -> false
      | Some "yes" -> true
      | Some "no" -> false
      | _ -> raise (WF_error("Illegal 'standalone' declaration"))
    in
    xml_standalone <- v


  method private event_start_tag position name attlist emptiness tag_beg_entid =
    (match position with
	 Some(e,l,c) -> event_handler(E_position(e,l,c))
       | None -> ()
    );
    if n_tags_open = 0 then begin
      if ep_root_element_seen then
	raise(WF_error("Document must consist of only one toplevel element"));
      ep_root_element_seen <- true;
      lit_root := name
    end;

    match config.enable_namespace_processing with
	None ->
	  (* no namespaces *)
	  if not emptiness then
	    stack_push (position, name, "", tag_beg_entid) ep_elstack;
	  event_handler(E_start_tag(name,attlist,None,tag_beg_entid));
	  if emptiness then
	    event_handler(E_end_tag(name,tag_beg_entid))
      | Some mng ->
	  (* enabled namespaces *)
	  let (src_prefix, localname, norm_name, norm_attlist) =
            self # push_src_norm_mapping mng name attlist in
	  let attlist' =
	    List.map (fun (orig_prefix, localname, norm_name, value) ->
			(norm_name, value)) norm_attlist in
	  if not emptiness then
	    stack_push (position, name, norm_name, tag_beg_entid) ep_elstack;
	  event_handler(E_start_tag
			  (norm_name,attlist',ns_scope,tag_beg_entid));
	  if emptiness then (
	    self # pop_src_norm_mapping();
	    event_handler(E_end_tag(norm_name,tag_beg_entid));
	  )


  method private event_end_tag name tag_end_entid =
    let norm_name =  
      (* only used in namespace mode, else "" *)
      ( try
	  let x_pos, x_name, x_norm_name, tag_beg_entid = 
	    stack_pop ep_elstack in
	  if name <> x_name then begin
	    let where = 
	      match x_pos with
		| None -> ""
		| Some (_, 0, _) -> ""
		| Some (x_entname, x_line, x_col) ->
		    " (was at line " ^ string_of_int x_line ^
		    ", position " ^ string_of_int x_col ^ ")" 
	    in
	    raise(WF_error("End tag `" ^ name ^
			   "' does not match start tag `" ^ x_name ^ "'" ^
			   where))
	  end;
	  if tag_beg_entid != tag_end_entid then
	    raise(WF_error("End tag `" ^ name ^
			   "' not in the same entity as the start tag `" ^
			   x_name ^ "'"));
	  x_norm_name
	with
	    Stack.Empty ->
	      assert false;   (* because n_tags_open = 0 is checked *)
      ) in

    match config.enable_namespace_processing with
	None ->
	  (* no namespaces *)
	  event_handler(E_end_tag(name,tag_end_entid))

      | Some mng ->
	  (* namespaces *)
	  self # pop_src_norm_mapping();
	  event_handler(E_end_tag(norm_name,tag_end_entid))
	  

  method private event_char_data data =
    event_handler(E_char_data(data))


  method private event_pinstr position target value ent_id =
    if config.enable_pinstr_nodes &&
       (n_tags_open > 0 || config.enable_super_root_node) then begin
      let ev_list = 
	(match position with
	     Some(e,l,c) -> [ E_position(e,l,c) ]
	   | None -> []
	)
	@ [ E_pinstr(target,value,ent_id) ] in
      if init_done then
	List.iter event_handler ev_list
      else
	ep_early_events <- ep_early_events @ ev_list
    end
    else begin
      let ev = E_pinstr_member(target,value,ent_id) in
      if init_done then
	event_handler ev
      else
	ep_early_events <- ep_early_events @ [ev]
    end


  method private event_comment position mat =
    if config.enable_comment_nodes && 
       (n_tags_open > 0 || config.enable_super_root_node) then begin
       let ev_list = 
	(match position with
	     Some(e,l,c) -> [ E_position(e,l,c) ]
	   | None -> []
	)
	@ [ E_comment(String.concat "" mat) ] in
      if init_done then
	List.iter event_handler ev_list
      else
	ep_early_events <- ep_early_events @ ev_list
    end


  method private sub_parser () =
    let pobj = new event_parser dtd config event_handler false (-1) (ref "") in
    (pobj :> core_parser)

end
;;

(**********************************************************************)
(* event-based interface *)

let create_entity_manager ?(is_document=true) cfg src =
  let dtd = new dtd ?swarner:cfg.swarner cfg.warner cfg.encoding in
  ( match cfg.enable_namespace_processing with
	Some mng -> dtd # set_namespace_manager mng
      | None -> ()
  );
(*
  dtd # add_pinstr               (* select well-formedness mode *)
    (new proc_instruction
       "pxp:dtd"
       "optional-element-and-notation-declarations"
       cfg.encoding);
*)
  let r, en =
    open_source cfg src is_document dtd in
  new entity_manager en dtd
;;


let process_entity 
      cfg entry mgr eh =

  let have_document_entry = 
    match entry with `Entry_document _ -> true | _ -> false in
  let lit_root = ref "" in
  let pobj =
    new event_parser
      mgr#dtd
      cfg
      eh
      have_document_entry
      (-1)
      lit_root
  in
  let init_lexer =
    match entry with
	`Entry_document _     -> Document
      | `Entry_declarations _ -> failwith "Pxp_yacc.process_entity: bad entry point"
      | `Entry_content _      -> Content
      | `Entry_expr _         -> Content
  in
  let en = mgr # current_entity in
  let gen_att_events = Some(cfg.escape_attributes <> None) in
  en # open_entity ?gen_att_events true init_lexer;
  begin try
    let context = make_context mgr in

    pobj # parse context (entry : entry :> extended_entry);
    if en # is_open then ignore(en # close_entity);
    if cfg.enable_super_root_node then
      eh E_end_super;
    if have_document_entry then eh (E_end_doc !lit_root);
    eh E_end_of_stream;
  with
    | Failure "Invalid UTF-8 stream" ->
	(* raised by the wlex-generated lexers only: map to Malformed_code *)
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	let e = At(pos, Netconversion.Malformed_code) in
	eh (E_error e);
	raise e
    | error ->
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	let e = At(pos, error) in
	eh (E_error e);
	raise e
  end;
;;


let process_expr 
      ?first_token
      ?following_token
      cfg mgr eh =
  let lit_root = ref "" in
  let pobj = new event_parser mgr#dtd cfg eh false (-1) lit_root in
  let en = mgr # current_entity in
  begin try
    let context = make_context ?first_token mgr in
    pobj # parse context (`Entry_expr []);
    ( match following_token with
          Some v -> v := context.current_token
	| None   -> ()
    );
  with
    | Failure "Invalid UTF-8 stream" ->
	(* raised by the wlex-generated lexers only: map to Malformed_code *)
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	let e = At(pos, Netconversion.Malformed_code) in
	eh (E_error e);
	raise e
    | error ->
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	let e = At(pos, error) in
	eh (E_error e);
	raise e
  end
;;


let close_entities mgr =
  let top = mgr#top_entity in
  mgr # pop_entity_until top;
  if top # is_open then ignore(top # close_entity)


let create_pull_parser cfg entry mgr =

  (* Do control inversion with a queue serving as buffer, and a very special
   * kind of continuations 
   *)

  let pull_queue = Queue.create() in
  let pull_queue_eof = ref false in

  let eh event =
    Queue.add event pull_queue
  in

  let have_document_entry = 
    match entry with `Entry_document _ -> true | _ -> false in
  let lit_root = ref "" in
  let pobj =
    new event_parser
      mgr#dtd
      cfg
      eh
      have_document_entry
      100                   (* the number of loops until Interrupt_parsing *)
      lit_root
  in
  let init_lexer =
    match entry with
	`Entry_document _     -> Document
      | `Entry_declarations _ -> failwith "Pxp_yacc.process_entity: bad entry point"
      | `Entry_content _      -> Content
      | `Entry_expr _         -> Content
  in
  let en = mgr # current_entity in
  let gen_att_events = Some(cfg.escape_attributes <> None) in
  en # open_entity ?gen_att_events true init_lexer;

  let fill = ref (fun () -> ()) in
    (* This function is called when the queue is empty to add more elements *)

  let rec return_result x =
    try
      Some(Queue.take pull_queue)
    with
	Queue.Empty ->
	  if !pull_queue_eof then
	    None
	  else begin
	    !fill();
	    return_result x
	  end
  in

  let record_error exn =
    match exn with
      | Failure "Invalid UTF-8 stream" ->
	  (* raised by the wlex-generated lexers only: map to Malformed_code *)
	  let pos = mgr # position_string in
	  mgr # pop_entity_until en;
	  if en # is_open then ignore(en # close_entity);
	  let e = At(pos, Netconversion.Malformed_code) in
	  eh (E_error e);
	  pull_queue_eof := true;
      | error ->
	  let pos = mgr # position_string in
	  mgr # pop_entity_until en;
	  if en # is_open then ignore(en # close_entity);
	  let e = At(pos, error) in
	  eh (E_error e);
	  pull_queue_eof := true;
  in
  
  let rec parse_slice context e () =
    try
      pobj # parse context e;
      (* If the [parse] method terminates, the end of the stream is reached!
       *)
      if en # is_open then ignore(en # close_entity);
      if cfg.enable_super_root_node then eh E_end_super;
      if have_document_entry then eh (E_end_doc !lit_root);
      eh E_end_of_stream;
      pull_queue_eof := true;
    with
      | Interrupt_parsing st ->
	  fill := parse_slice st.cont_context (`Entry_continuation st);
	  ()
      | exn ->
	  record_error exn
  in

  try
    let context =  make_context mgr  in
    fill := parse_slice context (entry : entry :> extended_entry);
    return_result
  with
    | exn -> 
	record_error exn;
	return_result
;;
