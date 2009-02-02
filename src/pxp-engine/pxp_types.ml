(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

include Pxp_core_types.I

open Pxp_entity
open Pxp_dtd
open Pxp_entity_manager
open Pxp_reader
open Netchannels


type config =
    { warner : collect_warnings;
      swarner : symbolic_warnings option;
      enable_pinstr_nodes : bool;
      enable_comment_nodes : bool;
      enable_super_root_node : bool;
      drop_ignorable_whitespace : bool;
      encoding : rep_encoding;
      recognize_standalone_declaration : bool;
      store_element_positions : bool;
      idref_pass : bool;
      validate_by_dfa : bool;
      accept_only_deterministic_models : bool;
      disable_content_validation : bool;
      name_pool : pool;
      enable_name_pool_for_element_types    : bool;
      enable_name_pool_for_attribute_names  : bool;
      enable_name_pool_for_attribute_values : bool;
      (* enable_name_pool_for_notation_names   : bool; *)
      enable_name_pool_for_pinstr_targets   : bool;
      enable_namespace_processing : namespace_manager option;
      escape_contents : 
	             (Pxp_lexer_types.token -> entity_manager -> string) option;
      escape_attributes : 
	             (Pxp_lexer_types.token -> int -> entity_manager -> string)
	             option;
      debugging_mode : bool;
    }

let default_config =
  let w = new drop_warnings in
  { warner = w;
    swarner = None;
    enable_pinstr_nodes = false;
    enable_super_root_node = false;
    enable_comment_nodes = false;
    drop_ignorable_whitespace = true;
    encoding = `Enc_iso88591;
    recognize_standalone_declaration = true;
    store_element_positions = true;
    idref_pass = false;
    validate_by_dfa = true;
    accept_only_deterministic_models = true;
    disable_content_validation = false;
    name_pool = make_probabilistic_pool 10;
    enable_name_pool_for_element_types = false;
    enable_name_pool_for_attribute_names = false;
    enable_name_pool_for_pinstr_targets = false;
    enable_name_pool_for_attribute_values = false;
    enable_namespace_processing = None;
    escape_contents = None;
    escape_attributes = None;
    debugging_mode = false;
  }

let default_namespace_config =
  { default_config with
      enable_namespace_processing = Some (new namespace_manager)
  }

type source = Pxp_dtd.source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (ext_id * Pxp_reader.resolver)
  | XExtID of (ext_id * string option * Pxp_reader.resolver)


let from_obj_channel ?(alt = []) ?system_id ?fixenc 
                     ?id:init_id ?system_encoding ch =
  let channel_id = allocate_private_id() in
  let r = new resolve_to_any_obj_channel
	    ~channel_of_id:(fun rid ->
			      if rid.rid_private = Some channel_id then begin
				let active_id =
				  match init_id with
				      None -> 
					(match system_id with
					     None -> None
					   | Some sid ->
					       Some (resolver_id_of_ext_id
						       (System sid))
					)
				    | Some xid ->
					Some (resolver_id_of_ext_id xid)
				in
				(ch, fixenc, active_id)
			      end
			      else
				raise Not_competent)
	    ()
  in
  let r_total = new combine 
		  ( [r] @
		    (if init_id <> None then 
		       [ new resolve_as_file 
			   ?system_encoding 
			   ~base_url_defaults_to_cwd:false
			   ()
		       ] 
		     else
		       []) @
		    alt) in
  ExtID(Private channel_id, r_total)
;;


let from_channel ?alt ?system_id ?fixenc ?id ?system_encoding ch =
  from_obj_channel 
    ?alt ?system_id ?system_encoding ?id ?fixenc 
    (new input_channel ch)
;;
    

let from_file ?(alt = []) ?(system_encoding = `Enc_utf8) ?enc utf8_filename =
  let r =
    new resolve_as_file
      ~system_encoding
      ~base_url_defaults_to_cwd:false
      ()
  in

  let url = make_file_url
	      ~system_encoding
	      ?enc
	      utf8_filename in

  let xid = System (Neturl.string_of_url url) in

  let sysbase =
    let cwd = Sys.getcwd() in
    let cwd_slash = if cwd = "/" then cwd else cwd ^ "/" in
    let sysbase_url = make_file_url 
			~system_encoding ~enc:system_encoding cwd_slash in
    Some(Neturl.string_of_url sysbase_url) 
  in
  XExtID(xid, sysbase, new combine (r :: alt))
;;


let from_string ?(alt = []) ?system_id ?fixenc s =
  let channel_id = allocate_private_id() in
  let r = new resolve_to_any_obj_channel
	    ~channel_of_id:(fun rid ->
			      if rid.rid_private = Some channel_id then begin
				let active_id =
				  match system_id with
				      None -> None
				    | Some sid ->
					Some 
					  (resolver_id_of_ext_id(System sid))
				in
				let ch = new input_string s in
				(ch, fixenc, active_id)
			      end
			      else
				raise Not_competent)
	    ()
  in
  let r_total = new combine ( [r] @ alt ) in
  ExtID(Private channel_id, r_total)
;;


let open_source cfg src use_document_entity dtd =
  let w = cfg.warner in
  let sw = cfg.swarner in
  let r, en =
    match src with
	Entity(m,r')  -> r', m dtd
      | ExtID(xid,r') -> r',
	                 if use_document_entity then
                           new document_entity
			     r' dtd "[toplevel]" sw w xid None
                             cfg.encoding
			 else
                           new external_entity
			     r' dtd "[toplevel]" sw w xid None false
                             cfg.encoding
      | XExtID(xid,sysbase,r') -> r',
	                          if use_document_entity then
				    new document_entity
				      r' dtd "[toplevel]" sw w xid sysbase
				      cfg.encoding
				  else
				    new external_entity
				      r' dtd "[toplevel]" sw w xid sysbase 
				      false cfg.encoding
  in
  r # init_rep_encoding cfg.encoding;
  r # init_warner sw w;
  en # set_debugging_mode (cfg.debugging_mode);
  (r, en)
;;

type entity_id = Pxp_lexer_types.entity_id

type entity = Pxp_entity.entity


type entry =
    [ `Entry_document     of [ `Val_mode_dtd | 
			       `Extend_dtd_fully | `Parse_xml_decl ] list
    | `Entry_declarations of [ `Val_mode_dtd | `Extend_dtd_fully ] list
    | `Entry_content      of [ `Dummy ] list
    | `Entry_expr         of [ `Dummy ] list
    ]


type event =
  | E_start_doc of (string * dtd)
  | E_end_doc of string
  | E_start_tag of (string * (string * string) list * 
		    namespace_scope option *
		    Pxp_lexer_types.entity_id)
  | E_end_tag    of (string * Pxp_lexer_types.entity_id)
  | E_char_data of  string
  | E_pinstr of (string * string * Pxp_lexer_types.entity_id)
  | E_pinstr_member of (string * string * Pxp_lexer_types.entity_id)
  | E_comment of string
  | E_start_super
  | E_end_super
  | E_position of (string * int * int)
  | E_error of exn
  | E_end_of_stream


