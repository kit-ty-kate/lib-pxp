(* $Id: pxp_types.ml,v 1.18 2003/06/19 21:10:15 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

include Pxp_core_types

open Pxp_entity
open Pxp_dtd
open Pxp_entity_manager
open Pxp_reader
open Netchannels

type config =
    { warner : collect_warnings;
      enable_pinstr_nodes : bool;
      enable_super_root_node : bool;
      enable_comment_nodes : bool;
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
      enable_namespace_info : bool;
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
    enable_namespace_info = false;
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
  let r, en =
    match src with
	Entity(m,r')  -> r', m dtd
      | ExtID(xid,r') -> r',
	                 if use_document_entity then
                           new document_entity
			     r' dtd "[toplevel]" w xid None
                             cfg.encoding
			 else
                           new external_entity
			     r' dtd "[toplevel]" w xid None false
                             cfg.encoding
      | XExtID(xid,sysbase,r') -> r',
	                          if use_document_entity then
				    new document_entity
				      r' dtd "[toplevel]" w xid sysbase
				      cfg.encoding
				  else
				    new external_entity
				      r' dtd "[toplevel]" w xid sysbase false
				      cfg.encoding
  in
  r # init_rep_encoding cfg.encoding;
  r # init_warner w;
  en # set_debugging_mode (cfg.debugging_mode);
  (r, en)
;;


type entry =
    [ `Entry_document     of [ `Extend_dtd_fully | `Parse_xml_decl ] list
    | `Entry_declarations of [ `Extend_dtd_fully ] list
    | `Entry_content      of [ `Dummy ] list
    | `Entry_expr         of [ `Dummy ] list
    ]


(* ======================================================================
 * History:
 *
 * $Log: pxp_types.ml,v $
 * Revision 1.18  2003/06/19 21:10:15  gerd
 * 	Revised the from_* functions.
 *
 * Revision 1.17  2003/06/15 18:19:56  gerd
 * 	Pxp_yacc has been split up
 *
 * Revision 1.16  2003/06/15 12:23:22  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.15  2003/01/21 00:18:09  gerd
 * 	New type resolver_id. It is related to ext_id but contains
 * more information.
 *
 * Revision 1.14  2002/08/28 23:54:34  gerd
 * 	Support for new lexer definition style.
 *
 * Revision 1.13  2001/06/27 23:33:53  gerd
 * 	Type output_stream is now a polymorphic variant
 *
 * Revision 1.12  2001/06/07 22:49:51  gerd
 * 	New namespace exceptions.
 *
 * Revision 1.11  2001/04/26 23:57:04  gerd
 * 	New exception Method_not_applicable. It is raised if there are
 * classes A and B both conforming to class type C, but A does not implement
 * a method required by the class type. In this case, invoking the method
 * in A raises Method_not_applicable.
 * 	This feature is mainly used in Pxp_document.
 *
 * Revision 1.10  2001/04/22 14:14:41  gerd
 * 	Updated to support private IDs.
 *
 * Revision 1.9  2000/09/17 00:12:19  gerd
 * 	Bugfix in the pool implementation.
 *
 * Revision 1.8  2000/09/09 16:38:47  gerd
 * 	New type 'pool'.
 *
 * Revision 1.7  2000/08/14 22:24:55  gerd
 * 	Moved the module Pxp_encoding to the netstring package under
 * the new name Netconversion.
 *
 * Revision 1.6  2000/07/27 00:41:15  gerd
 * 	new 8 bit codes
 *
 * Revision 1.5  2000/07/16 18:31:09  gerd
 * 	The exception Illegal_character has been dropped.
 *
 * Revision 1.4  2000/07/14 21:25:27  gerd
 * 	Simplified the type 'collect_warnings'.
 *
 * Revision 1.3  2000/07/08 16:23:50  gerd
 * 	Added the exception 'Error'.
 *
 * Revision 1.2  2000/07/04 22:14:05  gerd
 * 	Implemented the changes of rev. 1.2 of pxp_types.mli.
 *
 * Revision 1.1  2000/05/29 23:48:38  gerd
 * 	Changed module names:
 * 		Markup_aux          into Pxp_aux
 * 		Markup_codewriter   into Pxp_codewriter
 * 		Markup_document     into Pxp_document
 * 		Markup_dtd          into Pxp_dtd
 * 		Markup_entity       into Pxp_entity
 * 		Markup_lexer_types  into Pxp_lexer_types
 * 		Markup_reader       into Pxp_reader
 * 		Markup_types        into Pxp_types
 * 		Markup_yacc         into Pxp_yacc
 * See directory "compatibility" for (almost) compatible wrappers emulating
 * Markup_document, Markup_dtd, Markup_reader, Markup_types, and Markup_yacc.
 *
 * ======================================================================
 * Old logs from markup_types.ml:
 *
 * Revision 1.7  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.6  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.5  2000/05/01 20:43:19  gerd
 * 	New type output_stream; new function 'write'.
 *
 * Revision 1.4  1999/09/01 16:25:35  gerd
 * 	Dropped Illegal_token and Content_not_allowed_here. WF_error can
 * be used instead.
 *
 * Revision 1.3  1999/08/15 02:22:33  gerd
 * 	Added exception Undeclared.
 *
 * Revision 1.2  1999/08/14 22:14:58  gerd
 * 	New class "collect_warnings".
 *
 * Revision 1.1  1999/08/10 00:35:52  gerd
 * 	Initial revision.
 *
 *
 *)
