(* $Id: pxp_yacc.mli,v 1.27 2003/06/20 15:14:14 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


(*$ markup-yacc.mli *)

(**********************************************************************)

(* THE WHOLE MODULE PXP_YACC IS DEPRECATED.
 *
 * The functionality has been distributed over several smaller modules:
 * - Pxp_types: has most type definitions
 * - Pxp_dtd_parser: DTD parsing
 * - Pxp_tree_parser: Parsers that represent the XML tree as object tree
 *   (with classes from Pxp_document)
 * - Pxp_ev_parser: Event-based parsing
 *
 * There are no plans to delete the module completely, however. It just
 * remains as old-style interface.
 *)

(**********************************************************************)

open Pxp_types
open Pxp_dtd
open Pxp_document


type config = Pxp_types.config =
    { warner : collect_warnings;
      swarner : symbolic_warnings option;
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
      name_pool : Pxp_core_types.pool;
      enable_name_pool_for_element_types    : bool;
      enable_name_pool_for_attribute_names  : bool;
      enable_name_pool_for_attribute_values : bool;
      enable_name_pool_for_pinstr_targets   : bool;
      enable_namespace_processing : Pxp_dtd.namespace_manager option;
      enable_namespace_info : bool;
      escape_contents : 
         (Pxp_lexer_types.token -> Pxp_entity_manager.entity_manager -> 
		string) option;
      escape_attributes : 
         (Pxp_lexer_types.token -> int -> Pxp_entity_manager.entity_manager -> 
		string) option;
      debugging_mode : bool;
    }


val default_config : config
  (* now defined in Pxp_types *)

val default_namespace_config : config
  (* now defined in Pxp_types *)

type source = Pxp_types.source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (ext_id * Pxp_reader.resolver)
  | XExtID of (ext_id * string option * Pxp_reader.resolver)
      (* (ext_id, system_base, resolver) *)
  (* both defined in Pxp_types and Pxp_dtd *)

val from_channel : 
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      ?id:ext_id -> 
      ?system_encoding:encoding -> 
      in_channel -> 
        source
  (* now defined in Pxp_types *)

val from_string :
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      string -> 
        source
  (* now defined in Pxp_types *)

val from_obj_channel :
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      ?id:ext_id -> 
      ?system_encoding:encoding -> 
      Netchannels.in_obj_channel -> 
        source
  (* now defined in Pxp_types *)


val from_file :
       ?alt:Pxp_reader.resolver list ->
       ?system_encoding:encoding -> ?enc:encoding -> string -> source
  (* now defined in Pxp_types *)

exception ID_not_unique
  (* now defined in Pxp_tree_parser *)

class type [ 'ext ] index = [ 'ext ] Pxp_tree_parser.index

class [ 'ext ] hash_index : [ 'ext ] Pxp_tree_parser.hash_index

val default_extension : ('a node extension) as 'a
  (* now defined in Pxp_tree_parser *)

val default_spec : ('a node extension as 'a) spec
  (* now defined in Pxp_tree_parser *)

val default_namespace_spec : ('a node extension as 'a) spec
  (* now defined in Pxp_tree_parser *)

val parse_document_entity : 
  ?transform_dtd:(dtd -> dtd) ->
  ?id_index:('ext index) ->
  config -> source -> 'ext spec -> 'ext document
  (* now defined in Pxp_tree_parser *)

val parse_wfdocument_entity : 
  config -> source -> 'ext spec -> 'ext document
  (* now defined in Pxp_tree_parser *)

val parse_content_entity  : 
  ?id_index:('ext index) ->
  config -> source -> dtd -> 'ext spec -> 'ext node
  (* now defined in Pxp_tree_parser *)

val parse_wfcontent_entity : 
  config -> source -> 'ext spec -> 'ext node
  (* now defined in Pxp_tree_parser *)

val parse_dtd_entity : config -> source -> dtd
  (* now defined in Pxp_dtd_parser *)

val extract_dtd_from_document_entity : config -> source -> dtd
  (* now defined in Pxp_dtd_parser *)

type event = Pxp_ev_parser.event =
  | E_start_doc of (string * bool * dtd)
  | E_end_doc
  | E_start_tag of (string * (string * string) list * Pxp_lexer_types.entity_id)
  | E_end_tag   of (string * Pxp_lexer_types.entity_id)
  | E_char_data of  string
  | E_pinstr of (string * string)
  | E_comment of string
  | E_position of (string * int * int)
  | E_error of exn
  | E_end_of_stream

val create_entity_manager :
      ?is_document:bool ->
      config -> 
      source -> 
        Pxp_entity_manager.entity_manager
  (* now defined in Pxp_ev_parser *)

type entry = Pxp_types.entry

val process_entity :
      config -> 
      entry ->
      Pxp_entity_manager.entity_manager ->
      (event -> unit) ->
        unit
  (* now defined in Pxp_ev_parser *)

val process_expr :
      ?first_token: Pxp_lexer_types.token ->
      ?following_token: Pxp_lexer_types.token ref ->
      config -> 
      Pxp_entity_manager.entity_manager ->
      (event -> unit) ->
        unit
  (* now defined in Pxp_ev_parser *)

val create_pull_parser :
      config -> 
      entry ->
      Pxp_entity_manager.entity_manager ->
        ('a -> event option)
  (* now defined in Pxp_ev_parser *)


(*$-*)


(* ======================================================================
 * History:
 *
 * $Log: pxp_yacc.mli,v $
 * Revision 1.27  2003/06/20 15:14:14  gerd
 * 	Introducing symbolic warnings, expressed as polymorphic
 * variants
 *
 * Revision 1.26  2003/06/19 21:10:15  gerd
 * 	Revised the from_* functions.
 *
 * Revision 1.25  2003/06/15 18:19:56  gerd
 * 	Pxp_yacc has been split up
 *
 * Revision 1.24  2003/06/15 12:23:22  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.23  2003/01/21 00:19:40  gerd
 * 	Support for the new resolvers.
 *
 * Revision 1.22  2002/10/22 23:29:48  gerd
 * 	More docs for process_entity
 *
 * Revision 1.21  2002/08/17 23:51:56  gerd
 * 	Added pull parsing.
 *
 * Revision 1.20  2002/08/17 19:53:53  gerd
 * 	Changed type [entry] into a polymorphic variant. New variant
 * `Entry_expr. New: flags for [entry].
 * 	New: [process_expr].
 *
 * Revision 1.19  2002/08/05 22:34:29  gerd
 * 	escape_attributes: this config option has an additional
 * argument "position".
 *
 * Revision 1.18  2002/08/03 17:55:59  gerd
 * 	Support for event-based parsing of attribute values: New config
 * option escape_attributes.
 *
 * Revision 1.17  2002/07/14 23:05:01  gerd
 * 	Event-based interface.
 *
 * Revision 1.16  2002/03/10 23:40:52  gerd
 * 	type source is now primarily defined in Pxp_dtd.
 *
 * Revision 1.15  2001/06/08 01:15:47  gerd
 * 	Moved namespace_manager from Pxp_document to Pxp_dtd. This
 * makes it possible that the DTD can recognize the processing instructions
 * <?pxp:dtd namespace prefix="..." uri="..."?>, and add the namespace
 * declaration to the manager.
 *
 * Revision 1.14  2001/06/07 22:53:20  gerd
 * 	New config option: drop_ignorable_whitespace.
 * 	New defaults: default_namespace_config, default_namespace_spec.
 *
 * Revision 1.13  2001/05/17 21:39:15  gerd
 * 	New options: enable_namespace_processing, enable_namespace_info.
 *
 * Revision 1.12  2001/04/24 21:07:13  gerd
 * 	New option ~alt in from_channel and from_file.
 *
 * Revision 1.11  2001/04/03 20:22:44  gerd
 * 	New resolvers for catalogs of PUBLIC and SYSTEM IDs.
 * 	Improved "combine": PUBLIC and SYSTEM IDs are handled
 * separately.
 * 	Rewritten from_file: Is now a simple application of the
 * Pxp_reader classes and functions. (The same has still to be done
 * for from_channel!)
 *
 * Revision 1.10  2000/10/01 19:48:25  gerd
 * 	Option errors_with_line_numbers has been removed.
 *
 * Revision 1.9  2000/09/21 21:30:46  gerd
 * 	New option: disable_content_validation
 *
 * Revision 1.8  2000/09/09 16:41:03  gerd
 * 	Effort to reduce the amount of allocated memory: The number of
 * instance variables in document nodes has been miminized; the class
 * default_ext no longer stores anything; string pools have been implemented.
 *
 * Revision 1.7  2000/08/18 20:15:43  gerd
 * 	Config options:
 * - enable_super_root_nodes: new name for virtual_root
 * - enable_pinstr_nodes: new name for processing_instructions_inline
 * - enable_comment_nodes: new option
 * 	Updated comments for various options.
 *
 * Revision 1.6  2000/07/23 02:16:33  gerd
 * 	Support for DFAs.
 *
 * Revision 1.5  2000/07/14 13:57:29  gerd
 * 	Added the id_index feature.
 *
 * Revision 1.4  2000/07/09 17:52:54  gerd
 * 	New option store_element_positions.
 *
 * Revision 1.3  2000/07/08 16:26:21  gerd
 * 	Added the signatures of the functions
 * 'extract_dtd_from_document_entity' and 'parse_wfcontent_entity'.
 * Updated the signature of 'parse_document_entity': New optional
 * argument 'transform_dtd'.
 * 	Updated the comments.
 *
 * Revision 1.2  2000/07/04 22:09:03  gerd
 * 	MAJOR CHANGE: Redesign of the interface (not yet complete).
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
 * Old logs from markup_yacc.mli:
 *
 * Revision 1.4  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.3  2000/05/27 19:24:01  gerd
 * 	New option: recognize_standalone_declaration.
 *
 * Revision 1.2  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.1  2000/05/06 23:21:49  gerd
 * 	Initial revision.
 *
 * Revision 1.9  2000/04/30 18:23:38  gerd
 * 	New config options 'processing_instructions_inline' and
 * 'virtual_root'.
 *
 * Revision 1.8  2000/03/13 23:46:46  gerd
 * 	Change: The 'resolver' component of the 'config' type has
 * disappeared. Instead, there is a new resolver component in the Entity
 * and ExtID values of 'source'. I hope that this makes clearer that the
 * resolver has only an effect if used together with Entity and ExtID
 * sources.
 * 	Change: The Entity value can now return the entity dependent
 * on the DTD that is going to be used.
 *
 * Revision 1.7  2000/02/22 02:32:02  gerd
 * 	Updated.
 *
 * Revision 1.6  2000/02/22 01:52:45  gerd
 * 	Added documentation.
 *
 * Revision 1.5  2000/01/20 20:54:43  gerd
 * 	New config.errors_with_line_numbers.
 *
 * Revision 1.4  1999/09/01 23:09:10  gerd
 * 	New function parse_wf_entity that simulates a well-formedness
 * parser.
 *
 * Revision 1.3  1999/09/01 16:26:36  gerd
 * 	Added an empty line. This is *really* a big change.
 *
 * Revision 1.2  1999/08/14 22:20:27  gerd
 *         The "config" slot has now a component "warner"which is
 * an object with a "warn" method. This is used to warn about characters
 * that cannot be represented in the Latin 1 alphabet.
 *         Furthermore, there is a new component "debugging_mode".
 *
 * Revision 1.1  1999/08/10 00:35:52  gerd
 * 	Initial revision.
 *
 *
 *)
