(* $Id$
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
  ?transform_dtd:(dtd -> dtd) ->
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


(* Event-based stuff now only in Pxp_ev_parser! *)


(*$-*)


