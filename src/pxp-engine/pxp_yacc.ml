(* $Id$ -*- tuareg -*-
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Parsing
open Pxp_core_types
open Pxp_lexer_types
open Pxp_dtd
open Pxp_entity
open Pxp_entity_manager
open Pxp_document
open Pxp_aux
open Pxp_reader
open Netchannels

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


let default_config = Pxp_types.default_config

let default_namespace_config = Pxp_types.default_namespace_config


type source = Pxp_types.source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (ext_id * Pxp_reader.resolver)
  | XExtID of (ext_id * string option * Pxp_reader.resolver)


let from_channel = Pxp_types.from_channel

let from_string = Pxp_types.from_string

let from_obj_channel = Pxp_types.from_obj_channel

let from_file = Pxp_types.from_file

exception ID_not_unique = Pxp_tree_parser.ID_not_unique

class type [ 'ext ] index = [ 'ext ] Pxp_tree_parser.index

class [ 'ext ] hash_index = [ 'ext ] Pxp_tree_parser.hash_index

let default_extension = Pxp_tree_parser.default_extension

let default_spec = Pxp_tree_parser.default_spec

let default_namespace_spec = Pxp_tree_parser.default_namespace_spec

let parse_document_entity = Pxp_tree_parser.parse_document_entity

let parse_wfdocument_entity = Pxp_tree_parser.parse_wfdocument_entity

let parse_content_entity = Pxp_tree_parser.parse_content_entity

let parse_wfcontent_entity = Pxp_tree_parser.parse_wfcontent_entity

let parse_dtd_entity = Pxp_dtd_parser.parse_dtd_entity

let extract_dtd_from_document_entity = 
  Pxp_dtd_parser.extract_dtd_from_document_entity


type event = Pxp_types.event =
  | E_start_doc of (string * bool * dtd)
  | E_end_doc
  | E_start_tag of (string * (string * string) list * Pxp_lexer_types.entity_id)
  | E_ns_start_tag of (string * string * (string * string * string) list *
		       Pxp_dtd.namespace_scope * Pxp_lexer_types.entity_id)
  | E_end_tag   of (string * Pxp_lexer_types.entity_id)
  | E_ns_end_tag of (string * string * Pxp_lexer_types.entity_id)
  | E_char_data of  string
  | E_pinstr of (string * string)
  | E_comment of string
  | E_position of (string * int * int)
  | E_error of exn
  | E_end_of_stream

let create_entity_manager = Pxp_ev_parser.create_entity_manager

type entry = Pxp_types.entry

let process_entity = Pxp_ev_parser.process_entity

let process_expr = Pxp_ev_parser.process_expr

let create_pull_parser = Pxp_ev_parser.create_pull_parser


