(* $Id$ -*- tuareg -*-
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Parsing
open Pxp_core_types.I
open Pxp_lexer_types
open Pxp_dtd
open Pxp_entity
open Pxp_entity_manager
open Pxp_document
open Pxp_aux
open Pxp_reader
open Netchannels

include Pxp_types

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

