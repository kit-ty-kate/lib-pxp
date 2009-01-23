(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(** Generate O'Caml code for creating large constant XML trees *)

open Pxp_document
open Pxp_yacc
open Pxp_dtd

(** See also {!Pxp_marshal} for direct marshalling functions. *)

val write_document : out_channel -> 'ext document -> unit
    (** Writes O'Caml code to the [out_channel] so that when the code
     * is compiled and executed, a fresh document is created with the
     * same contents as the passed document:
     *
     * {[ "let create_document ?enable_namespace_processing config spec = ...;;" ]}
     *
     * If you compile the code and call [create_document config spec]  the 
     * function creates a document tree which is (almost) equal to the 
     * passed document.
     * 
     * The following properties may not be equal:
     * - Parsed entities
     * - Whether a declaration occurs in an external entity or not
     * 
     * - [config]: The configuration to assume for re-creating the tree
     * - [spec]:  a {!Pxp_document.spec}
     * - [enable_namespace_processing]: You can pass here a namespace_manager
     *   to enable the namespace code (default: no namespace processing)
     *)

  

val write_subtree : out_channel -> 'ext node -> unit
    (** Writes O'Caml code to the [out_channel] so that when the code
     * is compiled and executed, a fresh tree is created with the
     * same contents as the passed tree:
     *
     * {[ "let create_subtree dtd spec = ...;;" ]}
     *
     * If you compile the code and call [create_subtree dtd spec]  the 
     * function creates a DTD object which is equal to the passed object.
     * 
     * - [dtd]: a DTD object
     * - [spec]: a Pxp_document.spec
     *)

