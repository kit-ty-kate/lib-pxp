(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_document
open Pxp_yacc
open Pxp_dtd

val write_document : out_channel -> 'ext document -> unit
    (* Writes O'Caml code to the out_channel that is a top-level function
     * creating a fresh document which is equal to the passed document:
     *
     * "let create_document ?enable_namespace_processing config spec = ...;;"
     *
     * If you compile the code and call "create_document config spec"  the 
     * function creates a document tree which is (almost) equal to the 
     * passed document.
     * 
     * The following properties may not be equal:
     * - Parsed entities
     * - Whether a declaration occurs in an external entity or not
     * 
     * 'config': a Pxp_yacc.config
     * 'spec': a Pxp_document.spec
     * enable_namespace_processing: You can pass here a namespace_manager
     *   to enable the namespace code (default: no namespace processing)
     *
     * NOTE: The signature of the generated function has changed from
     * PXP 1.0 to PXP 1.1; the first argument is now 'config' and not
     * 'warner'
     *)

  

val write_subtree : out_channel -> 'ext node -> unit
    (* Writes O'Caml code to the out_channel that is a top-level function
     * creating a fresh node tree which is equal to the passed tree:
     *
     * "let create_subtree dtd spec = ...;;"
     *
     * If you compile the code and call "create_subtree dtd spec"  the 
     * function creates a DTD object which is equal to the passed object.
     * 
     * 'dtd': a DTD object
     * 'spec': a Pxp_document.spec
     *)


(* write_dtd: this method is deprecated! *)

  

