(* $Id: pxp_codewriter.mli,v 1.2 2000/07/09 00:30:14 gerd Exp $
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
     * "let create_document warner spec = ...;;"
     *
     * If you compile the code and call "create_document warner map"  the 
     * function creates a document tree which is (almost) equal to the 
     * passed document.
     * 
     * The following properties may not be equal:
     * - Parsed entities
     * - Whether a declaration occurs in an external entity or not
     * 
     * 'warner': a collect_warnings object
     * 'spec': a Pxp_document.spec
     *)

  
val write_dtd : out_channel -> dtd -> unit
    (* Writes O'Caml code to the out_channel that is a top-level function
     * creating a fresh DTD which is equal to the passed DTD:
     *
     * "let create_dtd warner = ...;;"
     *
     * If you compile the code and call "create_dtd warner"  the 
     * function creates a DTD object which is (almost) equal to the 
     * passed object.
     * 
     * The following properties may not be equal:
     * - Parsed entities
     * - Whether a declaration occurs in an external entity or not
     * 
     * 'warner': a collect_warnings object
     *)

val write_subtree : out_channel -> 'ext node -> unit
    (* Writes O'Caml code to the out_channel that is a top-level function
     * creating a fresh node tree which is equal to the passed tree:
     *
     * "let create_subtree dtd map = ...;;"
     *
     * If you compile the code and call "create_subtree dtd map"  the 
     * function creates a DTD object which is equal to the passed object.
     * 
     * 'dtd': a DTD object
     * 'map': a domspec
     *)


  

(* ======================================================================
 * History:
 * 
 * $Log: pxp_codewriter.mli,v $
 * Revision 1.2  2000/07/09 00:30:14  gerd
 * 	Updated.
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
 * Old logs from markup_codewriter.mli:
 *
 * Revision 1.1  2000/03/11 22:57:28  gerd
 * 	Initial revision.
 *
 * 
 *)
