(* $Id: pxp_lexers.mli,v 1.7 2003/06/15 12:23:21 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)


open Pxp_core_types
open Pxp_lexer_types

val get_lexer_set : rep_encoding -> lexer_set
  (* Return the set of lexer functions that is able to handle the passed
   * encoding.
   *)

val dummy_lexer_set : lexer_set
  (* Only internal usage! *)

val init : lexer_set -> unit
  (* Add a new lexerset *)

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexers.mli,v $
 * Revision 1.7  2003/06/15 12:23:21  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.6  2002/08/28 23:54:34  gerd
 * 	Support for new lexer definition style.
 *
 * Revision 1.5  2001/06/14 23:29:03  gerd
 * 	Arbitrary lexical analyzers can be plugged into the parser,
 * not only for ISO-8859-1 and UTF-8.
 *
 * Revision 1.4  2000/05/29 23:48:38  gerd
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
 * Revision 1.3  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.2  2000/05/23 00:09:44  gerd
 * 	The UTF-8 lexer set is no longer initialized here. It is done
 * in the new module Pxp_utf8. Reason: You can link without UTF-8 support.
 *
 * Revision 1.1  2000/05/20 20:30:50  gerd
 * 	Initial revision.
 *
 * 
 *)
