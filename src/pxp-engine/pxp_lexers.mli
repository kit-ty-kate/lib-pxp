(* $Id: pxp_lexers.mli,v 1.2 2000/05/23 00:09:44 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Markup_types
open Markup_lexer_types

val get_lexer_set : encoding -> lexer_set
  (* Return the set of lexer functions that is able to handle the passed
   * encoding.
   *)

val init_utf8 : lexer_set -> unit
  (* Internally used. *)

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexers.mli,v $
 * Revision 1.2  2000/05/23 00:09:44  gerd
 * 	The UTF-8 lexer set is no longer initialized here. It is done
 * in the new module Pxp_utf8. Reason: You can link without UTF-8 support.
 *
 * Revision 1.1  2000/05/20 20:30:50  gerd
 * 	Initial revision.
 *
 * 
 *)
