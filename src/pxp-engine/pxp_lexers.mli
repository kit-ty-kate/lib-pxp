(* $Id: pxp_lexers.mli,v 1.1 2000/05/20 20:30:50 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Markup_types
open Markup_lexer_types

val get_lexer_set : encoding -> lexer_set
  (* Return the set of lexer functions that is able to handle the passed
   * encoding.
   *)

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexers.mli,v $
 * Revision 1.1  2000/05/20 20:30:50  gerd
 * 	Initial revision.
 *
 * 
 *)
