(* $Id$
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

