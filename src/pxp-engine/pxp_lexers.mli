(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)


open Pxp_core_types.I
open Pxp_lexer_types

val get_lexer_factory : rep_encoding -> lexer_factory
  (** Return the lexer factory that is able to handle the passed
   * encoding.
   *)

val init : lexer_factory -> unit
  (** Add a new factory to the set of known factories *)

class false_factory : string -> lexer_factory
  (** A factory that always fails with the passed string when it is
   * tried to open a source
   *)

val get_lexer_set : rep_encoding -> lexer_set
  (* DEPRECATED. Only exists because WDialog needs it. This function
   * is only emulated to the level that WDialog can use it.
   *)
  (* Return the set of lexer functions that is able to handle the passed
   * encoding.
   *)

