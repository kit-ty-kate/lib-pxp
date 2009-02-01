(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(** Calling the parser to read DTDs *)

open Pxp_types
open Pxp_dtd

val create_empty_dtd : config -> dtd
  (** Create an empty DTD. See also {!Pxp_dtd.create_dtd} for a lower-level
      DTD constructor not requiring a full [config] record.
   *)

val parse_dtd_entity : config -> Pxp_types.source -> dtd
  (** Parse an entity containing a DTD (external subset), and return this DTD. *)

val extract_dtd_from_document_entity : config -> Pxp_types.source -> dtd
  (** Parses a closed document, i.e. a document beginning with [<!DOCTYPE...>],
   * and returns the DTD contained in the document.
   * The parts of the document outside the DTD are actually not parsed,
   * i.e. parsing stops when all declarations of the DTD have been read.
   *)
