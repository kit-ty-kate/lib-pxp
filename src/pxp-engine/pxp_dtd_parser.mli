(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_dtd

val create_empty_dtd : config -> dtd
  (* Create an empty DTD *)

val parse_dtd_entity : config -> source -> dtd
  (* Parse an entity containing a DTD (external subset), and return this DTD. *)

val extract_dtd_from_document_entity : config -> source -> dtd
  (* Parses a closed document, i.e. a document beginning with <!DOCTYPE...>,
   * and returns the DTD contained in the document.
   * The parts of the document outside the DTD are actually not parsed,
   * i.e. parsing stops when all declarations of the DTD have been read.
   *)


