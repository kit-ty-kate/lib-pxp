(* $Id: pxp_dtd_parser.mli,v 1.1 2003/06/15 18:18:34 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_dtd

val parse_dtd_entity : config -> source -> dtd
  (* Parse an entity containing a DTD (external subset), and return this DTD. *)

val extract_dtd_from_document_entity : config -> source -> dtd
  (* Parses a closed document, i.e. a document beginning with <!DOCTYPE...>,
   * and returns the DTD contained in the document.
   * The parts of the document outside the DTD are actually not parsed,
   * i.e. parsing stops when all declarations of the DTD have been read.
   *)


(* ======================================================================
 * History:
 * 
 * $Log: pxp_dtd_parser.mli,v $
 * Revision 1.1  2003/06/15 18:18:34  gerd
 * 	Initial revision
 *
 * 
 *)
