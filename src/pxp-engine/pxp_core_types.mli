(* $Id: pxp_core_types.mli,v 1.1 2003/06/15 12:22:41 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* The module Pxp_core_types exports all of the module type
 * Pxp_core_types_type.CORE_TYPES.
 *
 * This module is for internal use of PXP only. Users should refer
 * to Pxp_types instead. This module exports the same types, exceptions,
 * and values, plus a number of additional definitions.
 *)

include Pxp_core_types_type.CORE_TYPES

(* ======================================================================
 * History:
 * 
 * $Log: pxp_core_types.mli,v $
 * Revision 1.1  2003/06/15 12:22:41  gerd
 * 	Initial revision
 *
 * 
 *)
