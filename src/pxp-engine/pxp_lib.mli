(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* This module contains special versions of library functions which
 * have been optimized for various data and runtime cases.
 *)

val crlf_index_from : string -> int -> int
    (* Returns the leftmost position >= i in the string of either a CR or a
     * LF character.
     * Returns -1 if there is no such character.
     *)

val only_whitespace : string -> bool
    (* Returns true if the string consists only of whitespace characters
     * (space, tab, CR, LF)
     *)

