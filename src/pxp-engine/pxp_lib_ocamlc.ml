(* $Id: pxp_lib_ocamlc.ml,v 1.3 2001/12/19 21:05:10 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* WARNING: This file is pxp_lib_ocamlc.ml *)

(* Functions optimized for the bytecode compiler *)

let crlf_re = Netstring_str.regexp "[\010\013]";;

let search_forward = Netstring_str.search_forward ~groups:0;;

let crlf_index_from s i =
  try fst(search_forward crlf_re s i)
  with Not_found -> -1
;;

let nowhitespace_re = Netstring_str.regexp "[^\009\010\013\032]";;

let only_whitespace s =
  try
    ignore(search_forward nowhitespace_re s 0);
    false
  with
      Not_found -> true
;;

(* ======================================================================
 * History:
 *
 * $Log: pxp_lib_ocamlc.ml,v $
 * Revision 1.3  2001/12/19 21:05:10  gerd
 * 	Fix: Works now with both versions of netstring (0.10 and
 * 0.91).
 *
 * Revision 1.2  2001/12/15 17:34:09  gerd
 * 	Fixes for O'Caml 3.04
 *
 * Revision 1.1  2000/10/01 19:50:29  gerd
 * 	Initial revision.
 *
 *)
