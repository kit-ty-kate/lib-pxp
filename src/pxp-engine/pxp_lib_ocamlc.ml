(* $Id$
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

