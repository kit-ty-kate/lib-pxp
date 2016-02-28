(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* WARNING: This file is pxp_lib_ocamlopt.ml *)

(* Functions optimized for the native code compiler *)

let rec index_rec s i =
  let s_i = String.unsafe_get s i in
  if s_i = '\010' || s_i = '\013'
  then i
  else index_rec s (i+1)
;;

let rec index_lim_rec s lim i =
  if i >= lim then -1 else
    let s_i = String.unsafe_get s i in
    if s_i = '\010' || s_i = '\013'
    then i
    else index_lim_rec s lim (i+1)
;;

let crlf_index_from s i =
  let lim = String.length s in
  assert (i>=0 && i <= lim);
  if lim = 0 || i = lim then
    -1
  else
    index_lim_rec s lim i
;;

exception Found;;

let only_whitespace s =
  let l = String.length s in
  try
    for i=0 to l - 1 do
      match String.unsafe_get s i with
	  ('\009'|'\010'|'\013'|'\032') -> ()
	| _ ->
	    raise Found
    done;
    true
  with
      Found -> false
;;

