(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* WARNING: This file is pxp_lib_ocamlopt.ml *)

(* Functions optimized for the native code compiler *)

let rec index_rec s i =
  let s_i = String.unsafe_get s i in
  if s_i = '\010' or s_i = '\013'
  then i
  else index_rec s (i+1)
;;

let rec index_lim_rec s lim i =
  if i >= lim then -1 else
    let s_i = String.unsafe_get s i in
    if s_i = '\010' or s_i = '\013'
    then i
    else index_lim_rec s lim (i+1)
;;

let crlf_index_from s i =
  let lim = String.length s in
  assert (i>=0 && i <= lim);
  if lim = 0 || i = lim then
    -1
  else if lim <= 9 then begin
    index_lim_rec s lim i
  end
  else begin
    let c = String.unsafe_get s (lim-1) in
    String.unsafe_set s (lim-1) '\010';
    let k = index_rec s i in
    String.unsafe_set s (lim-1) c;
    if k = lim-1 then begin
      if c = '\010' then
	k
      else
	-1
    end
    else
      k
  end
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

