(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)


open Pxp_core_types
open Pxp_lexer_types

let lexer_factories = Hashtbl.create 100;;

class false_factory msg : lexer_factory =
object
  method encoding = `Enc_usascii
  method open_source _ =
    failwith msg
  method open_string _ =
    failwith msg
  method open_string_inplace _ =
    failwith msg
end


let init ls =
  Hashtbl.add lexer_factories ls#encoding ls
;;


let get_lexer_factory enc =
  try
    Hashtbl.find lexer_factories enc
  with
      Not_found ->
	failwith ("Pxp_lexers: This type of internal encoding is not supported: " ^ Netconversion.string_of_encoding (enc :> encoding) ^ " - maybe missing lexing module?")
;;


(* The following emulation of get_lexer_set works _only_ for WDialog!
 * We can assume that get_lexer_set is called every time a new buffer
 * is to be scanned. Furthermore, we know that the lexbuf was created
 * with Lexing.from_string.
 *)

open Pxp_reader;;

let get_lexer_set (enc : rep_encoding) = (* DEPRECATED *)
  let enc' = (enc :> encoding) in
  let factory = get_lexer_factory enc in

  let old_obj = ref None in

  let open_obj buf =
    match !old_obj with
	None ->
	  let src = { lsrc_lexbuf = 
			lazy buf;
		      lsrc_unicode_lexbuf = 
			lazy(Netulex.ULB.from_string enc' buf.Lexing.lex_buffer)
		    } in
	  let obj = factory # open_source src in
	  old_obj := Some obj;
	  obj
      | Some obj -> obj
  in

  let scan_name_string buf =
    let lobj = open_obj buf in
    lobj # scan_name_string()
  in

  { scan_name_string = scan_name_string }
;;
