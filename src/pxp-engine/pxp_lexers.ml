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

