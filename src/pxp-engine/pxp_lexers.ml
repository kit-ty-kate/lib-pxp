(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)


open Pxp_core_types
open Pxp_lexer_types

let lexer_sets = Hashtbl.create 5;;

let dummy_lexer_set =
  let dummy _ = 
    failwith "No lexical analyzer available! (Badly linked executable?)" in
  { lex_encoding         = `Enc_iso88591;
    scan_document        = dummy;
    scan_content         = dummy;
    scan_within_tag      = dummy;
    scan_document_type   = dummy;
    scan_declaration     = dummy;
    scan_comment         = dummy;
    scan_ignored_section = dummy;
    detect_xml_pi        = dummy;
    scan_xml_pi          = dummy;
    scan_pi_string       = dummy;
    scan_dtd_string      = dummy;
    scan_content_string  = dummy;
    scan_name_string     = dummy;
    scan_for_crlf        = dummy;
    scan_characters      = dummy;
    scan_character       = dummy;
    scan_tag_eb          = dummy;
    scan_tag_eb_att      = dummy;
  }
;;


let current_lexer_set = ref dummy_lexer_set


let init ls =
  Hashtbl.add lexer_sets ls.lex_encoding ls;
  current_lexer_set := ls
;;


let get_lexer_set enc =
  (* This function must be safe against concurrent calls in mt environments *)
  let cls = !current_lexer_set in
  if enc = cls.lex_encoding then
    cls
  else
    try
      let ls = Hashtbl.find lexer_sets enc in
      current_lexer_set := ls;
      ls
    with
	Not_found ->
	  failwith ("Pxp_lexers: This type of internal encoding is not supported")
;;

