(* $Id: pxp_lexers.ml,v 1.11 2003/06/15 12:23:21 gerd Exp $
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


let current_lexer_set_encoding = ref `Enc_iso88591;;


let init ls =
  Hashtbl.add lexer_sets ls.lex_encoding ls;
  current_lexer_set_encoding := ls.lex_encoding;
  current_lexer_set := ls
;;


let get_lexer_set enc =
  if enc = !current_lexer_set_encoding then
    !current_lexer_set
  else
    try
      let ls = Hashtbl.find lexer_sets enc in
      current_lexer_set_encoding := ls.lex_encoding;
      current_lexer_set := ls;
      ls
    with
	Not_found ->
	  failwith ("Pxp_lexers: This type of internal encoding is not supported")
;;

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexers.ml,v $
 * Revision 1.11  2003/06/15 12:23:21  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.10  2002/08/31 23:26:50  gerd
 * 	Follow-up of pxp_lexer_types.mli, rev. 1.11
 *
 * Revision 1.9  2002/08/28 23:54:34  gerd
 * 	Support for new lexer definition style.
 *
 * Revision 1.8  2002/08/03 17:55:21  gerd
 * 	Followup of pxp_lexer_types.mli rev 1.8
 *
 * Revision 1.7  2001/10/12 21:38:14  gerd
 * 	Changes for O'caml 3.03-alpha.
 *
 * Revision 1.6  2001/06/14 23:29:03  gerd
 * 	Arbitrary lexical analyzers can be plugged into the parser,
 * not only for ISO-8859-1 and UTF-8.
 *
 * Revision 1.5  2000/09/17 00:11:42  gerd
 * 	Updated for wlexers.
 *
 * Revision 1.4  2000/05/29 23:48:38  gerd
 * 	Changed module names:
 * 		Markup_aux          into Pxp_aux
 * 		Markup_codewriter   into Pxp_codewriter
 * 		Markup_document     into Pxp_document
 * 		Markup_dtd          into Pxp_dtd
 * 		Markup_entity       into Pxp_entity
 * 		Markup_lexer_types  into Pxp_lexer_types
 * 		Markup_reader       into Pxp_reader
 * 		Markup_types        into Pxp_types
 * 		Markup_yacc         into Pxp_yacc
 * See directory "compatibility" for (almost) compatible wrappers emulating
 * Markup_document, Markup_dtd, Markup_reader, Markup_types, and Markup_yacc.
 *
 * Revision 1.3  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.2  2000/05/23 00:09:44  gerd
 * 	The UTF-8 lexer set is no longer initialized here. It is done
 * in the new module Pxp_utf8. Reason: You can link without UTF-8 support.
 *
 * Revision 1.1  2000/05/20 20:30:50  gerd
 * 	Initial revision.
 *
 * 
 *)
