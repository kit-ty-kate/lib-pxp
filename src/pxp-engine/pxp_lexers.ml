(* $Id: pxp_lexers.ml,v 1.7 2001/10/12 21:38:14 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)


open Pxp_types
open Pxp_lexer_types

let lexer_sets = Hashtbl.create 5;;

(*
let lexer_set_iso88591 = 
  { lex_encoding         = `Enc_iso88591;
    scan_document        = Pxp_lex_document_iso88591.scan_document;
    scan_content         = Pxp_lex_content_iso88591.scan_content;
    scan_within_tag      = Pxp_lex_within_tag_iso88591.scan_within_tag;
    scan_document_type   = Pxp_lex_document_type_iso88591.
			     scan_document_type;
    scan_declaration     = Pxp_lex_declaration_iso88591.scan_declaration;
    scan_content_comment  = Pxp_lex_misc_iso88591.scan_content_comment;
    scan_decl_comment     = Pxp_lex_misc_iso88591.scan_decl_comment;
    scan_document_comment = Pxp_lex_misc_iso88591.scan_document_comment;
    scan_ignored_section = Pxp_lex_name_string_iso88591.
                             scan_ignored_section;
    scan_xml_pi          = Pxp_lex_misc_iso88591.scan_xml_pi;
    scan_dtd_string      = Pxp_lex_dtd_string_iso88591.scan_dtd_string;
    scan_content_string  = Pxp_lex_content_string_iso88591.
			     scan_content_string;
    scan_name_string     = Pxp_lex_name_string_iso88591.scan_name_string;
    scan_only_xml_decl   = Pxp_lex_misc_iso88591.scan_only_xml_decl;
    scan_for_crlf        = Pxp_lex_misc_iso88591.scan_for_crlf;
  }
;;
*)


let current_lexer_set = 
  let dummy _ = failwith "No lexical analyzer available! (Badly linked executable?)" in
  ref
    { lex_encoding         = `Enc_iso88591;
      scan_document        = dummy;
      scan_content         = dummy;
      scan_within_tag      = dummy;
      scan_document_type   = dummy;
      scan_declaration     = dummy;
      scan_content_comment = dummy;
      scan_decl_comment    = dummy;
      scan_document_comment= dummy;
      scan_ignored_section = dummy;
      scan_xml_pi          = dummy;
      scan_dtd_string      = dummy;
      scan_content_string  = dummy;
      scan_name_string     = dummy;
      scan_only_xml_decl   = dummy;
      scan_for_crlf        = dummy;
    }
;;

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
