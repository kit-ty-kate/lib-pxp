(* $Id: pxp_lexers.ml,v 1.1 2000/05/20 20:30:50 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Markup_types
open Markup_lexer_types

let get_lexer_set enc =
  match enc with
      Enc_iso88591 ->
	{ lex_encoding         = Enc_iso88591;
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
    | Enc_utf8 ->
	{ lex_encoding         = Enc_utf8;
	  scan_document        = Pxp_lex_document_utf8.scan_document;
	  scan_content         = Pxp_lex_content_utf8.scan_content;
	  scan_within_tag      = Pxp_lex_within_tag_utf8.scan_within_tag;
	  scan_document_type   = Pxp_lex_document_type_utf8.
				   scan_document_type;
	  scan_declaration     = Pxp_lex_declaration_utf8.scan_declaration;
	  scan_content_comment  = Pxp_lex_misc_utf8.scan_content_comment;
          scan_decl_comment     = Pxp_lex_misc_utf8.scan_decl_comment;
          scan_document_comment = Pxp_lex_misc_utf8.scan_document_comment;
	  scan_ignored_section = Pxp_lex_name_string_utf8.scan_ignored_section;
	  scan_xml_pi          = Pxp_lex_misc_utf8.scan_xml_pi;
	  scan_dtd_string      = Pxp_lex_dtd_string_utf8.scan_dtd_string;
	  scan_content_string  = Pxp_lex_content_string_utf8.
				   scan_content_string;
	  scan_name_string     = Pxp_lex_name_string_utf8.scan_name_string;
	  scan_only_xml_decl   = Pxp_lex_misc_utf8.scan_only_xml_decl;
	  scan_for_crlf        = Pxp_lex_misc_utf8.scan_for_crlf;
	}
    | _ ->
	failwith ("Pxp_lexers: This type of internal encoding is not supported")
;;

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexers.ml,v $
 * Revision 1.1  2000/05/20 20:30:50  gerd
 * 	Initial revision.
 *
 * 
 *)