(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_lexer_types


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
    scan_tag_eb          = Pxp_lex_within_tag_iso88591.scan_tag_eb;
    scan_tag_eb_att      = Pxp_lex_content_string_iso88591.scan_tag_eb_att;
  }
;;


Pxp_lexers.init lexer_set_iso88591
;;

