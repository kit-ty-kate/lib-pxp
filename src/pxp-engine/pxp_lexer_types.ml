(* $Id: pxp_lexer_types.ml,v 1.10 2002/08/28 23:54:34 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

type lexers =
    Document
  | Document_type
  | Content
  | Within_tag
  | Within_tag_entry
  | Declaration
  | Content_comment
  | Decl_comment
  | Document_comment
  | Ignored_section
  | Closed
  | Tag_eb
  | Tag_eb_att of bool


let string_of_lexers =
  function
      Document          -> "Document"
    | Document_type     -> "Document_type"
    | Content           -> "Content"
    | Within_tag        -> "Within_tag"
    | Within_tag_entry  -> "Within_tag_entry"
    | Declaration       -> "Declaration"
    | Content_comment   -> "Content_comment"
    | Decl_comment      -> "Decl_comment"
    | Document_comment  -> "Document_comment"
    | Ignored_section   -> "Ignored_section"
    | Closed            -> "Closed"
    | Tag_eb            -> "Tag_eb"
    | Tag_eb_att b      -> "Tag_eb_att " ^ string_of_bool b
;;


type prolog_token =
    Pro_name of string
  | Pro_eq                  (* "=" *)
  | Pro_string of string    (* "..." or '...' *)
  | Pro_eof


type entity_id = < >
  (* The class without properties; but you can still compare if two objects
   * are the same.
   *)

type token = 
  | Begin_entity             (* Beginning of entity *)
  | End_entity               (* End of entity *)
  | Comment_begin of entity_id (* <!-- *)
  | Comment_material of string (* within a comment *)
  | Comment_end of entity_id   (* --> *)
  | Ignore                   (* ignored whitespace *)
  | IgnoreLineEnd            (* ignored whitespace (one newline character) *)
  | Eq                       (* = *)
  | Rangle                   (* > as tag delimiter *)
  | Rangle_empty             (* /> as tag delimiter *)
  | Percent                  (* % followed by space in declaration *)
  | Plus                     (* + in declaration *)
  | Star                     (* * in declaration *)
  | Bar                      (* | in declaration *)
  | Comma                    (* , in declaration *)
  | Qmark                    (* ? in declaration *)
  | Pcdata                   (* #PCDATA in declaration *)
  | Required                 (* #REQUIRED in declaration *)
  | Implied                  (* #IMPLIED in declaration *)
  | Fixed                    (* #FIXED in declaration *)
  | Bof                      (* A marker for 'beginning of file' *)
  | Eof                      (* End of file *)
  | Conditional_begin of entity_id  (* <![ in declaration *)
  | Conditional_body  of entity_id  (* [ in declaration *)
  | Conditional_end   of entity_id  (* ]]> in declaration *)
  | Doctype        of entity_id  (* <!DOCTYPE *)
  | Doctype_rangle of entity_id  (* > as DOCTYPE delimiter *)
  | Dtd_begin      of entity_id  (* '[' after DOCTYPE *)
  | Dtd_end        of entity_id  (* ']' *)
  | Decl_element   of entity_id  (* <!ELEMENT *)
  | Decl_attlist   of entity_id  (* <!ATTLIST *)
  | Decl_entity    of entity_id  (* <!ENTITY *)
  | Decl_notation  of entity_id  (* <!NOTATION *)
  | Decl_rangle    of entity_id  (* > *)
  | Lparen         of entity_id  (* ( in declaration *)
  | Rparen         of entity_id  (* ) in declaration *)
  | RparenPlus     of entity_id  (* )+ in declaration *)
  | RparenStar     of entity_id  (* )* in declaration *)
  | RparenQmark    of entity_id  (* )? in declaration *)
      
  | Tag_beg of (string*entity_id)     (* <name *)
  | Tag_end of (string*entity_id)     (* </name *)

  | PI        of (string*string)      (* <?name ... ?> *)
  | PI_xml    of (prolog_token list)  (* <?xml ...?> *)
  | Cdata     of string               (* <![CDATA[...]]> *)
  | CRef      of int                  (* &#digits; *)
  | ERef      of string               (* &name; *)
  | PERef     of string               (* %name; *)
  | CharData  of string             (* any characters not otherwise matching *)
  | Lcurly                            (* { *)
  | LLcurly                           (* {{ *)
  | Rcurly                            (* } *)
  | RRcurly                           (* }} *)
  | LineEnd   of string
  | LineEnd_att   of string
  | Name      of string               (* name *)
  | Nametoken of string               (* nmtoken but not name *)
  | Attval    of string           (* attribute value; may contain entity refs *)
  | Attval_nl_normalized of string
  | Unparsed_string      of string    (* "data" or 'data' *)
  | SQuote                            (* Single quote *)
  | DQuote                            (* Double quote *)
  | ERef_att of string                (* &name; in attribute values *)


(**********************************************************************)
(* debugging *)

let string_of_tok tok =
  match tok with
    Begin_entity -> "Begin_entity"
  | End_entity -> "End_entity"
  | Doctype _ -> "Doctype"
  | Doctype_rangle _ -> "Doctype_rangle"
  | Comment_begin _ -> "Comment_begin"
  | Comment_end _ -> "Comment_end"
  | Comment_material _ -> "Comment_material"
  | Rangle -> "Rangle"
  | Rangle_empty -> "Rangle_empty"
  | Ignore -> "Ignore"
  | IgnoreLineEnd -> "IgnoreLineEnd"
  | Eq -> "Eq"
  | Dtd_begin _ -> "Dtd_begin"
  | Dtd_end _ -> "Dtd_end"
  | Conditional_begin _ -> "Conditional_begin"
  | Conditional_body _ -> "Conditional_body"
  | Conditional_end _ -> "Conditional_end"
  | Percent -> "Percent"
  | Lparen _ -> "Lparen"
  | Rparen _ -> "Rparen"
  | Plus -> "Plus"
  | Star -> "Star"
  | Bar -> "Bar"
  | Comma -> "Comma"
  | Qmark -> "Qmark"
  | Pcdata -> "Pcdata"
  | Required -> "Required"
  | Implied -> "Implied"
  | Fixed -> "Fixed"
  | Decl_element _ -> "Decl_element"
  | Decl_attlist _ -> "Decl_attlist"
  | Decl_entity _ -> "Decl_entity"
  | Decl_notation _ -> "Decl_notation"
  | Decl_rangle _ -> "Decl_rangle"
  | RparenPlus _ -> "RparenPlus"
  | RparenStar _ -> "RparenStar"
  | RparenQmark _ -> "RparenQmark"
  | Bof -> "Bof"
  | Eof -> "Eof"
  | PI _ -> "PI"
  | PI_xml _ -> "PI_xml"
  | Tag_beg _ -> "Tag_beg"
  | Tag_end _ -> "Tag_end"
  | Cdata _ -> "Cdata"
  | CRef _ -> "CRef"
  | ERef _ -> "ERef"
  | PERef _ -> "PERef"
  | CharData _ -> "CharData"
  | Name _ -> "Name" 
  | Nametoken _ -> "Nametoken" 
  | Attval _ -> "Attval" 
  | Attval_nl_normalized _ -> "Attval_nl_normalized"
  | Unparsed_string _ -> "Unparsed_string" 
  | LineEnd _ -> "LineEnd"
  | LineEnd_att _ -> "LineEnd_att"
  | Lcurly -> "Lcurly"
  | LLcurly -> "LLcurly"
  | Rcurly -> "Rcurly"
  | RRcurly -> "RRcurly"
  | SQuote -> "SQuote"
  | DQuote -> "DQuote"
  | ERef_att _ -> "ERef_att"


type lexer_set =
    { lex_encoding         : Pxp_types.rep_encoding;
      scan_document        : Lexing.lexbuf -> (token * lexers);
      scan_content         : Lexing.lexbuf -> (token * lexers);
      scan_within_tag      : Lexing.lexbuf -> (token * lexers);
      scan_document_type   : Lexing.lexbuf -> (token * lexers);
      scan_declaration     : Lexing.lexbuf -> (token * lexers);
      scan_content_comment : Lexing.lexbuf -> (token * lexers);
      scan_decl_comment    : Lexing.lexbuf -> (token * lexers);
      scan_document_comment: Lexing.lexbuf -> (token * lexers);
      scan_ignored_section : Lexing.lexbuf -> (token * lexers);
      scan_xml_pi          : Lexing.lexbuf -> prolog_token;
      scan_dtd_string      : Lexing.lexbuf -> token;
      scan_content_string  : Lexing.lexbuf -> token;
      scan_name_string     : Lexing.lexbuf -> token;
      scan_only_xml_decl   : Lexing.lexbuf -> token;
      scan_for_crlf        : Lexing.lexbuf -> token;
      scan_characters      : Lexing.lexbuf -> unit;
      scan_tag_eb          : Lexing.lexbuf -> (token * lexers);
      scan_tag_eb_att      : Lexing.lexbuf -> bool -> (token * lexers);
    }

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexer_types.ml,v $
 * Revision 1.10  2002/08/28 23:54:34  gerd
 * 	Support for new lexer definition style.
 *
 * Revision 1.9  2002/08/05 22:33:33  gerd
 * 	New token LineEnd_att for newline characters inside
 * attribute values (event-based parser).
 *
 * Revision 1.8  2002/08/03 17:54:47  gerd
 * 	Support for event-based parsing of attribute values: New
 * lexers Tag_eb, Tag_eb_att, and the new pseudo lexer Within_tag_entry
 * (which immediately changes the lexer to either Within_tag or Tag_eb).
 * New tokens SQuote, DQuote, ERef_att.
 *
 * Revision 1.7  2002/07/14 23:04:17  gerd
 * 	New lexer "Closed", used after an entity is closed.
 * 	New tokens Lcurly, LLcurly, Rcurly, RRcurly.
 *
 * Revision 1.6  2002/03/13 22:45:42  gerd
 * 	Improved Pxp_lexing.
 *
 * Revision 1.5  2001/06/28 22:42:07  gerd
 * 	Fixed minor problems:
 * 	- Comments must be contained in one entity
 * 	- Pxp_document.document is now initialized with encoding.
 *           the DTD encoding may be initialized too late.
 *
 * Revision 1.4  2000/10/01 19:47:53  gerd
 * 	New functions: sub_lexeme, fast_lexing_from_string,
 * reuse_lexing_from_string.
 *
 * Revision 1.3  2000/09/21 21:28:16  gerd
 * 	New token IgnoreLineEnd: simplifies line counting, and
 * corrects a bug.
 *
 * Revision 1.2  2000/08/18 20:14:31  gerd
 * 	Comment -> Comment_begin, Comment_material, Comment_end.
 *
 * Revision 1.1  2000/05/29 23:48:38  gerd
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
 * ======================================================================
 * Old logs from markup_lexer_types.ml:
 *
 * Revision 1.6  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.5  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.4  2000/05/14 17:45:36  gerd
 * 	Bugfix.
 *
 * Revision 1.3  2000/05/14 17:35:12  gerd
 * 	Conditional_begin, _end, and _body have an entity_id.
 *
 * Revision 1.2  2000/05/08 21:59:06  gerd
 * 	New token Bof (beginning of file).
 *
 * Revision 1.1  2000/05/06 23:21:49  gerd
 * 	Initial revision.
 *
 *
 * ======================================================================
 *
 * DERIVED FROM REVISION 1.4 of markup_lexer_types_shadow.ml
 *
 * Revision 1.4  2000/04/30 18:19:04  gerd
 * 	Added new tokens.
 *
 * Revision 1.3  1999/08/31 19:13:31  gerd
 * 	Added checks on proper PE nesting. The idea is that tokens such
 * as Decl_element and Decl_rangle carry an entity ID with them. This ID
 * is simply an object of type < >, i.e. you can only test on identity.
 * The lexer always produces tokens with a dummy ID because it does not
 * know which entity is the current one. The entity layer replaces the dummy
 * ID with the actual ID. The parser checks that the IDs of pairs such as
 * Decl_element and Decl_rangle are the same; otherwise a Validation_error
 * is produced.
 *
 * Revision 1.2  1999/08/10 21:35:08  gerd
 * 	The XML/encoding declaration at the beginning of entities is
 * evaluated. In particular, entities have now a method "xml_declaration"
 * which returns the name/value pairs of such a declaration. The "encoding"
 * setting is interpreted by the entity itself; "version", and "standalone"
 * are interpreted by Markup_yacc.parse_document_entity. Other settings
 * are ignored (this does not conform to the standard; the standard prescribes
 * that "version" MUST be given in the declaration of document; "standalone"
 * and "encoding" CAN be declared; no other settings are allowed).
 * 	TODO: The user should be warned if the standard is not exactly
 * fulfilled. -- The "standalone" property is not checked yet.
 *
 * Revision 1.1  1999/08/10 00:35:51  gerd
 * 	Initial revision.
 *
 * 
 *)
