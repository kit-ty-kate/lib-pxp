(* $Id: pxp_wlex.mll,v 1.5 2001/06/28 22:42:41 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

classes 
  invalid                  (* contains all invalid code points *)
  unicode_baseChar
  ideographic 
  extender
  ascii_digit
  unicode_digit
  combiningChar
  otherChar                (* but not control chars *)
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz"
  "<>?!-/[]&#;%+*|,()'=.:_" '"'
  '\010'
  '\013'
  '\009'
  '\032'

{
  open Pxp_types
  open Pxp_lexer_types

  class dummy_entity = object end

  let dummy_entity = ( new dummy_entity : entity_id )

  (* The following tokens are pre-allocated to reduce the load on the
   * GC.
   *)

  let tok_Doctype__Document_type = Doctype dummy_entity, Document_type
  let tok_Ignore__Document       = Ignore, Document
  let tok_Ignore__Within_tag     = Ignore, Within_tag
  let tok_IgnoreLineEnd__Within_tag = IgnoreLineEnd, Within_tag
  let tok_Ignore__Document_type  = Ignore, Document_type
  let tok_Ignore__Declaration    = Ignore, Declaration
  let tok_Ignore__Ignored        = Ignore, Ignored_section
  let tok_Eof__Document          = Eof, Document
  let tok_Eof__Content           = Eof, Content
  let tok_Eof__Within_tag        = Eof, Within_tag
  let tok_Eof__Document_type     = Eof, Document_type
  let tok_Eof__Declaration       = Eof, Declaration
  let tok_Eof__Ignored           = Eof, Ignored_section
  let tok_LineEndCRLF__Content   = LineEnd "\r\n", Content
  let tok_LineEndCR__Content     = LineEnd "\r", Content
  let tok_LineEndLF__Content     = LineEnd "\n", Content
  let tok_CharDataRBRACKET__Content = CharData "]", Content
  let tok_Eq__Within_tag         = Eq, Within_tag
  let tok_Rangle__Content        = Rangle, Content
  let tok_Rangle_empty__Content  = Rangle_empty, Content
  let tok_Dtd_begin__Declaration = Dtd_begin dummy_entity, Declaration
  let tok_Doctype_rangle__Document = Doctype_rangle dummy_entity, Document
  let tok_Percent__Declaration   = Percent, Declaration
  let tok_Plus__Declaration      = Plus, Declaration
  let tok_Star__Declaration      = Star, Declaration
  let tok_Bar__Declaration       = Bar, Declaration
  let tok_Comma__Declaration     = Comma, Declaration
  let tok_Qmark__Declaration     = Qmark, Declaration
  let tok_Lparen__Declaration    = Lparen dummy_entity, Declaration
  let tok_RparenPlus__Declaration   = RparenPlus dummy_entity, Declaration
  let tok_RparenStar__Declaration   = RparenStar dummy_entity, Declaration
  let tok_RparenQmark__Declaration  = RparenQmark dummy_entity, Declaration
  let tok_Rparen__Declaration    = Rparen dummy_entity, Declaration
  let tok_Required__Declaration  = Required, Declaration
  let tok_Implied__Declaration   = Implied, Declaration
  let tok_Fixed__Declaration     = Fixed, Declaration
  let tok_Pcdata__Declaration    = Pcdata, Declaration
  let tok_Decl_element__Declaration  = Decl_element dummy_entity, Declaration
  let tok_Decl_attlist__Declaration  = Decl_attlist dummy_entity, Declaration
  let tok_Decl_entity__Declaration   = Decl_entity dummy_entity, Declaration
  let tok_Decl_notation__Declaration = Decl_notation dummy_entity, Declaration
  let tok_Conditional_begin__Declaration = Conditional_begin dummy_entity, 
                                           Declaration 
  let tok_Conditional_begin__Ignored     = Conditional_begin dummy_entity, 
                                           Ignored_section
  let tok_Conditional_end__Declaration   = Conditional_end dummy_entity, 
                                           Declaration
  let tok_Conditional_end__Ignored       = Conditional_end dummy_entity, 
                                           Ignored_section
  let tok_Conditional_body__Declaration  = Conditional_body dummy_entity, 
                                           Declaration
  let tok_Decl_rangle__Declaration   = Decl_rangle dummy_entity, Declaration
  let tok_Dtd_end__Document_type     = Dtd_end dummy_entity, Document_type

  open Pxp_wlex_aux

}


let ws = [ ' ' '\t' '\r' '\n' ]

let ascii_hexdigit = [ "ABCDEFabcdef" ] | ascii_digit

let letter = [ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	       "abcdefghijklmnopqrstuvwxyz"
             ] | unicode_baseChar | ideographic

let digit = ascii_digit | unicode_digit

let namechar = letter | digit | '.' | ':' | '-' | '_' | combiningChar | extender

let name = ( letter | '_' | ':' ) namechar*

let nmtoken = namechar+

let character = [^ invalid ]

let character_except_question_mark =                    (* '?' = '\063' *)
  [^ '?' invalid]


let character_except_right_angle_bracket =              (* '>' = '\062' *)
  [^ '>' invalid]


let character_except_minus =                            (* '-' = '\045' *)
  [^ '-' invalid]

let character_except_quot =                             (* '"' = '\034' *)
  [^ '"' invalid]

let character_except_apos =                             (* '\'' = '\039' *)
  [^ "'" invalid]


let pi_string = character_except_question_mark* 
                ( '?' character_except_right_angle_bracket 
                      character_except_question_mark* )* 
                '?'?


let comment_string = character_except_minus* 
                     ('-' character_except_minus+ )*


let normal_character = 
  [^ '\010' '\013' '&' '<' ']' invalid]

let character_except_rbracket =                               (* ']' = '\093' *)
  [^ ']' invalid]

let character_except_rbracket_rangle =          (* ']' = '\093', '>' = '\062' *)
  [^ ']' '>' invalid]


let cdata_string = 
  character_except_rbracket*
  ( "]" character_except_rbracket+ |
    "]]" ']'* character_except_rbracket_rangle character_except_rbracket*
  )*
  ']'*


let printable_character_except_amp_lt =
  (* '&' = '\038', '<' = '\060' *)
  [^ '&' '<' '\009' '\010' '\013' invalid]

let printable_character_except_amp_percent =
  (* '%' = '\037', '&' = '\038' *)
  [^ '&' '%' '\009' '\010' '\013' invalid]

let character_except_special =
  (* '<'=060, ']'=093, '"'=034, '\''=039 *)
  [^ '<' ']' '"' "'" invalid]
  


rule scan_content = parse
    "<?" pi_string "?>"
      { scan_pi (Lexing.lexeme lexbuf) (scan_xml_pi engine), Content }
  | "<?"
      { raise (WF_error ("Illegal processing instruction")) }
  | "<!--"
      { Comment_begin dummy_entity, Content_comment }
  | '<' '/'? name
      (* One rule for Tag_beg and Tag_end saves transitions. *)
      { let l = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
	if Lexing.lexeme_char lexbuf 1 = '/' then
	  Tag_end (sub_lexeme lexbuf 2 (l-2), dummy_entity), 
	  Within_tag 
	else
	  Tag_beg (sub_lexeme lexbuf 1 (l-1), dummy_entity), 
	  Within_tag 
      }
  | "<![CDATA[" cdata_string "]]>"
      { let l = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
	Cdata (sub_lexeme lexbuf 9 (l-12)), Content }
  | "<!"
      { raise (WF_error "Declaration either malformed or not allowed in this context") 
      }
  | "<"
      { raise (WF_error ("The left angle bracket '<' must be written as '&lt;'"))
      }
  | "&#" ascii_digit+ ";"
      { let l = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
	CRef (int_of_string (sub_lexeme lexbuf 2 (l-3))), Content }
  | "&#x" ascii_hexdigit+ ";"
      { let l = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
	CRef (int_of_string ("0x" ^ sub_lexeme lexbuf 3 (l-4))), Content }
  | "&" name ";"
      { let l = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
	ERef (sub_lexeme lexbuf 1 (l-2)), Content }
  | "&" 
      { raise (WF_error ("The ampersand '&' must be written as '&amp;'"))
      }

  (* LineEnd: Depending on whether we are reading from a primary source
   * (file) or from the replacement text of an internal entity, line endings
   * must be normalized (converted to \n) or not.
   * The entity classes do that. The yacc parser will never see LineEnd;
   * this token is always converted to the appropriate CharData token.
   *)

  | '\013' '\010'
      { tok_LineEndCRLF__Content }
  | '\013'
      { tok_LineEndCR__Content }
  | '\010'
      { tok_LineEndLF__Content }
  | eof
      { tok_Eof__Content }
  | "]]>" 
      { raise (WF_error ("The sequence ']]>' must be written as ']]&gt;'"))
      }
  | "]"
      { tok_CharDataRBRACKET__Content }
  | normal_character+
      { let s = Lexing.lexeme lexbuf in
	CharData s, Content 
      }
  | _
      { raise Netconversion.Malformed_code }


and scan_within_tag = parse
    '\013' '\010'
      { tok_IgnoreLineEnd__Within_tag }
  | '\013'
      { tok_IgnoreLineEnd__Within_tag }
  | '\010'
      { tok_IgnoreLineEnd__Within_tag }
  | [' ' '\t']+
      { tok_Ignore__Within_tag }
  | name
      { Name (Lexing.lexeme lexbuf ), Within_tag }
  | '='
      { tok_Eq__Within_tag }
  | '"' character_except_quot* '"'
      { let l = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
	let v = sub_lexeme lexbuf 1 (l-2) in
	Attval v, Within_tag }
  | '"'
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | "'" character_except_apos* "'"
      { let l = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
	let v = sub_lexeme lexbuf 1 (l-2) in
	Attval v, Within_tag }
  | "'"
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | '>'
      { tok_Rangle__Content }
  | "/>"
      { tok_Rangle_empty__Content }
  | eof
      { tok_Eof__Within_tag }
  | character
      { raise (WF_error ("Illegal inside tags")) }
  | _
      { raise Netconversion.Malformed_code }


(* This lexer is used to expand and normalize attribute values: *)

and scan_content_string = parse
    '&' name ';'
      { let s = Lexing.lexeme lexbuf in
	ERef (String.sub s 1 (String.length s - 2)) }
  | "&#" ascii_digit+ ";"
      { let s = Lexing.lexeme lexbuf in
	CRef (int_of_string (String.sub s 2 (String.length s - 3))) }
  | "&#x" ascii_hexdigit+ ";"
      { let s = Lexing.lexeme lexbuf in
	CRef (int_of_string ("0x" ^ String.sub s 3 (String.length s - 4))) }
  | '&'
      { raise(WF_error("The character '&' must be written as '&amp;'")) }
  | printable_character_except_amp_lt+
      { CharData "" (* (Lexing.lexeme lexbuf) *) }
  | '\009'
      { CRef 32 }
  | '\013' '\010'
      { CRef(-1)   (* A special case *)
      }
  | '\013'
      { CRef 32 }
  | '\010'
      { CRef 32 }
  | '<'
      { 
	(* Depending on the situation, '<' may be legal or not: *)
	CharData "<" 
      }
  | eof
      { Eof }
  | _
      { raise Netconversion.Malformed_code }


(* scan_document: Lexer for the outermost structures *)

and scan_document = parse
    "<?" pi_string "?>"
      { scan_pi (Lexing.lexeme lexbuf) (scan_xml_pi engine), Document }
  | "<?"
      { raise (WF_error ("Illegal processing instruction")) }
  | "<!DOCTYPE"
      { tok_Doctype__Document_type }
  | "<!--" 
      { Comment_begin dummy_entity, Document_comment }
  | "<!"
      { raise (WF_error "Declaration either malformed or not allowed in this context") 
      }
  | "<" name
      { let s = Lexing.lexeme lexbuf in
	Tag_beg (String.sub s 1 (String.length s - 1), dummy_entity), Within_tag
      }
  | '<'
      { raise (WF_error ("Illegal token")) }
  | ws+
      { tok_Ignore__Document }
  | eof
      { tok_Eof__Document }
  | character
      { raise (WF_error ("Content not allowed here")) }
  | _
      { raise Netconversion.Malformed_code }


(* scan_document_type: after "<!DOCTYPE" until matching ">" *)

and scan_document_type = parse
    name
      { let s = Lexing.lexeme lexbuf in
	Name s, Document_type }
  | ws+
      { tok_Ignore__Document_type }
  | '"' character_except_quot* '"'
      { let s = Lexing.lexeme lexbuf in
	(Unparsed_string (String.sub s 1 (String.length s - 2))), Document_type }
  | '"'
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | "'" character_except_apos* "'"
      { let s = Lexing.lexeme lexbuf in
	(Unparsed_string (String.sub s 1 (String.length s - 2))), Document_type }
  | "'"
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | '['
      { tok_Dtd_begin__Declaration }
  | '>'
      { tok_Doctype_rangle__Document }
  | eof
      { tok_Eof__Document_type }
  | '&' 
      { raise (WF_error("References to general entities not allowed here")) }
  | '%' 
      { raise (WF_error("References to parameter entities not allowed here")) }
  | character
      { raise (WF_error("Content not allowed here")) }
  | _
      { raise Netconversion.Malformed_code }



(* scan_declaration: after "[" in DTD until matching "]" *)

and scan_declaration = parse
    ws+
      { tok_Ignore__Declaration }
  | '%' name ';'
      { let s = Lexing.lexeme lexbuf in
	(PERef (String.sub s 1 (String.length s - 2))), Declaration }
  | '%'
      { tok_Percent__Declaration }
  | '&' 
      { raise(WF_error("References to general entities not allowed in DTDs")) }
  | name
      { Name (Lexing.lexeme lexbuf), Declaration }
  | nmtoken
      { Nametoken (Lexing.lexeme lexbuf), Declaration }
  | '+'
      { tok_Plus__Declaration }
  | '*'
      { tok_Star__Declaration }
  | '|'
      { tok_Bar__Declaration }
  | ','
      { tok_Comma__Declaration }
  | '?'
      { tok_Qmark__Declaration }
  | '('
      { tok_Lparen__Declaration }
  | ")+" 
      { tok_RparenPlus__Declaration }
  | ")*" 
      { tok_RparenStar__Declaration }
  | ")?"
      { tok_RparenQmark__Declaration }
  | ')'
      { tok_Rparen__Declaration }
  | "#REQUIRED"
      { tok_Required__Declaration }
  | "#IMPLIED"
      { tok_Implied__Declaration }
  | "#FIXED"
      { tok_Fixed__Declaration }
  | "#PCDATA"
      { tok_Pcdata__Declaration }
  | "<!ELEMENT"
      { tok_Decl_element__Declaration }
  | "<!ATTLIST"
      { tok_Decl_attlist__Declaration }
  | "<!ENTITY"
      { tok_Decl_entity__Declaration }
  | "<!NOTATION"
      { tok_Decl_notation__Declaration }
  | "<!--"
      { Comment_begin dummy_entity, Decl_comment }
  | "<!["
      { tok_Conditional_begin__Declaration }
  | "]]>"
      { tok_Conditional_end__Declaration }
  | "["
      { tok_Conditional_body__Declaration }

  (* TODO: PIs modified *) 

  | "<?" pi_string "?>"
      { scan_pi (Lexing.lexeme lexbuf) (scan_xml_pi engine), Declaration }
  | "<?"
      { raise (WF_error ("Illegal processing instruction")) }
  | '"' character_except_quot* '"'
      { let s = Lexing.lexeme lexbuf in
	(Unparsed_string (String.sub s 1 (String.length s - 2))), Declaration }
  | '"'
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | "'" character_except_apos* "'"
      { let s = Lexing.lexeme lexbuf in
	(Unparsed_string (String.sub s 1 (String.length s - 2))), Declaration }
  | "'"
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | '>'
      { tok_Decl_rangle__Declaration }
  | ']'
      { tok_Dtd_end__Document_type }
  | eof
      { tok_Eof__Declaration }
  | "<!"
      { raise (WF_error "Declaration either malformed or not allowed in this context") 
      }
  | character
      { raise (WF_error("Illegal token or character")) }
  | _
      { raise Netconversion.Malformed_code }


(* The following scanner is used to determine the replacement text of
 * internal entities:
 *)

and scan_dtd_string = parse
    '%' name ';'
      { let s = Lexing.lexeme lexbuf in
	PERef (String.sub s 1 (String.length s - 2)) }
  | '%'
      { raise(WF_error("The character '%' must be written as '&#37;'")) }
  | '&' name ';'
      { let s = Lexing.lexeme lexbuf in
	ERef (String.sub s 1 (String.length s - 2)) }
  | "&#" ascii_digit+ ";"
      { let s = Lexing.lexeme lexbuf in
	CRef (int_of_string (String.sub s 2 (String.length s - 3))) }
  | "&#x" ascii_hexdigit+ ";"
      { let s = Lexing.lexeme lexbuf in
	CRef (int_of_string ("0x" ^ String.sub s 3 (String.length s - 4))) }
  | '&'
      { raise(WF_error("The character '&' must be written as '&amp;'")) }
  | '\013' '\010'
      { CRef(-1) }
  | '\013'
      { CRef(-2) }
  | '\010'
      { CRef(-3) }
  | '\009'
      { CharData "\009" }
  | printable_character_except_amp_percent+
      { CharData (Lexing.lexeme lexbuf) }
  | eof
      { Eof }
  | _
      { raise Netconversion.Malformed_code }



and scan_characters = parse
  character*
    { () }
| eof 
    { () }
| _
    { raise Netconversion.Malformed_code }


and scan_xml_pi = parse
    name ws*
      { let s = Lexing.lexeme lexbuf in
	let j = get_name_end s 0 in
	Pro_name (String.sub s 0 j)
      }
  | "=" ws*
      { Pro_eq }
  | "'" character_except_apos* "'" ws+
      { let s = Lexing.lexeme lexbuf in
	let j = String.index_from s 1 '\'' in
	Pro_string (String.sub s 1 (j-1))
      }
  | "'"
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | '"' character_except_quot* '"' ws+
      { let s = Lexing.lexeme lexbuf in
	let j = String.index_from s 1 '"' in
	Pro_string (String.sub s 1 (j-1))
      }
  | '"'
      { raise (WF_error ("Cannot find the second quotation mark"))
      }
  | eof
      { Pro_eof }
  | character
      { (* prerr_endline (Lexing.lexeme lexbuf); *)
	raise (WF_error("Illegal token or character")) 
      }
  | _ 
      { raise Netconversion.Malformed_code }

and scan_only_xml_decl = parse
    "<?xml" ws+ pi_string "?>"
      { scan_pi (Lexing.lexeme lexbuf) (scan_xml_pi engine) }
  | ""
      { Eof }

and scan_for_crlf = parse
  | '\013' '\010'
      { CharData "\n" }
  | '\013'
      { CharData "\n" }
  | '\010'
      { CharData "\n" }
  | [^ '\010' '\013' ]+
      { CharData (Lexing.lexeme lexbuf) }
  | eof 
      { Eof }

and scan_content_comment = parse
    "-->"
      { Comment_end dummy_entity, Content }
  | "--"
      { raise (WF_error "Double hyphens are illegal inside comments") }
  | "-"
      { Comment_material "-", Content_comment }
  | character_except_minus+
      { Comment_material(Lexing.lexeme lexbuf), Content_comment }
  | eof
      { Eof, Content_comment }
  | _
      { raise Netconversion.Malformed_code }


(* In declarations, comments are always thrown away. *)

and scan_decl_comment = parse
    "-->"
      { Comment_end dummy_entity, Declaration }
  | "--"
      { raise (WF_error "Double hyphens are illegal inside comments") }
  | "-"
      { Comment_material "", Decl_comment }
  | character_except_minus+
      { Comment_material "", Decl_comment }
  | eof
      { Eof, Decl_comment }
  | _
      { raise Netconversion.Malformed_code }


and scan_document_comment = parse
    "-->"
      { Comment_end dummy_entity, Document }
  | "--"
      { raise (WF_error "Double hyphens are illegal inside comments") }
  | "-"
      { Comment_material "-", Document_comment }
  | character_except_minus+
      { Comment_material(Lexing.lexeme lexbuf), Document_comment }
  | eof
      { Eof, Document_comment }
  | _
      { raise Netconversion.Malformed_code }


and scan_name_string = parse
    name
      { Name (Lexing.lexeme lexbuf) }
  | ws+
      { Ignore }
  | nmtoken
      { Nametoken (Lexing.lexeme lexbuf) }
  | eof
      { Eof }
  | character
      { CharData (Lexing.lexeme lexbuf) }
  | _
      { raise Netconversion.Malformed_code }


and scan_ignored_section = parse
  | "<!["
      { tok_Conditional_begin__Ignored }
  | "]]>"
      { tok_Conditional_end__Ignored }
  | "<!--" comment_string "-->"
      { tok_Ignore__Ignored }
  | '"' character_except_quot* '"'
      { tok_Ignore__Ignored }
  | "'" character_except_apos* "'"
      { tok_Ignore__Ignored }
  | eof
      { tok_Eof__Ignored }
  | character_except_special+
      { tok_Ignore__Ignored }
  | "<"
      { tok_Ignore__Ignored }
  | "]"
      { tok_Ignore__Ignored }
  | "'"
      { tok_Ignore__Ignored }
  | "\""
      { tok_Ignore__Ignored }
  | _
      { raise Netconversion.Malformed_code }


(* ======================================================================
 * History:
 * 
 * $Log: pxp_wlex.mll,v $
 * Revision 1.5  2001/06/28 22:42:41  gerd
 * 	Comment tokens have entity_id
 *
 * Revision 1.4  2001/06/14 16:40:14  gerd
 * 	Updated
 *
 * Revision 1.3  2000/10/01 19:51:50  gerd
 * 	Optimizations.
 *
 * Revision 1.2  2000/09/21 21:33:16  gerd
 * 	Bugfix: Line counting within tags
 *
 * Revision 1.1  2000/09/17 00:14:06  gerd
 * 	Initial revision.
 *
 *)
