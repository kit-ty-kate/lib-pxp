(* $Id$
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
  | Comment of lexers
  | Ignored_section
  | Closed
  | Tag_eb
  | Tag_eb_att of bool


let rec string_of_lexers =
  function
      Document          -> "Document"
    | Document_type     -> "Document_type"
    | Content           -> "Content"
    | Within_tag        -> "Within_tag"
    | Within_tag_entry  -> "Within_tag_entry"
    | Declaration       -> "Declaration"
    | Comment lexid     -> ("Comment/" ^ string_of_lexers lexid)
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

class type lexer_factory =
object
  method encoding : Pxp_core_types.rep_encoding
  method open_source : Pxp_reader.lexer_source -> lexer_obj
  method open_string : string -> lexer_obj
  method open_string_inplace : string -> lexer_obj
end

and lexer_obj =
object
  method factory : lexer_factory
  method encoding : Pxp_core_types.rep_encoding
  method open_source : Pxp_reader.lexer_source -> unit
  method open_string : string -> unit
  method open_string_inplace : string -> unit

  method scan_document        : unit -> (token * lexers)
  method scan_content         : unit -> (token * lexers)
  method scan_within_tag      : unit -> (token * lexers)
  method scan_document_type   : unit -> (token * lexers)
  method scan_declaration     : unit -> (token * lexers)
  method scan_comment         : unit -> lexers -> (token * lexers)
  method scan_ignored_section : unit -> (token * lexers)
  method detect_xml_pi        : unit -> bool
  method scan_xml_pi          : unit -> prolog_token
  method scan_pi_string       : unit -> string option
  method scan_dtd_string      : unit -> token
  method scan_content_string  : unit -> token
  method scan_name_string     : unit -> token
  method scan_for_crlf        : unit -> token
  method scan_characters      : unit -> unit
  method scan_character       : unit -> unit
  method scan_tag_eb          : unit -> (token * lexers)
  method scan_tag_eb_att      : unit -> bool -> (token * lexers)

  method lexeme : string
  method lexeme_len : int
end
