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
  | Comment of lexers (* the arg is the lexer to resume after comment *)
  | Ignored_section
  | Closed
  (* alternate version of Within_tag for event-based attribute parsing: *)
  | Tag_eb             (* Recognizes SQuote and DQuote *)
  | Tag_eb_att of bool (* The attribute value. bool: whether DQuote 
			* (true) or SQuote (false) delimit the value
			*)


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
  | Comment_begin of entity_id  (* <!-- *)
  | Comment_material of string  (* within a comment *)
  | Comment_end of entity_id    (* --> *)
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
  | LineEnd   of string               (* Line end (LF) *)
  | LineEnd_att of string             (* Line end in attributes (space) *)
  | Name      of string               (* name *)
  | Nametoken of string               (* nmtoken but not name *)
  | Attval    of string           (* attribute value; may contain entity refs *)
  | Attval_nl_normalized of string
  | Unparsed_string      of string    (* "data" or 'data' *)
  | SQuote                            (* Single quote *)
  | DQuote                            (* Double quote *)
  | ERef_att of string                (* &name; in attribute values *)
      
(* Notes:
 * - LineEnd s is converted to a single CharData "\n" if the entity manager
 *   reads from an external entity, and to CharData s otherwise. This LineEnd
 *   version is used when "\n" is the line separator (in most cases).
 * - LineEnd_att s is converted to a single CharData " " (space) if the
 *   entity manager reads from an external entity, and to CharData s otherwise.
 *   This LineEnd version is used when space is the line separator (i.e. in
 *   attribute values)
 *
 * - ERef n is substituted by the contents of the entity n by the entity 
 *   manager. The parser never sees this token, because it is already
 *   replaced by its replacement text.
 * - ERef_att n is not touched by the entity manager, and this token is
 *   seen by the parser. This is sometimes used when analyzing attribute
 *   values.
 * - PERef n is substituted by the contents of the entity, too, and the
 *   entity manager passed only the replacement text to the parser.
 *
 * - Attval s: The scanner passes such tokens to the entity manager when
 *   a complete attribute value is scanned at once, e.g. "abc" or 'def'.
 *   The entity manager leaves this token as it is if it comes from an
 *   external entity, but changes it to Attval_nl_normalized if it comes
 *   from an internal entity (because newline characters are already
 *   normalized).
 * - Attval_nl_normalized s: Generated by the entity manager if the
 *   newline characters of the attribute value are already normalized
 *   (always represented by "\n").
 * - Unparsed_string is only used in DTDs.
 *)

val string_of_lexers : lexers -> string

val string_of_tok : token -> string


(** The [lexer_factory] creates lexers for a certain character encoding.
 *)
class type lexer_factory =
object
  method encoding : Pxp_core_types.rep_encoding
    (** The (announced) character encoding of the scanned strings *)

  method open_source : Pxp_reader.lexer_source -> lexer_obj
    (** Open a source *)

  method open_string : string -> lexer_obj
    (** Open a string as source *)

  method open_string_inplace : string -> lexer_obj
    (** Open a string as source.
     * The string is physically used as lexical buffer (no copy is made)
     *)
end

(** A [lexer_obj] scans from a certain [lexer_source]. The lexbuf is an 
 * internal value of the [lexer_obj].
 *)
and lexer_obj =
object
  method factory : lexer_factory
    (** The [lexer_factory] that created this [lexer_obj] *)

  method encoding : Pxp_core_types.rep_encoding
    (** The character encoding of the scanned strings *)

  method open_source : Pxp_reader.lexer_source -> unit
    (** Drop the current source, and open another source *)

  method open_string : string -> unit
    (** Drop the current source, and open the string as next source *)

  method open_string_inplace : string -> unit
    (** Drop the current source, and open the string as next source.
     * The string is physically used as lexical buffer (no copy is made)
     *)

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
    (** The lexeme scanned last, encoded as [encoding] *)

  method lexeme_len : int
    (** = String.length lexeme, i.e. number of bytes of the lexeme,
     * not the number of characters
     *)
end
