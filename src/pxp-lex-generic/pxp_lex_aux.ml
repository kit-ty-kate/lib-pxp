(* $Id: pxp_lex_aux.ml,v 1.1 2001/06/14 14:15:20 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

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

  (* functions: *)

  let get_name_end s k =
    (* Get the index of the end+1 of the name beginning at position k *)
    let l = String.length s in
    let rec find j =
      if j < l then
	match s.[j] with
	  | ('\009'|'\010'|'\013'|'\032') -> j
	  |_                              -> find (j+1)
      else
	l
    in
    find k

  let get_ws_end s k =
    let l =  String.length s in
    let rec find j =
      if j < l then
	match s.[j] with
	    (' '|'\t'|'\r'|'\n') -> find (j+1)
	  | _                    -> j
      else
	l
    in
    find k

  let scan_pi pi xml_scanner =
    let s = String.sub pi 2 (String.length pi - 4) in
            (* the PI without the leading "<?" and the trailing "?>" *)
    let xml_lexbuf = Lexing.from_string (s ^ " ") in
      (* Add space because the lexer expects whitespace after every
       * clause; by adding a space there is always whitespace at the 
       * end of the string.
       *)

    (* The first word of a PI must be a name: Extract it. *)

    let s_name, s_len =
      match xml_scanner xml_lexbuf with
	  Pro_name n -> 
	    let ltok = String.length (Lexing.lexeme xml_lexbuf) in
	    if String.length n = ltok then
              (* No whitespace after the name *)
	      raise (WF_error ("Bad processing instruction"));
	    n, ltok
	| _ -> raise (WF_error ("Bad processing instruction"))
    in

    (* Note: s_len is the length of s_name + the whitespace following s_name *)

    match s_name with
	"xml" -> begin
	  (* It is a <?xml ...?> PI: Get the other tokens *)
	  let rec collect () =
	    let t = xml_scanner xml_lexbuf in
	    (* prerr_endline (string_of_int (Lexing.lexeme_end xml_lexbuf)); *)
	    if t = Pro_eof then
	      []
	    else
	      t :: collect()
	  in
	  PI_xml (collect())
	end
      | _ -> 
	  let len_param = String.length s - s_len in
	  (* It is possible that len_param = -1 *)
	  if len_param >= 1 then
	    PI(s_name, String.sub s s_len len_param)
	  else
	    PI(s_name, "")

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lex_aux.ml,v $
 * Revision 1.1  2001/06/14 14:15:20  gerd
 * 	Initial revision
 *
 * 
 *)
