(* $Id: pxp_aux.ml,v 1.2 2000/07/08 22:15:45 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 * Some auxiliary functions 
 *)

(**********************************************************************)
(* Lexing *)


open Pxp_types
open Pxp_lexer_types
open Pxp_lexers

let character enc warner k =
  assert (k>=0);
  if (k >= 0xd800 & k < 0xe000) or (k >= 0xfffe & k <= 0xffff) or k > 0x10ffff
     or (k < 8) or (k = 11) or (k = 12) or (k >= 14 & k <= 31)
  then
    raise (Illegal_character 0);

  try
    Pxp_encoding.makechar enc k
  with
      Not_found ->
	warner # warn ("Code point cannot be represented in internal encoding: "
		       ^ string_of_int k);
	""
;;


let check_name warner name =
  (* produces a warning for names beginning with "xml". *)
  if String.length name >= 3 then begin
    match String.sub name 0 3 with
	("xml" | "xmL" | "xMl" | "xML" | "Xml" | "XmL" | "XMl" | "XML") ->
	  warner # warn ("Name is reserved for future extensions: " ^ name)
      | _ ->
	  ()
  end
;;


let tokens_of_content_string lexerset s =
  (* tokenizes general entities and character entities *)
  let lexbuf = Lexing.from_string s in
  let rec next_token () =
    match lexerset.scan_content_string lexbuf with
	Eof -> []
      | tok -> tok :: next_token()
  in
  next_token()
;;


let rec expand_attvalue_with_rec_check lexerset dtd s warner entities norm_crlf =
  (* recursively expands general entities and character entities;
   * checks "standalone" document declaration;
   * normalizes whitespace
   *)
  let toklist = tokens_of_content_string lexerset s in
  let rec expand tl =
    match tl with
	[] -> ""
      | ERef n :: tl' ->
	  if List.mem n entities then
	    raise(WF_error("Recursive reference to general entity `" ^ n ^ "'"));
	  let en, extdecl = dtd # gen_entity n in
	  if dtd # standalone_declaration && extdecl then
	    raise(Validation_error("Reference to entity `" ^ n ^ 
				   "' violates standalone declaration"));
	  let rtext, rtext_contains_ext_refs = en # replacement_text in
	  if rtext_contains_ext_refs then
	    raise(Validation_error("Found reference to external entity in attribute value"));
	  expand_attvalue_with_rec_check 
	    lexerset dtd rtext warner (n :: entities) false    ^    expand tl'
      | CRef(-1) :: tl' ->
	  if norm_crlf then
	    " " ^ expand tl'
	  else
	    "  " ^ expand tl'
      | CRef n :: tl' ->
	  character lexerset.lex_encoding warner n ^ expand tl'
      | CharData "<" :: tl' ->
	  raise 
	    (WF_error
	       ("Attribute value contains character '<' literally"))
      | CharData x :: tl' ->
	  x ^ expand tl'
      | _ -> assert false
  in
  expand toklist
;;


let expand_attvalue lexerset dtd s warner norm_crlf =
  (* norm_crlf: whether the sequence CRLF is recognized as one character or
   * not (i.e. two characters)
   *)
  expand_attvalue_with_rec_check lexerset dtd s warner [] norm_crlf
;;


let count_lines s =
  (* returns number of lines in s, number of columns of the last line *)
  let l = String.length s in

  let rec count n k no_cr no_lf =
    let next_cr = 
      if no_cr then
	(-1)
      else
	try String.index_from s k '\013' with Not_found -> (-1) in
    let next_lf = 
      if no_lf then
	(-1)
      else
	try String.index_from s k '\010' with Not_found -> (-1) in
    if next_cr >= 0 & (next_lf < 0 or next_cr < next_lf) then begin
      if next_cr+1 < l & s.[next_cr+1] = '\010' then
	count (n+1) (next_cr+2) false (next_lf < 0)
      else
	count (n+1) (next_cr+1) false (next_lf < 0)
    end
    else if next_lf >= 0 then begin
      count (n+1) (next_lf+1) (next_cr < 0) false
    end
    else
      n, (l - k)

  in
  count 0 0 false false
;;


let decode_xml_pi pl =
  (* 'pl' must consist of name="value" or name='value' pairs which are returned
   * as list of pairs.
   * The "value" is returned as it is; no substitution of &entities; happens.
   *)
  let rec decode pl =
    match pl with
	Pro_name name :: Pro_eq :: Pro_string value :: pl' ->
	  (name, value) :: decode pl'
      | [] ->
	  []
      | _ ->
	  raise (WF_error("Bad XML processing instruction"))
  in
  decode pl
;;


let decode_doc_xml_pi pl =
  match pl with
      [ "version", v ]                                  -> (v, None, None)
    | [ "version", v; "encoding", e ]                   -> (v, Some e, None)
    | [ "version", v; "standalone", s ]                 -> (v, None, Some s)
    | [ "version", v; "encoding", e; "standalone", s ]  -> (v, Some e, Some s)
    | _ ->
	raise(WF_error("Bad XML declaration"))
;;


let check_text_xml_pi pl =
  match pl with
    | [ "version", v; "encoding", e ] -> ()
    | [ "encoding", e ]  -> ()
    | _ ->
	raise(WF_error("Bad XML declaration"))
;;


let check_version_num s =
  let l = String.length s in
  for i = 0 to l - 1 do
    match s.[i] with
	('a'..'z'|'A'..'Z'|'0'..'9'|
	 '-'|'_'|'.'|':') -> ()
      | _ ->
	  raise(WF_error("Bad XML version string"))
  done
;;


let check_public_id s =
  let l = String.length s in
  for i = 0 to l - 1 do
    match s.[i] with
	(' '|'\013'|'\010'|'a'..'z'|'A'..'Z'|'0'..'9'|
	 '-'|'\''|'('|')'|'+'|','|'.'|'/'|':'|'='|'?'|
	 ';'|'!'|'*'|'#'|'@'|'$'|'_'|'%') -> ()
      | _ ->
	  raise(WF_error("Illegal character in PUBLIC identifier"))
  done
;;


(**********************************************************************)
(* list functions *)


let rec check_dups l =
  match l with
      [] -> false
    | c :: l' -> 
	if List.mem c l' then true else check_dups l'
;;


let rec count pred l =
  match l with
      [] -> 0
    | x :: l' -> 
	if pred x then  1 + (count pred l') else count pred l'
;;


(**********************************************************************)
(* attributes *)

let check_attribute_value_lexically lexerset x t v =
  (* raises x if the attribute value v does not match the lexical rules
   * for attribute type t:
   * - t = A_id: v must be a <name>
   * - t = A_idref: v must match <name>
   * - t = A_idrefs: v must match <names>
   * - t = A_entity: v must match <name>
   * - t = A_entities: v must match <names>
   * - t = A_nmtoken: v must match <nmtoken>
   * - t = A_nmtokens: v must match <nmtokens>
   * - t = A_notation _: v must match <name>
   * - t = A_enum _: v must match <nmtoken>
   * - t = A_cdata: not checked
   *)
  let lexbuf = Lexing.from_string v in
  let rec get_name_list() =
    match lexerset.scan_name_string lexbuf with
	Eof    -> []
      | Ignore -> get_name_list()
      | tok    -> tok :: get_name_list()
  in
  let l = get_name_list() in
  match t with
      (A_id | A_idref | A_entity | A_notation _) ->
	begin match l with
	    [ Name n ] -> ()
	  | _          -> raise (Lazy.force x)
	end
    | (A_idrefs | A_entities) ->
	if List.exists (fun tok -> 
			  match tok with
			      Name _ -> false
			    | _ -> true) l then
	  raise (Lazy.force x)
    | (A_nmtoken | A_enum _) ->
	begin match l with
	    [ Name n ]      -> ()
	  | [ Nametoken n ] -> ()
	  | _               -> raise (Lazy.force x)
	end
    | A_nmtokens ->
	if List.exists (fun tok -> 
			  match tok with
			      Name _ -> false
			    | Nametoken _ -> false
			    | _ -> true
		       ) l then
	  raise (Lazy.force x)
    | _ -> ()
;;


let split_attribute_value lexerset v =
  (* splits 'v' into a list of names or nmtokens. The white space separating
   * the names/nmtokens in 'v' is suppressed and not returned.
   *)
  let lexbuf = Lexing.from_string v in
  let rec get_name_list() =
    match lexerset.scan_name_string lexbuf with
	Eof         -> []
      | Ignore      -> get_name_list()
      | Name s      -> s :: get_name_list()
      | Nametoken s -> s :: get_name_list()
      | _           -> raise(Validation_error("Illegal attribute value"))
  in
  get_name_list()
;;


let normalize_line_separators lexerset s =
  let lexbuf = Lexing.from_string s in
  let rec get_string() =
    match lexerset.scan_for_crlf lexbuf with
	Eof        -> ""
      | CharData s -> s ^ get_string()
      | _          -> assert false
  in
  get_string()
;;


let value_of_attribute lexerset dtd n atype v =
  (* The attribute with name 'n', type 'atype' and string value 'v' is
   * decomposed, and the att_value is returned:
   * - It is checked whether 'v' conforms to the lexical rules for attributes
   *   of type 'atype'
   * - If 'atype <> A_cdata', leading and trailing spaces are removed from 'v'.
   * - If 'atype = A_notation d', it is checked if 'v' matches one of the
   *   notation names contained in d.
   * - If 'atype = A_enum d', it is checked whether 'v' matches one of the
   *   tokens from d
   * - If 'atype' refers to a "single-value" type, the value is retured as
   *   Value u, where u is the normalized value. If 'atype' refers to a 
   *   "list" type, the value if returned as Valuelist l, where l contains
   *   the tokens.
   *
   * Note that this function does not implement all normalization rules.
   * It is expected that the string passed as 'v' is already preprocessed;
   * i.e. character and entity references are resolved, and the substitution
   * of white space characters by space characters has already been performed.
   * If these requirements are met, the value returned by this function
   * will be perfectly normalized.
   *
   * Further checks:
   * - ENTITY and ENTITIES values: It is checked whether there is an
   *   unparsed general entity
   * [ Other checks planned: ID, IDREF, IDREFS but not yet implemented ]
   *)

  let lexical_error() =
    lazy (raise(Validation_error("Attribute `" ^ n ^ "' is lexically malformed"))) in

  let remove_leading_and_trailing_spaces u =
    (* Precondition: 'u' matches <name> or <nmtoken> *)
    match split_attribute_value lexerset u with
	[ u' ] -> u'
      | _      -> assert false
  in

  let check_ndata_entity u =
    let en, extdecl = dtd # gen_entity u in  (* or Validation_error *)
    if not (en # is_ndata) then
      raise(Validation_error("Reference to entity `" ^ u ^ 
			     "': NDATA entity expected"));
    if dtd # standalone_declaration && extdecl then
      raise(Validation_error("Reference to entity `" ^ u ^ 
			     "' violates standalone declaration"));
  in

  match atype with
      A_cdata ->
	Value v

    | (A_id | A_idref | A_nmtoken) ->
	check_attribute_value_lexically lexerset (lexical_error()) atype v;
	Value (remove_leading_and_trailing_spaces v)
    | A_entity ->
	check_attribute_value_lexically lexerset (lexical_error()) atype v;
	let v' = remove_leading_and_trailing_spaces v in
	check_ndata_entity v';
	Value v'

    | (A_idrefs | A_nmtokens) ->
	check_attribute_value_lexically lexerset (lexical_error()) atype v;
	Valuelist (split_attribute_value lexerset v)

    | A_entities ->
	check_attribute_value_lexically lexerset (lexical_error()) atype v;
	let l = split_attribute_value lexerset v in
	List.iter check_ndata_entity l;
	Valuelist l

    | A_notation nl ->
	check_attribute_value_lexically lexerset (lexical_error()) atype v;
	let v' = remove_leading_and_trailing_spaces v in
	if not (List.mem v' nl) then
	  raise(Validation_error
		  ("Attribute `" ^ n ^ 
		   "' does not match one of the declared notation names"));
	Value v'

    | A_enum enuml ->
	check_attribute_value_lexically lexerset (lexical_error()) atype v;
	let v' = remove_leading_and_trailing_spaces v in
	if not (List.mem v' enuml) then
	  raise(Validation_error
		  ("Attribute `" ^ n ^ 
		   "' does not match one of the declared enumerator tokens"));
	Value v'
;;


let normalization_changes_value lexerset atype v =
  (* Returns true if:
   * - 'atype' is a "single-value" type, and the normalization of the string
   *   value 'v' of this type discards leading and/or trailing spaces
   * - 'atype' is a "list" type, and the normalization of the string value
   *   'v' of this type discards leading and/or trailing spaces, or spaces
   *   separating the tokens of the list (i.e. the normal form is that
   *   the tokens are separated by exactly one space character).
   *
   * Note: It is assumed that TABs, CRs, and LFs in 'v' are already converted
   * to spaces.
   *)

  match atype with
      A_cdata -> 
	false

    | (A_id | A_idref | A_entity | A_nmtoken | A_notation _ | A_enum _) ->
	(* Return 'true' if the first or last character is a space.
	 * The following check works for both ISO-8859-1 and UTF-8.
	 *)
	v <> "" && (v.[0] = ' ' || v.[String.length v - 1] = ' ')

    | (A_idrefs | A_entities | A_nmtokens) ->
	(* Split the list, and concatenate the tokens as required by
	 * the normal form. Return 'true' if this operation results in 
	 * a different string than 'v'.
	 * This check works for both ISO-8859-1 and UTF-8.
	 *)
	let l = split_attribute_value lexerset v in
	let v' = String.concat " " l in
	v <> v'
;;


(**********************************************************************)

let write_data_string os content =
  let i = ref 0 in
  for k = 0 to String.length content - 1 do
    match content.[k] with
	('&' | '<' | '>' | '"' | '%') as c ->
	  if !i < k then
	    write os content !i (k - !i);
	  begin match c with
	      '&' -> write os "&amp;"  0 5
	    | '<' -> write os "&lt;"   0 4
	    | '>' -> write os "&gt;"   0 4
	    | '"' -> write os "&quot;" 0 6
	    | '%' -> write os "&#37;"  0 5  (* reserved in DTDs *)
	    | _   -> assert false
	  end;
	  i := k+1
      | _ -> ()
  done;
  if !i < String.length content then
    write os content !i (String.length content - !i)
;;


(* ======================================================================
 * History:
 * 
 * $Log: pxp_aux.ml,v $
 * Revision 1.2  2000/07/08 22:15:45  gerd
 * 	[Merging 0.2.10:] write_data_string: The character '%' is special, too.
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
 * Old logs from markup_aux.ml:
 *
 * Revision 1.12  2000/05/27 19:08:30  gerd
 * 	Added functionality to check standalone declaration:
 *
 * 	expand_attvalue: Checks whether included entities violate the
 * stand-alone declaration.
 *
 * 	value_of_attribute: Checks whether ENTITY/ENTITIES values violate
 * this declaration. (Furthermore, it is checked whether the NDATA
 * entity exists - this has been forgotten in previous versions.)
 *
 * 	value_of_attribute/check_attribute_value_lexically: improved.
 *
 * 	New function normalization_changes_value: helps detecting
 * one case which violates the standalone declaration.
 *
 * Revision 1.11  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.10  2000/05/01 20:41:56  gerd
 * 	New function write_data_string.
 *
 * Revision 1.9  2000/04/30 18:11:31  gerd
 * 	New function normalize_line_separators.
 * 	In function expand_attvalue: New argument norm_crlf. If the attvalue
 * is read directly from a file, the sequence CR LF must be converted to a
 * single space. If the attvalue is read from a replacement text, CR LF has
 * already converted to a single LF, and CR LF, if still occurring, must be
 * converted to two spaces. The caller can indicate the case by passing
 * true/false as norm_crlf.
 *
 * Revision 1.8  1999/09/01 22:51:07  gerd
 * 	Added functions.
 * 	'character' raises Illegal_character if characters are found that
 * do not match the production Char.
 *
 * Revision 1.7  1999/09/01 16:17:37  gerd
 * 	Added function 'check_name'.
 *
 * Revision 1.6  1999/08/15 20:33:19  gerd
 * 	Added: a function that checks public identifiers. Only certain
 * characters may occur in these identifiers.
 * 	Control characters are rejected by the "character" function.
 * 	Bugfix: recursive entity references are detected in attribute
 * expansion
 *
 * Revision 1.5  1999/08/15 02:18:02  gerd
 * 	That '<' is not allowed in attribute values, is a violation
 * of well-formedness, not of the validity; so WF_error is raised.
 *
 * Revision 1.4  1999/08/15 00:20:37  gerd
 * 	When expanding attribute values, references to parameter
 * entities are now resolved by the method "replacement_text" which
 * has an additional return value, and no longer by "attlist_replacement_text".
 * The new return value indicates whether references to external entities
 * have been resolved (directly or indirectly); this is allowed at some
 * locations but not in attribute values.
 *
 * Revision 1.3  1999/08/14 22:05:53  gerd
 * 	Several functions have now a "warner" as argument which is
 * an object with a "warn" method. This is used to warn about characters
 * that cannot be represented in the Latin 1 alphabet.
 *
 * Revision 1.2  1999/08/10 21:35:06  gerd
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
 * Revision 1.1  1999/08/10 00:35:50  gerd
 * 	Initial revision.
 *
 * 
 *)
