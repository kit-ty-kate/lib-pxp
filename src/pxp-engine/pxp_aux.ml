(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 * Some auxiliary functions
 *)

(**********************************************************************)
(* Lexing *)


open Pxp_core_types
open Pxp_lexer_types
open Pxp_lexers
open Pxp_lib
open Netconversion

module HashedString = struct
  type t = string
  let equal (a:string) (b:string) = (a = b)
  let hash = Hashtbl.hash
end;;

module Str_hashtbl = Hashtbl.Make(HashedString);;

module StringOrd = struct
  type t = string
  let compare = (compare : string -> string -> int)
end;;

module StringMap = Map.Make(StringOrd);;
  (* 'a StringMap.t: the type of maps (dictionaries) from string to 'a *)

let character ?swarner enc warner k =
  assert (k>=0);
  if (k >= 0xd800 & k < 0xe000) or (k >= 0xfffe & k <= 0xffff) or k > 0x10ffff
     or (k < 8) or (k = 11) or (k = 12) or (k >= 14 & k <= 31)
  then
    raise (WF_error("Code point " ^ string_of_int k ^
		    " outside the accepted range of code points"));

  try
    makechar (enc : rep_encoding :> encoding) k
  with
      Not_found ->
	warn swarner warner (`W_code_point_cannot_be_represented k);
	""
;;


let check_name ?swarner warner name =
  (* produces a warning for names beginning with "xml". *)
  if String.length name >= 3 then begin
    match String.sub name 0 3 with
	("xml" | "xmL" | "xMl" | "xML" | "Xml" | "XmL" | "XMl" | "XML") ->
	  warn swarner warner (`W_name_is_reserved_for_extensions name)
      | _ ->
	  ()
  end
;;


let tokens_of_content_string lfactory s =
  (* tokenizes general entities and character entities *)
  let lexobj = lfactory # open_string_inplace s in
  let scan   = lexobj # scan_content_string in
  let rec next_token () =
    match scan() with
	Eof        -> []
      | CharData _ -> let tok = CharData lexobj#lexeme in
	              tok :: next_token()
      | tok        -> tok :: next_token()
  in
  next_token()
;;


exception Quick_exit;;

let rec expand_attvalue_with_rec_check (lexobj : lexer_obj) l dtd entities norm_crlf =
  (* recursively expands general entities and character entities;
   * checks "standalone" document declaration;
   * normalizes whitespace
   *
   * Exception: Quick_exit: the expanded value is equal to s
   *)
  match lexobj # scan_content_string () with
      Eof -> []
    | ERef n ->
	if List.mem n entities then
	  raise(WF_error("Recursive reference to general entity `" ^ n ^ "'"));
	let en, extdecl = dtd # gen_entity n in
	if dtd # standalone_declaration && extdecl then
	  raise(Validation_error("Reference to entity `" ^ n ^
				 "' violates standalone declaration"));
	let rtext, rtext_contains_ext_refs = en # replacement_text in
	if rtext_contains_ext_refs then
	  raise(Validation_error("Found reference to external entity in attribute value"));
	let l' =
	  try
	    expand_attvalue_with_rec_check
	      (lexobj # factory # open_string_inplace rtext)
	      (String.length rtext)
	      dtd (n :: entities) false
	  with
	      Quick_exit -> [rtext]
	  in
	  l' @ expand_attvalue_with_rec_check
	         lexobj l dtd entities norm_crlf
    | CRef(-1) ->
	if norm_crlf then begin
	  " " :: expand_attvalue_with_rec_check
	             lexobj l dtd entities norm_crlf
	end
	else begin
	  "  " :: expand_attvalue_with_rec_check
 	              lexobj l dtd entities norm_crlf
	end
    | CRef n ->
	(character ?swarner:dtd#swarner lexobj#encoding dtd#warner n) ::
	expand_attvalue_with_rec_check
 	    lexobj l dtd entities norm_crlf
    | CharData _  ->
	let ll = lexobj # lexeme_strlen in
	if ll > 1 && ll = l then
	   raise Quick_exit
	else
	  let x = lexobj # lexeme in
	  if x.[0] = '<' then  (* or better: x = "<", ensured by the lexer *)
	    raise
	      (WF_error
		 ("Attribute value contains character '<' literally"));
	  x :: expand_attvalue_with_rec_check
 	           lexobj l dtd entities norm_crlf
    | _ -> assert false
;;


let expand_attvalue (lexobj : lexer_obj) dtd s norm_crlf =
  (* norm_crlf: whether the sequence CRLF is recognized as one character or
   * not (i.e. two characters).
   * lexbuf: must result from a previous Lexing.from_string
   *)
  (* print_string ("expand_attvalue \"" ^ s ^ "\" = "); *)
  try
    lexobj # open_string_inplace s;
    let l =
      expand_attvalue_with_rec_check
	lexobj (String.length s) dtd [] norm_crlf in
    let s' =
      String.concat "" l in
    (* print_string ("\"" ^ s' ^ "\"\n"); *)
    s'
  with
      Quick_exit ->
	(* print_string ("\"" ^ s ^ "\"\n"); *)
	s
;;


type linecount =
    { mutable lines : int;
      mutable columns : int;
    }
;;


let rec count_lines_aux lc s n k =
  let next_cr_or_lf = crlf_index_from s k in
  if next_cr_or_lf >= 0 then begin
    match s.[next_cr_or_lf] with
	'\010' ->
	  count_lines_aux lc s (n+1) (next_cr_or_lf+1)
      | '\013' ->
	  let l = String.length s in
	  if (next_cr_or_lf+1 < l && s.[next_cr_or_lf+1] = '\010') then
	    count_lines_aux lc s (n+1) (next_cr_or_lf+2)
	  else
	    count_lines_aux lc s (n+1) (next_cr_or_lf+1)
      | _ ->
	  assert false
  end
  else begin
    lc.lines <- n;
    lc.columns <- String.length s - k;
  end
;;

let count_lines lc s =
  (* modifies lc: number of lines in s, number of columns of the last line *)
  count_lines_aux lc s 0 0
;;


let tokens_of_xml_pi (lfactory : lexer_factory) s =
  let lexobj = lfactory # open_string_inplace (s ^ " ") in
  let scan = lexobj # scan_xml_pi in
  let rec collect () =
    let t = scan() in
    match t with
	Pro_eof -> []
      | _       -> t :: collect()
  in
  collect()
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

let check_attribute_value_lexically (lfactory:lexer_factory) x t v =
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
  let lexobj = lfactory # open_string_inplace v in
  let scan = lexobj#scan_name_string in
  let rec get_name_list() =
    match scan() with
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


let split_attribute_value (lfactory:lexer_factory) v =
  (* splits 'v' into a list of names or nmtokens. The white space separating
   * the names/nmtokens in 'v' is suppressed and not returned.
   *)
  (* print_string ("split_attribute_value \"" ^ v ^ "\" = "); *)
  let lexobj = lfactory # open_string_inplace v in
  let scan = lexobj # scan_name_string in
  let rec get_name_list() =
    match scan() with
	Eof         -> []
      | Ignore      -> get_name_list()
      | Name s      -> s :: get_name_list()
      | Nametoken s -> s :: get_name_list()
      | _           -> raise(Validation_error("Illegal attribute value"))
  in
  let l = get_name_list() in
  (* print_string "[";
     print_string (String.concat "," l);
     print_string "]\n";
  *)
  l
;;


let rev_concat l =
  (* = String.concat "" (List.rev l) *)
  let k = ref 0 in
  List.iter 
    (fun s -> k := !k + String.length s) 
    l;
  let r = String.create !k in
  List.iter
    (fun s -> 
       let n = String.length s in
       k := !k - n;
       String.(*unsafe_*)blit s 0 r !k n;
    )
    l;
  assert(!k = 0);
  r
;;


let normalize_line_separators (lfactory:lexer_factory) s =
  (* Note: Returns [s] if [s] does not contain LFs *)
  let lexobj = lfactory # open_string_inplace s in
  let scan = lexobj # scan_for_crlf in
  let rec get_string l =
    match scan() with
	Eof        -> l
      | CharData s -> get_string (s::l)
      | _          -> assert false
  in
  match get_string [] with
      []  -> ""
    | [s] -> s
    | l   -> rev_concat l
;;


let value_of_attribute_aux (lfactory:lexer_factory) dtd n atype v =
  (* See value_of_attribute below. *)

  let lexical_error() =
    lazy (raise(Validation_error("Attribute `" ^ n ^ "' is lexically malformed"))) in

  let remove_leading_and_trailing_spaces u =
    (* Precondition: 'u' matches <name> or <nmtoken> *)
    match split_attribute_value lfactory u with
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
	check_attribute_value_lexically lfactory (lexical_error()) atype v;
	Value (remove_leading_and_trailing_spaces v)
    | A_entity ->
	check_attribute_value_lexically lfactory (lexical_error()) atype v;
	let v' = remove_leading_and_trailing_spaces v in
	check_ndata_entity v';
	Value v'

    | (A_idrefs | A_nmtokens) ->
	check_attribute_value_lexically lfactory (lexical_error()) atype v;
	Valuelist (split_attribute_value lfactory v)

    | A_entities ->
	check_attribute_value_lexically lfactory (lexical_error()) atype v;
	let l = split_attribute_value lfactory v in
	List.iter check_ndata_entity l;
	Valuelist l

    | A_notation nl ->
	check_attribute_value_lexically lfactory (lexical_error()) atype v;
	let v' = remove_leading_and_trailing_spaces v in
	if not (List.mem v' nl) then
	  raise(Validation_error
		  ("Attribute `" ^ n ^
		   "' does not match one of the declared notation names"));
	Value v'

    | A_enum enuml ->
	check_attribute_value_lexically lfactory (lexical_error()) atype v;
	let v' = remove_leading_and_trailing_spaces v in
	if not (List.mem v' enuml) then
	  raise(Validation_error
		  ("Attribute `" ^ n ^
		   "' does not match one of the declared enumerator tokens"));
	Value v'
;;


let value_of_attribute (lfactory:lexer_factory) dtd n atype v =
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

  if atype = A_cdata then
    (* The most frequent case *)
    Value v
  else
    value_of_attribute_aux lfactory dtd n atype v
      (* Note that value_of_attribute_aux allocates memory for the local
       * functions even if they are not called at all.
       *)
;;

let check_value_of_attribute (lfactory:lexer_factory) dtd n atype av =
  (* This function checks whether the decomposed attribute value av
   * matches the attribute type, i.e. it checks whether av is the
   * result of the above function value_of_attribute for some
   * unprocessed value v.
   * If the check fails, a validation error is indicated.
   *)

  let lexical_error() =
    lazy (raise(Validation_error("Attribute `" ^ n ^ "' is lexically malformed"))) in

  let unexpected_valuelist() =
    raise(Validation_error("A list value cannot be assigned to attribute `" ^ 
			   n ^ "'"))
  in

  let unexpected_value() =
    raise(Validation_error("A non-list value cannot be assigned to attribute `" ^ 
			   n ^ "'"))
  in

  let no_leading_and_trailing_spaces u =
    if u <> "" then begin
      let is_whitespace c =
	c = ' ' || c = '\t' || c = '\r' || c = '\n' in
      if (is_whitespace u.[0]) || (is_whitespace u.[ String.length u - 1 ])
      then
	raise(Validation_error("Attribute `" ^ n ^ "' has leading or trailing whitespace"))
    end
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
	(match av with
	   | Valuelist _ -> unexpected_valuelist()
	   | _  -> ()
	)
    | (A_id | A_idref | A_nmtoken) ->
	(match av with
	   | Valuelist _ -> unexpected_valuelist()
	   | Value v ->
	       check_attribute_value_lexically lfactory (lexical_error()) atype v;
	       no_leading_and_trailing_spaces v
	   | _ -> ()
	)
    | A_entity ->
	(match av with
	   | Valuelist _ -> unexpected_valuelist()
	   | Value v ->
	       check_attribute_value_lexically lfactory (lexical_error()) atype v;
	       no_leading_and_trailing_spaces v;
	       check_ndata_entity v
	   | _ -> ()
	)
    | (A_idrefs | A_nmtokens) ->
	(match av with
	   | Value _ -> unexpected_value()
	   | Valuelist l ->
	       List.iter no_leading_and_trailing_spaces l;
	       let subst_type = 
		 if atype = A_idrefs then A_id else A_nmtoken in
	       List.iter
		 (check_attribute_value_lexically 
		    lfactory (lexical_error()) subst_type) 
		 l
	   | _ -> ()
	)
    | A_entities ->
	(match av with
	   | Value _ -> unexpected_value()
	   | Valuelist l ->
	       List.iter no_leading_and_trailing_spaces l;
	       List.iter
		 (check_attribute_value_lexically 
		    lfactory (lexical_error()) A_entity) 
		 l;
	       List.iter check_ndata_entity l
	   | _ -> ()
	)
    | A_notation nl ->
	(match av with
	   | Valuelist _ -> unexpected_valuelist()
	   | Value v ->
	       check_attribute_value_lexically lfactory (lexical_error()) atype v;
	       no_leading_and_trailing_spaces v;
	       if not (List.mem v nl) then
		 raise(Validation_error
			 ("Attribute `" ^ n ^
			  "' does not match one of the declared notation names"));
	   | _ -> ()
	)
    | A_enum enuml ->
	(match av with
	   | Valuelist _ -> unexpected_valuelist()
	   | Value v ->
	       check_attribute_value_lexically lfactory (lexical_error()) atype v;
	       no_leading_and_trailing_spaces v;
	       if not (List.mem v enuml) then
		 raise(Validation_error
			 ("Attribute `" ^ n ^
			  "' does not match one of the declared enumerator tokens"));
	   | _ -> ()
	)
;;

let normalization_changes_value (lfactory:lexer_factory) atype v =
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
	let l = split_attribute_value lfactory v in
	let v' = String.concat " " l in
	v <> v'
;;


(**********************************************************************)

let re_multi_ws = Netstring_str.regexp "[ \010\013\009]+";;
let re_beg_ws =   Netstring_str.regexp "^[ \010\013\009]+";;
let re_end_ws =   Netstring_str.regexp "[ \010\013\009]+$";;

let normalize_public_id s =
  (* Replaces multiple occurrences of white space characters by a single
   * &#32;, and removes leading and trailing white space.
   *)
  let s1 = Netstring_str.global_replace
	     re_multi_ws
	     " "
	     s in

  let s2 = Netstring_str.global_replace
	     re_beg_ws
	     ""
	     s1 in

  let s3 = Netstring_str.global_replace
	     re_end_ws
	     ""
	     s2 in
  s3
;;


(**********************************************************************)

let namespace_split name =
  (* Searches ':' in name and returns (prefix, localname).
   * If there is no ':', prefix = "".
   *)
  try
    let n = String.index name ':' in   (* may raise Not_found *)
    let prefix = String.sub name 0 n in
    let localname = String.sub name (n+1) (String.length name - n - 1) 
    in
    (prefix, localname)
  with
      Not_found -> ("", name)
;;


let extract_prefix name =
  (* Returns the prefix of a name, or "" if there is no colon *)
  try
    let n = String.index name ':' in   (* may raise Not_found *)
    String.sub name 0 n
  with
      Not_found -> ""
;;


(**********************************************************************)

let write_markup_string ~(from_enc:rep_encoding) ~to_enc os s =
  (* Write the 'from_enc'-encoded string 's' as 'to_enc'-encoded string to
   * 'os'. All characters are written as they are.
   *)
  let s' =
    if to_enc = (from_enc :> encoding)
    then s
    else recode_string
	         ~in_enc:(from_enc :> encoding)
		 ~out_enc:to_enc
		 ~subst:(fun n ->
			   failwith
			     ("Pxp_aux.write_markup_string: Cannot represent " ^
			      "code point " ^ string_of_int n))
		 s
  in
  write os s' 0 (String.length s')
;;


let write_data_string ~(from_enc:rep_encoding) ~to_enc os content =
  (* Write the 'from_enc'-encoded string 's' as 'to_enc'-encoded string to
   * 'os'. The characters '&', '<', '>', '"', '%' and every character that
   * cannot be represented in 'to_enc' are paraphrased as entity reference
   * "&...;".
   *)
  let convert_ascii s =
    (* Convert the ASCII-encoded string 's'. Note that 'from_enc' is
     * always ASCII-compatible
     *)
    if to_enc = (from_enc :> encoding)
    then s
    else
      recode_string
        ~in_enc:(from_enc :> encoding)
        ~out_enc:to_enc
        ~subst:(fun n -> assert false)
	s
  in

  let write_ascii s =
    (* Write the ASCII-encoded string 's' *)
    let s' = convert_ascii s in
    write os s' 0 (String.length s')
  in

  let write_part j l =
    (* Writes the substring of 'content' beginning at pos 'j' with length 'l'
     *)
    if to_enc = (from_enc :> encoding) then
      write os content j l
    else begin
      let s' = recode_string
	         ~in_enc:(from_enc :> encoding)
	         ~out_enc:to_enc
	         ~subst:(fun n ->
			   convert_ascii ("&#" ^ string_of_int n ^ ";"))
		 (String.sub content j l)
      in
      write os s' 0 (String.length s')
    end
  in

  let i = ref 0 in
  for k = 0 to String.length content - 1 do
    match content.[k] with
	('&' | '<' | '>' | '"' | '%') as c ->
	  if !i < k then
	    write_part !i (k - !i);
	  begin match c with
	      '&' -> write_ascii "&amp;"
	    | '<' -> write_ascii "&lt;"
	    | '>' -> write_ascii "&gt;"
	    | '"' -> write_ascii "&quot;"
	    | '%' -> write_ascii "&#37;"  (* reserved in DTDs *)
	    | _   -> assert false
	  end;
	  i := k+1
      | _ -> ()
  done;
  if !i < String.length content then
    write_part !i (String.length content - !i)
;;


