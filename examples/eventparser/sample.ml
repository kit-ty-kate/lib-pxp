(* $Id: sample.ml,v 1.4 2002/08/17 23:09:47 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


(**********************************************************************)
(* Examples for event-based parsing ("SAX-like parsing")              *)
(**********************************************************************)


open Pxp_yacc
open Pxp_lexer_types
open Pxp_types
open Expr
open Exprlex
open Printf


(* dump_event: dumps a single parsing event *)

let dump_event =
  function
      E_start_doc(v,sa,dtd) ->
	printf "E_start_doc version=%s standalone=%b\n" v sa
    | E_end_doc ->
	printf "E_end_doc\n"
    | E_start_tag(name,attlist,_) ->
	printf "E_start_tag %s %s\n" name 
	  (String.concat " " (List.map (fun (n,v) -> n ^ "=" ^ v) attlist))
    | E_end_tag(name,_) ->
	printf "E_end_tag %s\n" name
    | E_char_data data ->
	printf "E_char_data %s\n" data
    | E_pinstr(target,data) ->
	printf "E_pinstr %s %s\n" target data
    | E_comment data ->
	printf "E_comment %s\n" data
    | E_position(ent,line,col) ->
	printf "E_position %s line=%d col=%d\n" ent line col
    | E_error e ->
	printf "E_error %s\n" (Printexc.to_string e)
    | E_end_of_stream ->
	printf "E_end_of_stream\n"
;;


(* parse: prints the events while parsing the passed string *)

let parse s =
  process_entity
    default_config
    (`Entry_document[])
    (create_entity_manager default_config (from_string s))
    dump_event;
  flush stdout
;;


(* curly_parse: demonstrates how to use escape_contents. The character
 * { escapes from normal parsing and calls [escape]. The arithmetic
 * expression inside { ... } is evaluated, and the result is taken 
 * as the text content.
 * Try: curly_parse "<a>{123 + 5}</a>"
 *      curly_parse "<a>{{123 + 5}}</a>"
 *      curly_parse "<a>{123 + 5</a>}</a>"
 *)

let inc_col (l,c) = (l,c+1);;
let inc_line (l,c) = (l+1,0);;
let add_col n (l,c) = (l,c+n);;


let curly_parse s =
  let parse_expr mng =
    (* We get now the current lexical buffer of PXP, and use it for
     * our own parsing. In particular, we call Expr.topexpr
     * to parse the arithmetic expression. While parsing,
     * we track the current line and column (function [scan]).
     *)
    let line_col = ref (mng # current_line_column) in
    (* Note: current_line_column returns the position of the beginning of
     * the token that has been parsed last. The last token was "{" (Lcurly).
     * So we must add 1 to this position to get the position of the 
     * beginning of the next token.
     *)
    line_col := inc_col !line_col;
    let rec scan buf =
      match scan_expr buf with
	  Newline -> 
	    line_col := inc_line !line_col; 
	    scan buf
	| Space ->
	    let n = Lexing.lexeme_end buf - Lexing.lexeme_start buf in
	    line_col := add_col n !line_col;
	    scan buf
	| tok -> 
	    let n = Lexing.lexeme_end buf - Lexing.lexeme_start buf in
	    line_col := add_col n !line_col;
	    tok
    in
    let lexbuf = mng # current_lexbuf in
    let value = topexpr scan lexbuf in
    printf "Result of expression: %d\n" value;
    mng # update_line_column !line_col;
    string_of_int value
  in

  let escape_contents tok mng =
    (* This function is called when "{", "{{", "}", or "}}" are found in
     * character node context.
     *)
    match tok with
	Lcurly ->
	  (* "{" found: *)
	  parse_expr mng
      | LLcurly -> 
	  (* "{{" found: map to "{" *)
	  "{"
      | Rcurly -> 
	  (* "}" found *)
	  failwith "Single brace } not allowed"
      | RRcurly -> 
	  (* "}}" found: map to "}" *)
	  "}"
      | _ -> assert false
  in

  let escape_attributes tok pos mng =
    (* This function is called when "{", "{{", "}", or "}}" are found in
     * attribute values.
     *)
    match tok with
	Lcurly ->
	  (* "{" found: *)
	  parse_expr mng
      | LLcurly ->
	  (* "{{" found: *)
	  "{"
      | Rcurly ->
	  (* "}" found: *)
	  failwith "Single brace } not allowed"
      | RRcurly ->
	  (* "}}" found: *)
	  "}"
      | _ ->
	  assert false
  in

  let config = { default_config with 
		   escape_contents = Some escape_contents;
		   escape_attributes = Some escape_attributes;
	       } in
  process_entity
    config
    (`Entry_document[])
    (create_entity_manager config (from_string s))
    dump_event;
  flush stdout
;;


(* rec_curly_parse: Here, escape_contents calls the XML parser recursively,
 * i.e. you can write XML documents inside curly braces, like in
 * rec_curly_parse "<A> { <B> x </B> } </A>" or
 * rec_curly_parse "<A att='{ <B> x </B> }'>y</A>"
 *
 * This is currently very experimental!
 *)

class any_entity_id = object end ;;
  (* An entity ID is an object without properties except identity *)

let rec_curly_parse s =
  let ent_id_guard = new any_entity_id in
  let base_config = default_config in

  let rec escape ent_id tok mng =
    (* ent_id: is the entity ID containing the last Lcurly, or ent_id_guard
     *  when there was none yet
     *)
    let current_ent = mng # current_entity in
    let current_ent_id = (current_ent :> entity_id) in
    match tok with
	Lcurly ->
	  printf "Starting subparser...\n";
	  (* Because [current_ent] is already open, we cannot use it as
	   * top-level entity in [process_entity] (it is not possible to
	   * open an entity several times). The solution is [sub_ent],
	   * a so-called entity section that behaves much like [current_ent]
	   * and shares most of the state with [current_ent], but pretends
	   * it were an entity of its own.
	   *)
	  let sub_ent = new Pxp_entity.entity_section current_ent in
	  let sub_ent_id = (sub_ent :> entity_id) in
	  let sub_config =
	    { base_config with
		escape_contents = Some (escape sub_ent_id) ;
		escape_attributes = Some (escape_att sub_ent_id) ;
	    }
	  in
	  (* Pushing sub_ent makes it the top-level entity: *)
	  mng # push_entity sub_ent;  
	  process_entity
	    sub_config
	    (`Entry_document[])
	    mng
	    dump_event;
	  assert(mng # current_entity = sub_ent);
	  (* Pop sub_ent afterwards: *)
	  mng # pop_entity ();
	  ""
      | LLcurly -> "{"
      | Rcurly -> 
	  if ent_id = ent_id_guard then
	    (* A right curly brace without matching left curly brace *)
	    failwith "Single brace } not allowed"
	  else 
	    if ent_id = current_ent_id then (
	      (* The normal case: *)
	      printf "Stopping subparser...\n";
	      ignore(current_ent # close_entity);
	      ""
		(* Causes that the current [process_entity] parser invocation
		 * terminates (if possible)
		 *)
	    )
	    else
	      (* ent_id <> current_ent_id: This can happen if entities and
	       * braces are used in strange ways:
	       * <!DOCTYPE a [ <!ENTITY brace '}'> ]> 
	       * <a> { <b>xxx</b> &brace; </a>
	       *)
	      failwith "Bad nesting of entities and braces {...}"
	
      | RRcurly -> "}"
      | _ -> assert false
  and escape_att ent_id tok pos mng = escape ent_id tok mng
  in
  let config = 
    { base_config with 
	escape_contents = Some (escape ent_id_guard);
	escape_attributes = Some (escape_att ent_id_guard);
    } in

  process_entity
    config
    (`Entry_document[])
    (create_entity_manager config (from_string s))
    dump_event;
  flush stdout
;;


(* parse_expr: An example for process_expr that parses the expressions
 * found in a string one after another.
 * Example:
 *      parse_expr "<?pi?><abc>def</abc> <qrt>def</qrt> "
 *
 * Unfortunately, we need the undocumented methods [open_entity] and
 * [close_entity] from [Pxp_entity], and we need the knowledge that
 * a [Begin_entity] token can be found at the beginning of the entity,
 * and that an [End_entity] token signals the end of the entity. Using
 * [process_expr] is quite low-level.
 *)

let parse_expr s =
  let config =
    { default_config with
	enable_pinstr_nodes = true;
	enable_comment_nodes = true
    } in
  let m = create_entity_manager config (from_string s) in
  m # current_entity # open_entity true Pxp_lexer_types.Content;
  let begin_entity_token = !(m # yy_get_next_ref)() in
  assert (begin_entity_token = Begin_entity);
  let tok = ref Ignore in       (* Ignore does not occur in the token stream *)
  while !tok <> End_entity do
    let first_token =
      if !tok <> Ignore then Some !tok else None in
    printf "*** Calling process_expr...\n";
    process_expr ?first_token ~following_token:tok config m dump_event;
  done;
  ignore(m # current_entity # close_entity);
;;

(* ======================================================================
 * History:
 * 
 * $Log: sample.ml,v $
 * Revision 1.4  2002/08/17 23:09:47  gerd
 * 	Updated.
 *
 * Revision 1.3  2002/08/05 22:36:06  gerd
 * 	escape_attributes has an additional position argument
 *
 * Revision 1.2  2002/08/03 17:40:19  gerd
 * 	Support for curly braces in attribute values.
 *
 * Revision 1.1  2002/07/14 23:02:51  gerd
 * 	Initial revision.
 *
 * 
 *)
