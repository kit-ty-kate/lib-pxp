(* $Id: pull.ml,v 1.2 2002/08/17 23:51:09 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(**********************************************************************)
(* Examples for pull parsing                                          *)
(**********************************************************************)

open Pxp_yacc
open Pxp_lexer_types
open Pxp_types
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
  let config = default_config in
  let mgr = create_entity_manager config (from_string s) in
  let next_event = 
    create_pull_parser config (`Entry_content[]) mgr in
  let event = ref (Some E_end_of_stream) in
  while !event <> None do
    event := next_event();
    match !event with
	Some e -> dump_event e
      | None -> ()
  done
;;


(* Stream parsers:
 * parse_list 
 *   "<list><cons><int>34</int><cons><int>67</int><nil/></cons></cons></list>"
 * = [34; 67]
 *)

let parse_list s =

  let rec parse_whole_list stream =
    match stream with parser
	[< 'E_start_tag("list",_,_);
	   l = parse_sub_list;
	   'E_end_tag("list",_);
	   'E_end_of_stream;
	>] ->
	  l

  and parse_sub_list stream =
    match stream with parser
	[< 'E_start_tag("cons",_,_); 
	   head = parse_object;
	   tail = parse_sub_list;
	   'E_end_tag("cons",_)
	>] ->
	  head :: tail
	  
      | [< 'E_start_tag("nil",_,_); 'E_end_tag("nil",_) >] ->
	  []

  and parse_object stream =
    match stream with parser
	[< 'E_start_tag("int",_,_);
	   number = parse_text;
	   'E_end_tag("int",_)
	>] ->
	  int_of_string number

  and parse_text stream =
    match stream with parser
	[< 'E_char_data data;
	   rest = parse_text
	>] ->
	  data ^ rest
      | [< >] ->
	  ""
  in
  let config = 
    { default_config with
	store_element_positions = false;
	  (* don't produce E_position events *)
    }
  in
  let mgr = create_entity_manager config (from_string s) in
  let next_event = 
    create_pull_parser config (`Entry_content[]) mgr in
  let next_event_or_error n =
    let e = next_event n in
    match e with
	Some(E_error exn) -> raise exn
      | _ -> e
  in
  let stream =
    Stream.from next_event_or_error in
  parse_whole_list stream
;;


(* ======================================================================
 * History:
 * 
 * $Log: pull.ml,v $
 * Revision 1.2  2002/08/17 23:51:09  gerd
 * 	Always using Entry_content
 *
 * Revision 1.1  2002/08/17 23:46:15  gerd
 * 	Initial revision.
 *
 * 
 *)