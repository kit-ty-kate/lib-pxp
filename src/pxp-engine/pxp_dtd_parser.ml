(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_lexers
open Pxp_lexer_types
open Pxp_entity_manager
open Pxp_dtd
open Pxp_core_parser
open Pxp_ev_parser


let create_empty_dtd config =
  create_dtd ?swarner:config.swarner ~warner:config.warner config.encoding ;;

class dtd_parser init_dtd init_config =
object (self)
  inherit core_parser init_dtd init_config (-1)

  (* The following methods cannot be called by the core_parser,
   * because this is impossible for `Entry_declarations.
   *)

  method private init_for_xml_body() = 
    assert false

  method private event_document_xmldecl xmldecl = 
    assert false

  method private event_start_tag position name attlist emptiness tag_beg_entid =
    assert false

  method private event_end_tag name tag_end_entid =
    assert false

  method private event_char_data data =
    assert false

  method private event_pinstr position target value =
    assert false

  method private event_comment position mat =
    assert false

  method private sub_parser () =
    assert false
end


let parse_dtd_entity cfg src =
  let dtd = new dtd ?swarner:cfg.swarner cfg.warner cfg.encoding in
  ( match cfg.enable_namespace_processing with
	Some mng -> dtd # set_namespace_manager mng
      | None     -> ()
  );
  let r, en =
    open_source cfg src false dtd in
  let pobj =  new dtd_parser dtd cfg in
  let mgr = new entity_manager en dtd in
  en # open_entity 
    ~gen_att_events:false true Declaration;
  begin try
    let context = make_context mgr in
    pobj # parse context (`Entry_declarations [`Val_mode_dtd ]);
    if en # is_open then ignore(en # close_entity);
  with
    | Failure "Invalid UTF-8 stream" ->
	(* raised by the wlex-generated lexers only: map to Malformed_code *)
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	raise (At(pos, Netconversion.Malformed_code))
    | error ->
	let pos = mgr # position_string in
	mgr # pop_entity_until en;
	if en # is_open then ignore(en # close_entity);
	raise (At(pos, error))
  end;
  dtd # validate;
  if cfg.accept_only_deterministic_models then dtd # only_deterministic_models;
  dtd
;;



exception Return_DTD of dtd

let extract_dtd_from_document_entity cfg src =
  let rec extract exn =
    match exn with
	Return_DTD dtd -> Some dtd
      | At(_, exn') -> extract exn'
      | _ -> None
  in

  let mng = create_entity_manager ~is_document:true cfg src in
  let entry = `Entry_document [ `Val_mode_dtd; `Parse_xml_decl ] in
  let handle ev =
    match ev with
	E_start_doc(_,dtd) -> raise(Return_DTD dtd)
      | E_error _ -> ()  (* ignore now, exception will be raised anyway *)
      | E_position(_,_,_) -> ()
      | _ -> assert false
  in
  try
    process_entity cfg entry mng handle;
    assert false
  with
      exn ->
	( match extract exn with
	      Some dtd -> dtd
	    | _ -> raise exn
	)
;;



