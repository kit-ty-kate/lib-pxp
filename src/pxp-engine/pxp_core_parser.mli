(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* INTERNAL PXP INTERFACE!
 *
 * This module should not be used from outside. Use Pxp_tree_parser or
 * Pxp_ev_parser. The signature of this module may be heavily changed
 * without keeping backwards compatibility.
 *)

open Pxp_types
open Pxp_lexers
open Pxp_lexer_types
open Pxp_entity_manager
open Pxp_dtd


type context =
    { mutable current : unit -> token;  (* get the current token *)
      mutable get_next : unit -> token; (* go on to the next token; return it *)
      mutable current_token : token;    (* This is the current token *)
      mutable manager : entity_manager; (* The entity manager *)
    }

type continuation_state =
  { cont_context : context;
    cont_extend_dtd : bool;
    cont_process_xmldecl : bool;
  }

exception End_of_parsing
  (* One way to signal that parsing is done *)

exception Interrupt_parsing of continuation_state
  (* Interrupt the parsing loop to process pull-style events *)

val make_context : ?first_token:token -> entity_manager -> context

type extended_entry =
  [ entry
  | `Entry_continuation of continuation_state
  ]


type 't array_stack

val stack_create : 't -> 't array_stack

val stack_push : 't -> 't array_stack -> unit

val stack_top : 't array_stack -> 't

val stack_pop : 't array_stack -> 't


class virtual core_parser : dtd -> config -> int ->
object 
  val mutable dtd : dtd
  val lexerset : lexer_set
  val config : config
  val mutable n_tags_open : int
  val mutable n_entities_open : int
  val pull_counter_limit : int
  val mutable pull_counter : int
  val mutable p_internal_subset : bool
  val mutable src_norm_mapping : (string * string ) list
  val mutable default_normprefix : string

  method parse : context -> extended_entry -> unit

  method private only_whitespace : string -> unit

  method private push_src_norm_mapping : 
                   namespace_manager -> string -> (string * string) list ->
		     (string * string * string * 
		      (string * string * string * string) list)

  method private pop_src_norm_mapping : unit -> unit

  method private virtual init_for_xml_body : unit -> unit

  method private virtual event_document_xmldecl : 
                             Pxp_lexer_types.prolog_token list -> unit

  method private virtual event_start_tag : 
                             (string*int*int) option ->
			     string ->
			     (string * string) list ->
			     bool ->
			     entity_id ->
			       unit

  method private virtual event_end_tag :
                             string ->
			     entity_id ->
			       unit

  method private virtual event_char_data : string -> unit

  method private virtual event_pinstr : 
                             (string*int*int) option ->
			     string ->
			     string ->
			       unit

  method private virtual event_comment : 
                             (string*int*int) option ->
			     string list ->
			       unit


  method private virtual sub_parser : unit -> core_parser
    (* used for the external subset *)

end

