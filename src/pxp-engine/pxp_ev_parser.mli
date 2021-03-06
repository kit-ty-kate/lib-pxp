(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(** Calling the parser in event mode *)

(** In event parsing, the parser generates a stream of events while reading
    XML text (one or several files). In "push mode" the user provides
    a callback function, and the parser invokes this function for every
    event. In "pull mode", the parser creates a function that fetches
    the next event of the stream, and that can be repeatedly be called
    by the user to get one event after the other.

    For an introduction into this type of parsing, see {!Intro_events}.
    In {!Pxp_event} you find functions for composing, analyzing and
    transforming streams of events (for pull mode). For converting a
    stream of events into a tree, see {!Pxp_document.solidify}. For
    converting a tree into a stream of events, see 
    {!Pxp_document.liquefy}.
 *)


open Pxp_dtd
open Pxp_types

val create_entity_manager :
      ?is_document:bool ->       (* default: true *)
      config -> 
      source -> 
        Pxp_entity_manager.entity_manager
  (** Creates an entity manager that is initialized with the toplevel
   * entity referenced by the source argument. The entity manager
   * can be used by [process_entity] below.
   *
   * The following configuration options are interpreted:
   * - [warner]
   * - [encoding]
   * - [debugging_mode]
   *
   * [is_document]: [true], the default, sets that the entity to read is a complete
   *   document, and [false] sets that it is only a fragment. The value [true] enforces
   *   several restrictions on document entities, e.g. that 
   *   [<![INCLUDE[..]]>] and [<![IGNORE[..]]>] are not allowed and that
   *   additional nesting rules are respected by parameter entities.
   *)



val process_entity :
      config -> 
      entry ->
      Pxp_entity_manager.entity_manager ->
      (event -> unit) ->
        unit
  (** Parses a document or a document fragment in push mode. At least the
   * well-formedness
   * of the document is checked, but the flags of the [entry] argument
   * may specify more.
   *
   * While parsing, events are generated and the passed function is
   * called for every event. The parsed text is read from the
   * current entity of the entity manager. It is allowed that the
   * current entity is open or closed.
   * 
   * The entry point to the parsing rules can be specified as follows:
   * - [`Entry_document]:
   *   This entry point corresponds to the grammar production for documents.
   *   The first generated event is always [E_start_doc],
   *   it contains the whole DTD as object (no events are generated
   *   during DTD parsing, only the wholly parsed DTD is passed back). The
   *   events for the XML body follow, terminated by [E_end_doc] and then
   *   [E_end_of_stream].
   * - [`Entry_content]:
   *   This entry point corresponds to the grammar production for 
   *   external entities (XML declaration followed by any sequence of
   *   content). The emitted events are terminated
   *   by [E_end_of_stream].
   * - [`Entry_element_content]:
   *   There is no corresponding grammar production in the XML standard.
   *   An XML declaration, followed by [misc* element misc*]. The emitted 
   *   events are terminated by [E_end_of_stream].
   * - [`Entry_declarations]:
   *   Currently not supported. (But see {!Pxp_dtd_parser} for functions
   *   parsing DTDs.)
   * - [`Entry_expr]: Do not pass this entry point! There is the specially
   *   crafted function {!Pxp_ev_parser.process_expr} for it.
   *
   * The entry points have options, see {!Pxp_types.entry} for explanations.
   *
   * It may happen that several adjacent [E_char_data] events are
   * emitted for the same character data section.
   *
   * There are filter functions that apply normalization routines
   * to the events, see below.
   *
   * Only the following config options have an effect:
   * - [warner]
   * - [encoding]
   * - [enable_pinstr_nodes]
   * - [enable_comment_nodes]
   * - [enable_super_root_node]
   * - [store_element_positions]
   * - [name_pool] and all name pool options
   * - [enable_namespace_processing]
   *
   * If an error happens, the callback function is invoked exactly once
   * with the [E_error] event. The error is additionally passed to the caller
   * by letting the exception fall through to the caller. It is not possible
   * to resume parsing after an error.
   *
   * The idea behind this special error handling is that the callback
   * function should always be notified when the parser stops, no matter
   * whether it is successful or not. So the last event passed to the
   * callback function is either [E_end_of_stream] or [E_error]. You can
   * imagine that [process_entity] follows this scheme:
   *
   * {[
   * try
   *   "parse";
   *   eh E_end_of_stream           (* eh is the callback function *)
   * with
   *   error ->
   *     "cleanup";
   *     let pos = ... in
   *     let e = At(pos, error) in
   *     eh (E_error e); 
   *     raise e
   * ]}
   *
   * Note that there is always an [At(_,_)] exception that wraps the exception
   * that originally occurred. - This style of exception handling applies
   * to exceptions generated by the parser as well as to exceptions raised
   * by the callback function.
   *)


val process_expr :
      ?first_token: Pxp_lexer_types.token ->
      ?following_token: Pxp_lexer_types.token ref ->
      config -> 
      Pxp_entity_manager.entity_manager ->
      (event -> unit) ->
        unit
  (** This is a special parsing function that corresponds to the entry
   * [`Entry_expr], i.e. it parses a single element, processing instruction,
   * or comment. In contrast to [process_entity], the current entity
   * is not opened, but it is expected that the entity is already open.
   * Of course, the entity is not closed after parsing (except an error
   * happens).
   *
   * - [first_token]: This token is prepended to the tokens read from the
   *    entity manager.
   * - [following_token]: The token following the last parsed token is
   *    optionally stored into this variable.
   *    Note: By design the parser {b always} reads the following token.
   *    I know that this may lead to serious problems when it is tried
   *    to integrate this parser with another parser. It is currently
   *    hard to change!
   *)

val close_entities : Pxp_entity_manager.entity_manager -> unit
  (** Closes all entities managed by this entity manager, and frees
      operating system resources like open files.
   *)

val create_pull_parser :
      config -> 
      entry ->
      Pxp_entity_manager.entity_manager ->
        (unit -> event option)
  (** Invoke the event parser using the pull model. It is used as:
   * {[
   * let next_event = create_pull_parser cfg entry mng in
   * let ev = next_event()
   * ]}
   *
   * Now [next_event]
   * should be invoked repeatedly until it returns [None], indicating the
   * end of the document. The events are encoded as [Some ev].
   *
   * The function returns exactly the same events as [process_entity].
   *
   * In contrast to [process_entity], no exception is raised when an
   * error happens. Only the [E_error] event is generated (as last event
   * before [None]).
   *
   * To create a stream of events, just do:
   * {[
   * let next = create_pull_parser cfg entry mng in
   * let stream = Stream.from(fun _ -> next())
   * ]}
   *)
