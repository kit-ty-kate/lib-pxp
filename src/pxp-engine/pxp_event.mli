(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_dtd

(** Dealing with events (for pull parsing) *)


(**********************************************************************)
(*                   Event streams and lists                          *)
(**********************************************************************)

(** {2 Events as lists} *)

val to_list : (unit -> event option) -> event list
  (** Fetch all events from the pull function, and return the corresponding 
   * list of events.
   *)

val of_list : event list -> (unit -> event option)
  (** [of_list l]: Create a pull function fetching the events from [l] *)

val concat : (unit -> event option) list -> 
             (unit -> event option) 
  (** [let p = concat l]: The pull functions contained in the list [l] are
      concatenated, and a new pull function [p] is created that pulls from
      the functions of the list in turn (when one function indicates
      the end of the events, it is continued with the next function in the
      list).
   *)

val iter : (event -> unit) -> (unit -> event option) -> unit
  (** [iter f p]: The pull function [p] is repeatedly called to get a
      stream of events [e]. For each event the function [f] is called.
   *)

(* Missing: map, fold, ... *)

val extract : event -> (unit -> event option) -> (unit -> event option)
  (** [let next' = extract e next]:
   * Extracts a subexpression from the pull function [next] prepended by [e].
   * A subexpression consists of either
   * - a single data, comment, PI, or error event
   * - a start tag, either of an element, a super root, or a document,
   *   until the corresponding end tag
   * - a position event followed by a subexpression
   *
   * The returned pull function contains all events of the subexpression.
   * When the extracted stream is read, the original stream is read, too.
   *
   * Example:
   * {[
   * let l = [ E_pinstr; E_start_tag; E_data; E_start_tag; E_end_tag;
   *           E_comment; E_end_tag; E_data ];;
   * let g = of_list l;;
   * g();;
   * let Some e = g();;         (* e = E_start_tag *)
   * let g' = extract e g;;
   * g'();;                     (* returns Some E_start_tag *)
   * ...
   * g'();;                     (* returns Some E_end_tag *)
   * g'();;                     (* returns None, end of subexpression *)
   * g();;                      (* returns Some E_data *)
   * g();;                      (* returns None *)
   * ]}
   *)


(**********************************************************************)
(*                            Filters                                 *)
(**********************************************************************)

(** {2 Filters} *)

type pull_fn = unit -> event option
  (** The result type of {!Pxp_ev_parser.create_pull_parser} *)

type filter = pull_fn -> pull_fn
  (** A filter transforms a pull function into another pull function *)

val norm_cdata_filter : filter
  (** This filter
   *  - removes empty [E_char_data] events
   *  - concatenates adjacent [E_char_data] events
   *
   * but does not touch any other parts of the event stream.
   *)

val drop_ignorable_whitespace_filter : filter
  (** This filter 
   *  - checks whether character data between elements in a 
   *    "regexp" or "non-PCDATA mixed" content model consists 
   *    only of whitespace, and
   *  - removes these whitespace characters from the event stream.
   *
   * If the check fails, a [WF_Error] will be raised.
   *
   * This filter works only if the DTD found in the event stream
   * actually contains element declarations. This is usually enabled
   * by including the [`Extend_dtd_fully] or [`Val_mode_dtd] options to 
   * the [entry] passed to the [create_pull_parser] call. Furthermore, 
   * there must be an [E_start_doc] event.
   *
   * This filter does not perform any other validation checks.
   *)

val pfilter : (event -> bool) -> filter
  (** Filters an event stream by a predicate
   *
   * Example: Remove comments:
   * {[ pfilter (function E_comment _ -> false | _ -> true) g ]}
   *)

val unwrap_document : pull_fn -> ((unit -> (string * Pxp_dtd.dtd)) * pull_fn)
  (** This filter removes the document wrapping from the stream
      (see {!Intro_events.docs} for a definition what this is).
      It is called like

      {[
         let (get_doc_details, next') = unwrap_document next
         let (version, dtd) = get_doc_details()
       ]}

      The returned [filter] removes any [E_start_doc], [E_end_doc],
      [E_start_super], [E_end_super], and [E_end_of_stream] events.
      If an [E_error] event is encountered, the contained exception
      is raised. All other events of the stream remain.

      The function [get_doc_details] can be called to get details
      about the document definition. If an [E_start_doc] event is
      found in the stream, the XML version string and the DTD
      object are returned. The function fails if [E_start_doc] is
      not the first event of the stream.
   *)


(* Missing: ID check *)


(** {2 Helpers for namespace processing} *)

(** The names in [E_start_tag] can be analyzed with the following. *)

val namespace_split : string -> (string * string)
  (** [let (p,l) = namespace_split name]: Splits [name] into the prefix
      [p] and the local name [l]. If there is no colon in [name], the
      function returns [p=""], and [l=name].
   *)


val extract_prefix : string -> string
  (** Returns the prefix in the name, or [""] if there is no prefix.
      Same as [fst(namespace_split name)].
   *)



(**********************************************************************)
(*                            Printing                                *)
(**********************************************************************)

(** {2 Printing event streams} *)

type dtd_style =
    [ `Ignore
    | `Include
    | `Reference
    ]

val write_events : 
  ?default:string ->        (* Default: none *)
  ?dtd_style:dtd_style ->   (* Default: `Include *)
  ?minimization:[`AllEmpty | `None] ->   (* Default: `None *)
  output_stream -> 
  encoding -> 
  rep_encoding -> 
  (unit -> event option) -> 
    unit
  (** Writes the events to the [output_stream]. The events must be encoded
   * as indicated by the [rep_encoding] argument, but the output is written
   * as specified by the [encoding] argument.
   *
   * The normalized namespace prefixes are declared as needed. Additionally,
   * one can set the default namespace by passing [default], which must be
   * the normalized prefix of the default namespace.
   *
   * For [E_doc_start] events, the DTD may be written. This is controlled by
   * [dtd_style]:
   * - [`Ignore]: No [DOCTYPE] clause is written
   * - [`Include]: The [DOCTYPE] clause is written, and the DTD is included
   *   in the internal subset (the default)
   * - [`Reference]: The [DOCTYPE] clause is written as a reference to an
   *   external DTD
   *
   * Option [minimization]: How to write out empty elements. [`AllEmpty]
   * means that all empty elements are minimized (using the [<name/>]
   * form). [`None] does not minimize at all and is the default.
   *)

val display_events : 
  ?dtd_style:dtd_style ->   (* Default: `Include *)
  ?minimization:[`AllEmpty | `None] ->   (* Default: `None *)
  output_stream -> 
  encoding -> 
  rep_encoding -> 
  (unit -> event option) -> 
    unit
  (** Writes the events to the [output_stream]. The events must be encoded
   * as indicated by the [rep_encoding] argument, but the output is written
   * as specified by the [encoding] argument.
   *
   * Namespace prefixes are declared as defined in the namespace scopes.
   * Missing prefixes are invented on the fly. 
   *
   * The way the DTD is printed can be set as in [write_events].
   *)

val string_of_event : event -> string
  (** Returns a string representation of events, for debugging *)
