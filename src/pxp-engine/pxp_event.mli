(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_dtd

(**********************************************************************)
(*                   Event streams and lists                          *)
(**********************************************************************)

val to_list : (unit -> event option) -> event list
  (* Fetch all events from the event stream, and return the corresponding 
   * list of events.
   *)

val of_list : event list -> (unit -> event option)
  (* Pull the events from the input list *)

val concat : (unit -> event option) list -> 
             (unit -> event option) 
  (* Pull the events from the streams in turn *)

val iter : (event -> unit) -> (unit -> event option) -> unit
  (* Iterates over the events of the stream and calls the function *)

(* Missing: map, fold, ... *)

val extract : event -> (unit -> event option) -> (unit -> event option)
  (* let next' = extract e next:
   * Extracts a subexpression from the stream [next] prepended by [e].
   * A subexpression consists of either
   * - a single data, comment, PI, or error event
   * - a start tag, either of an element, a super root, or a document,
   *   until the corresponding end tag
   * - a position event followed by a subexpression
   * The returned stream contains all events of the subexpression.
   * When the extracted stream is read, the original stream is read, too.
   *
   * Example:
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
   *)


(**********************************************************************)
(*                            Filters                                 *)
(**********************************************************************)

type filter = (unit -> event option) -> (unit -> event option)

val norm_cdata_filter : filter
  (* This filter
   *  - removes empty E_char_data events
   *  - concatenates adjacent E_char_data events
   * but does not touch any other parts of the event stream.
   *)

val drop_ignorable_whitespace_filter : filter
  (* This filter 
   *  - checks whether character data between elements in a 
   *    "regexp" or "non-PCDATA mixed" content model consists 
   *    only of whitespace, and
   *  - removes these whitespace characters from the event stream.
   * If the check fails, a WF_Error will be raised.
   *
   * This filter works only if the DTD found in the event stream
   * actually contains element declarations. This is usually enabled
   * by including the `Extend_dtd_fully or `Val_mode_dtd options to 
   * the [entry] passed to the [create_pull_parser] call. Furthermore, 
   * there must be an E_start_doc event.
   *
   * This filter does not perform any other validation checks.
   *)

val pfilter : (event -> bool) -> filter
  (* Filters an event stream by a predicate
   *
   * Example: Remove comments:
   * pfilter (function E_comment _ -> false | _ -> true) g
   *)


(* Missing: ID check *)

(**********************************************************************)
(*                            Printing                                *)
(**********************************************************************)

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
  (* Writes the events to the [output_stream]. The events must be encoded
   * as indicated by the [rep_encoding] argument, but the output is written
   * as specified by the [encoding] argument.
   *
   * The normalized namespace prefixes are declared as needed. Additionally,
   * one can set the default namespace by passing [default], which must be
   * the normalized prefix of the default namespace.
   *
   * For E_doc_start events, the DTD may be written. This is controlled by
   * [dtd_style]:
   * - `Ignore: No DOCTYPE clause is written
   * - `Include: The DOCTYPE clause is written, and the DTD is included
   *   in the internal subset
   * - `Reference: The DOCTYPE clause is written as a reference to an
   *   external DTD
   *
   * Option [~minimization]: How to write out empty elements. [`AllEmpty]
   * means that all empty elements are minimized (using the <name/>
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
  (* Writes the events to the [output_stream]. The events must be encoded
   * as indicated by the [rep_encoding] argument, but the output is written
   * as specified by the [encoding] argument.
   *
   * Namespace prefixes are declared as defined in the namespace scopes.
   * Missing prefixes are invented on the fly. 
   *
   * The way the DTD is printed can be set as in [write_events].
   *)
