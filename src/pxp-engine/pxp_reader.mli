(* $Id: pxp_reader.mli,v 1.1 2000/05/29 23:48:38 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types;;


(* The class type resolver is the official type of all "resolvers". 
 * Resolvers get file names (or better, external identifiers) and 
 * return lexbufs, scanning the file for tokens. Resolvers may be
 * cloned, and clones can interpret relative file names relative to
 * their creator. 
 *)

class type resolver =
  object
    (* A resolver can open a character source, and returns this source as
     * Lexing.lexbuf.
     * The resolver should recode the source into ISO-8859-1. By default,
     * a resolver should assume UTF-8 or UTF-16 encoding. Before
     * 'change_encoding' is invoked, the resolver should only return
     * lexbufs with one character. After 'change_encoding' has been invoked,
     * there is no character limit anymore.
     * 'change_encoding' can only be invoked once. This method is usually
     * called after the <? ... ?> prolog of the entity has been read.
     * If this method is not called, it is up to the resolver to find out
     * if UTF-8 or UTF-16 is used. It is recommended to invoke this method
     * with an empty string to indicate this situation.
     *)
    method open_in : ext_id -> Lexing.lexbuf
    method close_in : unit
    method change_encoding : string -> unit


    (* Every resolver can be cloned. The clone does not inherit the connection
     * with the external object, i.e. it is closed.
     *)
    method clone : resolver

  end
;;


(* The following class is the current main implementation of resolvers.
 * It fetches strings from an arbitrary source (by calling init_in, and
 * then repeatedly next_string), recodes them to ISO-8859-1, and creates
 * lexbufs for them.
 * It is not complete, as the source is missing.
 *
 * Note that 'resolve_general' may change in future revisions; it is ugly.
 *)

class virtual resolve_general :
  collect_warnings -> Pxp_types.rep_encoding ->
  object 
    val mutable encoding : Pxp_types.encoding
    val mutable encoding_requested : bool
    val warner : collect_warnings

    method clone : resolver

    method private warn : int -> unit
    method private autodetect : string -> unit

    method private virtual next_string : string -> int -> int -> int
    method private virtual init_in : ext_id -> unit
    method virtual close_in : unit

    method open_in : ext_id -> Lexing.lexbuf

    method change_encoding : string -> unit
  end
;;


(* The next classes are resolvers for concrete input sources. *)

class resolve_read_channel : 
  in_channel -> collect_warnings -> Pxp_types.rep_encoding -> resolver;;

  (* Reads from the passed channel (it may be even a pipe). Note that this
   * resolver cannot handle file inclusions, as it is pre-bound to a 
   * specific channel and is not able to interpret file names.
   * That means, if there is a entity reference (something like &name; or
   * %name;) to parse, and the definition points to another file, the
   * resolver will fail.
   *)


class resolve_read_string : 
  string -> Pxp_types.rep_encoding -> resolver;;

  (* Reads from the passed string. As 'resolver_read_channel', this 
   * resolver cannot handle file inclusions.
   *)


class resolve_as_file :
  collect_warnings -> Pxp_types.rep_encoding -> resolver;;

  (* Reads from the local file system. Every file name is interpreted as
   * file name of the local file system, and the referred file is read.
   * This resolver can handle file inclusions as long as they do not
   * exceed the scope of the local file system (i.e. no URLs).
   *)

(* ======================================================================
 * History:
 * 
 * $Log: pxp_reader.mli,v $
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
 * Old logs from markup_reader.mli:
 *
 * Revision 1.3  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.2  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.1  2000/03/13 23:41:54  gerd
 * 	Initial revision.
 *
 * 
 *)
