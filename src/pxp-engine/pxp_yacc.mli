(* $Id: pxp_yacc.mli,v 1.4 2000/07/09 17:52:54 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


(*$ markup-yacc.mli *)

open Pxp_types
open Pxp_dtd
open Pxp_document

type config =
    { warner : collect_warnings;
         (* An object that collects warnings. *)

      errors_with_line_numbers : bool;
         (* Whether error messages contain line numbers or not. The parser
	  * is 10 to 20 per cent faster if line numbers are turned off;
	  * you get only character positions in this case.
	  *)

      processing_instructions_inline : bool;
         (* true: turns a special mode for processing instructions on. Normally,
	  * you cannot determine the exact location of a PI; you only know
	  * in which element the PI occurs. The "inline" mode makes it possible
	  * to find the exact location out: Every PI is artificially wrapped
	  * by a special element with name "-pi". For example, if the XML text
	  * is <a><?x?><?y?></a>, the parser normally produces only an element
	  * object for "a", and puts the PIs "x" and "y" into it (without
	  * order). In inline mode, the object "a" will contain two objects
	  * with name "-pi", and the first object will contain "x", and the
	  * second "y".
	  * Notes:
	  * (1) The name "-pi" is reserved. You cannot use it for your own
	  *     tags because tag names must not begin with '-'.
	  * (2) You need not to add a declaration for "-pi" to the DTD. These
	  *     elements are handled separately.
	  * (3) Of course, the "-pi" objects are created from exemplars of
	  *     your DOM map.
	  *)

      virtual_root : bool;
         (* true: the topmost element of the XML tree is not the root element,
	  * but the so-called virtual root. The root element is a son of the
	  * virtual root. The virtual root is an ordinary element with name
	  * "-vr".
	  * The following behaviour changes, too:
	  * - PIs occurring outside the root element and outside the DTD are
	  *   added to the virtual root instead of the document object
	  * - If processing_instructions_inline is also turned on, these PIs
	  *   are added inline to the virtual root
	  * Notes:
	  * (1) The name "-vr" is reserved. You cannot use it for your own
	  *     tags because tag names must not begin with '-'.
	  * (2) You need not to add a declaration for "-vr" to the DTD. These
	  *     elements are handled separately.
	  * (3) Of course, the "-vr" objects are created from exemplars of
	  *     your DOM map.
	  *)

      encoding : rep_encoding;
        (* Specifies the encoding used for the *internal* representation
	 * of any character data.
	 * Note that the default is still Enc_iso88591.
	 *)

      recognize_standalone_declaration : bool;
        (* Whether the "standalone" declaration is recognized or not.
	 * This option does not have an effect on well-formedness parsing:
	 * in this case such declarations are never recognized.
	 *
	 * Recognizing the "standalone" declaration means that the 
	 * value of the declaration is scanned and passed to the DTD.
	 *)

      store_element_positions : bool;
        (* Whether the file name, the line and the column of the
	 * beginning of elements are stored in the element nodes.
	 * This option may be useful to generate helpful error messages.
	 *)

      (* The following options are not implemented, or only for internal
       * use.
       *)

      debugging_mode : bool;
    }


type source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (ext_id * Pxp_reader.resolver)

val from_channel : 
      ?system_encoding:encoding -> ?id:ext_id -> ?fixenc:encoding -> 
      in_channel -> source

val from_string :
      ?fixenc:encoding -> string -> source

val from_file :
      ?system_encoding:encoding -> string -> source

(* Notes on sources (version 2):
 *
 * Sources specify where the XML text to parse comes from. Sources not only
 * represent character streams, but also external IDs (i.e. SYSTEM or PUBLIC
 * names), and they are interpreted as a specific encoding of characters.
 * A source should be associated with an external ID, because otherwise
 * it is not known how to handle relative names.
 *
 * There are two primary sources, Entity and ExtID, and several functions
 * for derived sources. First explanations for the functions:
 *
 * from_channel: The XML text is read from an in_channel. By default, the
 *   channel is not associated with an external ID, and it is impossible
 *   to resolve relative SYSTEM IDs found in the document.
 *   If the ?id argument is passed, it is assumed that the channel has this
 *   external ID. If relative SYSTEM IDs occur in the document, they can
 *   be interpreted; however, it is only possible to read from "file:"
 *   IDs.
 *   By default, the channel automatically detects the encoding. You can
 *   set a fixed encoding by passing the ?fixenc argument.
 *
 * from_string: The XML text is read from a string.
 *   It is impossible to read from any external entity whose reference is found
 *   in the string.
 *   By default, the encoding of the string is detected automatically. You can
 *   set a fixed encoding by passing the ?fixenc argument.
 *
 * from_file: The XML text is read from the file whose file name is
 *   passed to the function (as UTF-8 string).
 *   Relative system IDs can be interpreted by this function.
 *   The ?system_encoding argument specifies the character encoding used
 *   for file names (sic!). By default, UTF-8 is assumed.
 *
 * Examples:
 *
 * from_file "/tmp/file.xml": 
 *   reads from this file, which is assumed to have the ID 
 *   SYSTEM "file://localhost/tmp/file.xml".
 *
 * let ch = open_in "/tmp/file.xml" in
 * from_channel ~id:(System "file://localhost/tmp/file.xml") ch
 *   This does the same, but uses a channel.
 *
 * from_channel ~id:(System "http://host/file.xml")
 *              ch
 *   reads from the channel ch, and it is assumed that the ID is
 *   SYSTEM "http://host/file.xml". If there is any relative SYSTEM ID,
 *   it will be interpreted relative to this location; however, there is
 *   no way to read via HTTP.
 *   If there is any "file:" SYSTEM ID, it is possible to read the file.
 *
 * The primary sources:
 *
 * - ExtID(x,r): The identifier x (either the SYSTEM or the PUBLIC name) of the
 *   entity to read from is passed to the resolver, and the resolver finds
 *   the entity and opens it.
 *   The intention of this option is to allow customized
 *   resolvers to interpret external identifiers without any restriction.
 *   The Pxp_reader module contains several classes allowing the user to
 *   compose such a customized resolver from predefined components.
 *
 *   ExtID is the interface of choice for own extensions to resolvers.
 *
 * - Entity(m,r): You can implementy every behaviour by using a customized
 *   entity class. Once the DTD object d is known that will be used during
 *   parsing, the entity  e = m d  is determined and used together with the
 *   resolver r.
 *   This is only for hackers.
 *)



val default_config : config
  (* - The resolver is able to read from files by name
   * - Warnings are thrown away
   * - Error message will contain line numbers
   * - The internal encoding is ISO-8859-1
   * - standalone declaration is checked
   *)

val default_extension : ('a node extension) as 'a
  (* A "null" extension; an extension that does not extend the funtionality *)

val default_spec : ('a node extension as 'a) spec
  (* Specifies that you do not want to use extensions. *)

val parse_dtd_entity      : config -> source -> dtd
  (* Parse an entity containing a DTD (external subset), and return this DTD. *)

val extract_dtd_from_document_entity : config -> source -> dtd
  (* Parses a closed document, i.e. a document beginning with <!DOCTYPE...>,
   * and returns the DTD contained in the document.
   * The parts of the document outside the DTD are actually not parsed,
   * i.e. parsing stops when all declarations of the DTD have been read.
   *)

val parse_document_entity : 
  ?transform_dtd:(dtd -> dtd) ->
  config -> source -> 'ext spec -> 'ext document
  (* Parse a closed document, i.e. a document beginning with <!DOCTYPE...>,
   * and validate the contents of the document against the DTD contained
   * and/or referenced in the document.
   *
   * If the optional argument ~transform_dtd is passed, the following 
   * modification applies: After the DTD (both the internal and external
   * subsets) has been parsed, the function ~transform_dtd is called,
   * and the resulting DTD is actually used to validate the document.
   *
   * If the optional argument ~transform_dtd is missing, the parser
   * behaves in the same way as if the identity were passed as ~transform_dtd.
   *)

val parse_wfdocument_entity : config -> source -> 'ext spec -> 'ext document
  (* Parse a closed document (see parse_document_entity), but do not
   * validate it. Only checks on well-formedness are performed.
   *)

val parse_content_entity  : config ->
                            source ->
			    dtd ->
			    'ext spec ->
			      'ext node
  (* Parse a file representing a well-formed fragment of a document. The
   * fragment must be a single element (i.e. something like <a>...</a>;
   * not a sequence like <a>...</a><b>...</b>). The element is validated
   * against the passed DTD, but it is not checked whether the element is
   * the root element specified in the DTD.
   *)

val parse_wfcontent_entity : config ->
                             source ->
 	 		     'ext spec ->
			       'ext node
  (* Parse a file representing a well-formed fragment of a document
   * (see parse_content_entity). The fragment is not validated, only
   * checked for well-formedness.
   *)
  

(*$-*)


(* ======================================================================
 * History:
 *
 * $Log: pxp_yacc.mli,v $
 * Revision 1.4  2000/07/09 17:52:54  gerd
 * 	New option store_element_positions.
 *
 * Revision 1.3  2000/07/08 16:26:21  gerd
 * 	Added the signatures of the functions
 * 'extract_dtd_from_document_entity' and 'parse_wfcontent_entity'.
 * Updated the signature of 'parse_document_entity': New optional
 * argument 'transform_dtd'.
 * 	Updated the comments.
 *
 * Revision 1.2  2000/07/04 22:09:03  gerd
 * 	MAJOR CHANGE: Redesign of the interface (not yet complete).
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
 * Old logs from markup_yacc.mli:
 *
 * Revision 1.4  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.3  2000/05/27 19:24:01  gerd
 * 	New option: recognize_standalone_declaration.
 *
 * Revision 1.2  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.1  2000/05/06 23:21:49  gerd
 * 	Initial revision.
 *
 * Revision 1.9  2000/04/30 18:23:38  gerd
 * 	New config options 'processing_instructions_inline' and
 * 'virtual_root'.
 *
 * Revision 1.8  2000/03/13 23:46:46  gerd
 * 	Change: The 'resolver' component of the 'config' type has
 * disappeared. Instead, there is a new resolver component in the Entity
 * and ExtID values of 'source'. I hope that this makes clearer that the
 * resolver has only an effect if used together with Entity and ExtID
 * sources.
 * 	Change: The Entity value can now return the entity dependent
 * on the DTD that is going to be used.
 *
 * Revision 1.7  2000/02/22 02:32:02  gerd
 * 	Updated.
 *
 * Revision 1.6  2000/02/22 01:52:45  gerd
 * 	Added documentation.
 *
 * Revision 1.5  2000/01/20 20:54:43  gerd
 * 	New config.errors_with_line_numbers.
 *
 * Revision 1.4  1999/09/01 23:09:10  gerd
 * 	New function parse_wf_entity that simulates a well-formedness
 * parser.
 *
 * Revision 1.3  1999/09/01 16:26:36  gerd
 * 	Added an empty line. This is *really* a big change.
 *
 * Revision 1.2  1999/08/14 22:20:27  gerd
 *         The "config" slot has now a component "warner"which is
 * an object with a "warn" method. This is used to warn about characters
 * that cannot be represented in the Latin 1 alphabet.
 *         Furthermore, there is a new component "debugging_mode".
 *
 * Revision 1.1  1999/08/10 00:35:52  gerd
 * 	Initial revision.
 *
 *
 *)
