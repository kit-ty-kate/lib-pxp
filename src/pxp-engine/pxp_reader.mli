(* $Id: pxp_reader.mli,v 1.2 2000/07/04 22:06:49 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types;;

exception Not_competent;;
  (* Raised by the 'open_in' method if the object does not know how to 
   * handle the passed external ID.
   *)


(* The class type 'resolver' is the official type of all "resolvers". 
 * Resolvers take file names (or better, external identifiers) and 
 * return lexbufs, scanning the file for tokens. Resolvers may be
 * cloned, and clones can interpret relative file names relative to
 * their creator.
 *
 * Example of the latter:
 *
 * Resolver r reads from file:/dir/f1.xml
 *
 * <tag>some XML text
 * &e;                       -----> Entity e is bound to "subdir/f2.xml"
 * </tag>                           Step (1): let r' = "clone of r"
 *                                  Step (2): open file "subdir/f2.xml"
 *
 * r' must still know the directory of the file r is reading, otherwise
 * it would not be able to resolve "subdir/f2.xml" = "file:/dir/subdir/f2.xml".
 *
 * Actually, this example can be coded as:
 *
 * let r = new resolve_as_file in
 * let lbuf = r # open_in "file:/dir/f1.xml" in
 * ... read from lbuf ...
 * let r' = r # clone in
 * let lbuf' = r' # open_in "subdir/f2.xml" in
 * ... read from lbuf' ...
 * r' # close_in;
 * ... read from lbuf ...
 * r # close_in;
 *)

class type resolver =
  object
    (* A resolver can open an input source, and returns this source as
     * Lexing.lexbuf.
     *
     * After creating a resolver, one must invoke the two methods
     * init_rep_encoding and init_warner to set the internal encoding of 
     * strings and the warner object, respectively. This is normally
     * done by the parsing functions in Pxp_yacc.
     * It is not necessary to invoke these two methods for a fresh
     * clone.
     *
     * It is possible that the character encoding of the source and the
     * internal encoding of the parser are different. To cope with this,
     * one of the tasks of the resolver is to recode the characters of
     * the input source into the internal character encoding.
     *
     * Note that there are several ways of determining the encoding of the
     * input: (1) It is possible that the transport protocol (e.g. HTTP)
     * transmits the encoding, and (2) it is possible to inspect the beginning
     * of the file, and to analyze:
     * (2.1) The first two bytes indicate whether UTF-16 is used
     * (2.2) Otherwise, one can assume that an ASCII-compatible character
     *       set is used. It is now possible to read the XML declaration
     *       <?xml ... encoding="xyz" ...?>. The encoding found here is
     *       to be used.
     * (2.3) If the XML declaration is missing, the encoding is UTF-8.
     * The resolver needs only to distinguish between cases (1), (2.1),
     * and the rest.
     * The details of analyzing whether (2.2) or (2.3) applies are programmed 
     * elsewhere, and the resolver will be told the result (see below).
     *
     * A resolver is like a file: it must be opened before one can work
     * with it, and it should be closed after all operations on it have been
     * done. The method 'open_in' is called with the external ID as argument
     * and it must return the lexbuf reading from the external resource.
     * The method 'close_in' does not require an argument.
     *
     * It is allowed to re-open a resolver after it has been closed. It is
     * forbidden to open a resolver again while it is open, or to close
     * a resolver after it has been closed (or before it is opened).
     *
     * The method 'open_in' may raise Not_competent to indicate that this
     * resolver is not able to open this type of IDs.
     *
     * The method 'change_encoding' is called from the parser after the
     * analysis of case (2) has been done; the argument is either the
     * string name of the encoding, or the empty string to indicate
     * that no XML declaration was found. It is guaranteed that 
     * 'change_encoding' is invoked after only a few tokens of the 
     * file. The resolver should react as follows:
     * - If case (1) applies:   Ignore the encoding passed to 'change_encoding'.
     * - If case (2.1) applies: The encoding passed to 'change_encoding' must
     *                          be compatible with UTF-16. This should be
     *                          checked, and violations should be reported.
     * - Else:                  If the passed encoding is "", assume UTF-8.
     *                          Otherwise, assume the passed encoding.
     *
     * The following rule helps synchronizing the lexbuf with the encoding:
     * If the resolver has been opened, but 'change_encoding' has not yet
     * been invoked, the lexbuf contains at most one character (which may
     * be represented by multiple bytes); i.e. the lexbuf is created by
     * Lexing.from_function, and the function puts only one character into
     * the buffer at once.
     * After 'change_encoding' has been invoked, there is no longer a limit
     * on the lexbuf size.
     *
     * The reason for this rule is that you know exactly the character where
     * the encoding changes to the encoding passed by 'change_encoding'.
     *
     * The method 'clone' may be invoked for open or closed resolvers.
     * Basically, 'clone' returns a new resolver which is always closed.
     * If the original resolver is closed, the clone is simply a clone.
     * If the original resolver is open at the moment of cloning:
     * If the clone is later opened for a relative system ID (i.e. relative
     * URL), the clone must interpret this ID relative to the ID of the
     * original resolver.
     *)
    method init_rep_encoding : rep_encoding -> unit
    method init_warner : collect_warnings -> unit

    method rep_encoding : rep_encoding

    method open_in : ext_id -> Lexing.lexbuf
      (* May raise Not_competent if the object does not know how to handle
       * this ext_id.
       *)
    method close_in : unit
    method change_encoding : string -> unit


    (* Every resolver can be cloned. The clone does not inherit the connection
     * with the external object, i.e. it is initially closed.
     *)
    method clone : resolver

  end
;;

(* Note: resolve_general is no longer exported. In most cases, the classes
 * resolve_read_any_channel or resolve_read_any_string are applicable, too,
 * and much easier to configure.
 *)


(* The next classes are resolvers for concrete input sources. *)

class resolve_read_this_channel : 
  ?id:ext_id -> ?fixenc:encoding -> ?auto_close:bool -> 
  in_channel -> resolver;;

  (* Reads from the passed channel (it may be even a pipe). If the ~id
   * argument is passed to the object, the created resolver accepts only
   * this ID. Otherwise all IDs are accepted.
   * Once the resolver has been cloned, it does not accept any ID. This
   * means that this resolver cannot handle inner references to external
   * entities. Note that you can combine this resolver with another resolver
   * that can handle inner references (such as resolve_as_file); see
   * class 'combine' below.
   * If you pass the ~fixenc argument, the encoding of the channel is
   * set to the passed value, regardless of any auto-recognition or
   * any XML declaration.
   * If ?auto_close = true (which is the default), the channel is
   * closed after use. If ?auto_close = false, the channel is left open.
   *)


class resolve_read_any_channel : 
  ?auto_close:bool -> 
  channel_of_id:(ext_id -> (in_channel * encoding option)) -> 
  resolver;;

  (* resolve_read_any_channel f_open:
   * This resolver calls the function f_open to open a new channel for
   * the passed ext_id. This function must either return the channel and
   * the encoding, or it must fail with Not_competent.
   * The function must return None as encoding if the default mechanism to
   * recognize the encoding should be used. It must return Some e if it is
   * already known that the encoding of the channel is e.
   * If ?auto_close = true (which is the default), the channel is
   * closed after use. If ?auto_close = false, the channel is left open.
   *)


class resolve_read_url_channel :
  ?base_url:Neturl.url ->
  ?auto_close:bool -> 
  url_of_id:(ext_id -> Neturl.url) -> 
  channel_of_url:(Neturl.url -> (in_channel * encoding option)) -> 
    resolver;;

  (* resolve_read_url_channel url_of_id channel_of_url:
   *
   * When this resolver gets an ID to read from, it calls the function
   * ~url_of_id to get the corresponding URL. This URL may be a relative
   * URL; however, a URL scheme must be used which contains a path.
   * The resolver converts the URL to an absolute URL if necessary.
   * The second function, ~channel_of_url, is fed with the absolute URL
   * as input. This function opens the resource to read from, and returns
   * the channel and the encoding of the resource.
   *
   * Both functions, ~url_of_id and ~channel_of_url, can raise
   * Not_competent to indicate that the object is not able to read from
   * the specified resource.
   *
   * The function ~channel_of_url must return None as encoding if the default 
   * mechanism to recognize the encoding should be used. It must return
   * Some e if it is already known that the encoding of the channel is e.
   *
   * If ?auto_close = true (which is the default), the channel is
   * closed after use. If ?auto_close = false, the channel is left open.
   * 
   * Objects of this class contain a base URL relative to which relative
   * URLs are interpreted. When creating a new object, you can specify
   * the base URL by passing it as ~base_url argument. When an existing
   * object is cloned, the base URL of the clone is the URL of the original
   * object.
   *
   * Note that the term "base URL" has a strict definition in RFC 1808.
   *)


class resolve_read_this_string : 
  ?id:ext_id -> ?fixenc:encoding -> string -> resolver;;

  (* Reads from the passed string. If the ~id
   * argument is passed to the object, the created resolver accepts only
   * this ID. Otherwise all IDs are accepted.
   * Once the resolver has been cloned, it does not accept any ID. This
   * means that this resolver cannot handle inner references to external
   * entities. Note that you can combine this resolver with another resolver
   * that can handle inner references (such as resolve_as_file); see
   * class 'combine' below.
   * If you pass the ~fixenc argument, the encoding of the string is
   * set to the passed value, regardless of any auto-recognition or
   * any XML declaration.
   *)


class resolve_read_any_string : 
  string_of_id:(ext_id -> (string * encoding option)) -> resolver;;

  (* resolver_read_any_string f_open:
   * This resolver calls the function f_open to get the string for
   * the passed ext_id. This function must either return the string and
   * the encoding, or it must fail with Not_competent.
   * The function must return None as encoding if the default mechanism to
   * recognize the encoding should be used. It must return Some e if it is
   * already known that the encoding of the string is e.
   *)


class resolve_as_file :
  ?file_prefix:[ `Not_recognized | `Allowed | `Required ] ->
  ?host_prefix:[ `Not_recognized | `Allowed | `Required ] ->
  ?system_encoding:encoding ->
  ?url_of_id:(ext_id -> Neturl.url) -> 
  ?channel_of_url: (Neturl.url -> (in_channel * encoding option)) ->
  unit -> 
  resolver;;

  (* Reads from the local file system. Every file name is interpreted as
   * file name of the local file system, and the referred file is read.
   *
   * The full form of a file URL is: file://host/path, where
   * 'host' specifies the host system where the file identified 'path'
   * resides. host = "" or host = "localhost" are accepted; other values
   * will raise Not_competent. The standard for file URLs is 
   * defined in RFC 1738.
   *
   * Option ~file_prefix: Specifies how the "file:" prefix of file names
   * is handled:
   * `Not_recognized:  The prefix is not recognized.
   * `Allowed:         The prefix is allowed but not required (the default).
   * `Required:        The prefix is required.
   *
   * Option ~host_prefix: Specifies how the "//host" phrase of file names
   * is handled:
   * `Not_recognized:  The phrase is not recognized.
   * `Allowed:         The phrase is allowed but not required (the default).
   * `Required:        The phrase is required.
   *
   * Option ~system_encoding: Specifies the encoding of file names of
   * the local file system. Default: UTF-8.
   *
   * Options ~url_of_id, ~channel_of_url: Not for the end user!
   *)


class combine : resolver list -> resolver;;
  
  (* Combines several resolver objects. If a concrete entity with an
   * ext_id is to be opened, the combined resolver tries the contained
   * resolvers in turn until a resolver accepts opening the entity
   * (i.e. it does not raise Not_competent on open_in).
   *
   * Clones: If the 'clone' method is invoked before 'open_in', all contained
   * resolvers are cloned and again combined. If the 'clone' method is 
   * invoked after 'open_in' (i.e. while the resolver is open), only the
   * active resolver is cloned.
   *) 

(* EXAMPLES OF RESOLVERS:
 *
 * let r1 = new resolve_as_file
 *   - r1 can open all local files
 *
 * let r2 = new resolve_read_this_channel 
 *            ~id:"file:/dir/f.xml" 
 *            (open_in "/dir/f.xml")
 *   - r2 can only read /dir/f.xml of the local file system. If this file
 *     contains references to other files, r2 will fail
 *
 * let r3 = new combine [ r2; r1 ]
 *   - r3 reads /dir/f.xml of the local file system by calling r2, and all
 *     other files by calling r1
 *)


(* ======================================================================
 * History:
 * 
 * $Log: pxp_reader.mli,v $
 * Revision 1.2  2000/07/04 22:06:49  gerd
 * 	MAJOR CHANGE: Complete redesign of the reader classes.
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
