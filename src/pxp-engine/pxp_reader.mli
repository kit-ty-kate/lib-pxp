(* $Id: pxp_reader.mli,v 1.9 2001/07/01 08:35:23 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* Purpose of this module: The Pxp_reader module allows you to exactly
 * specify how external identifiers (SYSTEM or PUBLIC) are mapped to
 * files or channels. This is normally only necessary for advanced
 * configurations, as the functions from_file, from_channel, and
 * from_string in Pxp_yacc often suffice.
 *
 * There are two ways to use this module. First, you can compose the
 * desired behaviour by combining several predefined resolver objects
 * or functions. See the example section at the end of the file.
 * Second, you can inherit from the classes (or define a resolver class
 * from scratch). I hope this is seldom necessary as this way is much
 * more complicated; however it allows you to implement any magic.
 *)


open Pxp_types;;

exception Not_competent;;
  (* Raised by the 'open_in' method if the object does not know how to
   * handle the passed external ID.
   *)

exception Not_resolvable of exn;;
  (* Indicates that one resolver was competent, but there was an error
   * while resolving the external ID. The passed exception explains the
   * reason.
   * Not_resolvable(Not_found) serves as indicator for an unknown reason.
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
     * forbidden to open a resolver again while it is open.
     * It is allowed to close a resolver several times: If 'close_in' is
     * invoked while the resolver is already closed, nothing happens.
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

    method close_all : unit
      (* Closes this resolver and every clone *)

  end
;;

(* Note: resolve_general is no longer exported. In most cases, the classes
 * resolve_read_any_channel or resolve_read_any_string are applicable, too,
 * and much easier to configure.
 *)


(* The next classes are resolvers for concrete input sources. *)

class resolve_read_this_channel :
  ?id:ext_id -> ?fixenc:encoding -> ?close:(in_channel -> unit) ->
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
   * When the resolver is closed, the function passed by the ~close
   * argument is called. By default, the channel is closed
   * (i.e. the default is: ~close:close_in).
   *)


class resolve_read_any_channel :
  ?close:(in_channel -> unit) ->
  channel_of_id:(ext_id -> (in_channel * encoding option)) ->
  unit ->
  resolver;;

  (* resolve_read_any_channel f_open ():
   * This resolver calls the function f_open to open a new channel for
   * the passed ext_id. This function must either return the channel and
   * the encoding, or it must fail with Not_competent.
   * The function must return None as encoding if the default mechanism to
   * recognize the encoding should be used. It must return Some e if it is
   * already known that the encoding of the channel is e.
   * When the resolver is closed, the function passed by the ~close
   * argument is called. By default, the channel is closed
   * (i.e. the default is: ~close:close_in).
   *)


class resolve_read_url_channel :
  ?base_url:Neturl.url ->
  ?close:(in_channel -> unit) ->
  url_of_id:(ext_id -> Neturl.url) ->
  channel_of_url:(ext_id -> Neturl.url -> (in_channel * encoding option)) ->
  unit ->
    resolver;;

  (* resolve_read_url_channel url_of_id channel_of_url ():
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
   * the specified resource. However, there is a difference: A Not_competent
   * from ~url_of_id is left as it is, but a Not_competent from ~channel_of_url
   * is converted to Not_resolvable. So only ~url_of_id decides which URLs
   * are accepted by the resolver and which not.
   *
   * The function ~channel_of_url must return None as encoding if the default
   * mechanism to recognize the encoding should be used. It must return
   * Some e if it is already known that the encoding of the channel is e.
   *
   * When the resolver is closed, the function passed by the ~close
   * argument is called. By default, the channel is closed
   * (i.e. the default is: ~close:close_in).
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
  string_of_id:(ext_id -> (string * encoding option)) -> unit -> resolver;;

  (* resolver_read_any_string f_open ():
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
  ?map_private_id:  (private_id -> Neturl.url) ->
  ?open_private_id: (private_id -> in_channel * encoding option) ->
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
   * Options ~map_private_id and ~open_private_id: These must always be
   * used together. They specify an exceptional behaviour in case a private
   * ID is to be opened. map_private_id maps the private ID to an URL
   * (or raises Not_competent). However, instead of opening the URL 
   * the function open_private_id is called to get an in_channel to read
   * from and to get the character encoding. The URL is taken into account
   * when subsequently relative SYSTEM IDs must be resolved.
   *)

val make_file_url :
  ?system_encoding:encoding ->
  ?enc:encoding ->
  string ->
    Neturl.url;;

(* This is a convenience function to create a file URL (for localhost).
 * The argument is the file name encoded in the character set enc.
 * Relative file names are automatically converted to absolute names
 * by prepending Sys.getcwd() to the passed file name.
 *
 * ~system_encoding: Specifies the encoding of file names of
 *     the local file system. Default: UTF-8. (This argument is
 *     necessary to interpret Sys.getcwd() correctly.)
 * ~enc: The encoding of the passed string. Defaults to `Enc_utf8
 *
 * Note: To get a string representation of the URL, apply
 * Neturl.string_of_url to the result.
 *)



(* The following classes and functions create resolvers for catalogs
 * of PUBLIC or SYSTEM identifiers.
 *)

class lookup_public_id :
  (string * resolver) list ->    (* catalog *)
    resolver;;

  (* This is the generic builder for PUBLIC id catalog resolvers: The catalog 
   * argument specifies pairs (pubid, r) mapping PUBLIC identifiers to
   * subresolvers.
   * The subresolver is invoked if an entity with the corresponding PUBLIC
   * id is to be opened.
   *)



val lookup_public_id_as_file :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;

  (* Makes a resolver for PUBLIC identifiers. The catalog argument specifies
   * pairs (pubid, filename) mapping PUBLIC identifiers to filenames. The
   * filenames must already be encoded in the character set the system uses
   * for filenames.
   *
   * ~fixenc: Overrides the encoding of the file contents. By default, the
   *     standard rule is applied to find out the encoding of the file.
   *)


val lookup_public_id_as_string :
  ?fixenc:encoding ->
  (string * string) list ->    (* catalog *)
    resolver;;

  (* Makes a resolver for PUBLIC identifiers. The catalog argument specifies
   * pairs (pubid, text) mapping PUBLIC identifiers to XML text (which must
   * begin with <?xml ...?>).
   *
   * ~fixenc: Overrides the encoding of the strings.
   *)


class lookup_system_id :
  (string * resolver) list ->    (* catalog *)
    resolver;;

  (* This is the generic builder for SYSTEM id catalog resolvers: The catalog 
   * argument specifies pairs (pubid, r) mapping PUBLIC identifiers to 
   * subresolvers.
   * The subresolver is invoked if an entity with the corresponding SYSTEM
   * id is to be opened.
   *
   * Important note: Two SYSTEM IDs are considered as equal if they are
   * equal in their string representation. (This may not what you want
   * and may cause trouble... However, I currently do not know how to
   * implement a "sematical" comparison logic.)
   *)


val lookup_system_id_as_file :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;

  (* Looks up resolvers for SYSTEM identifiers: The catalog argument specifies
   * pairs (sysid, filename) mapping SYSTEM identifiers to filenames. The
   * filenames must already be encoded in the character set the system uses
   * for filenames.
   *
   * ~fixenc: Overrides the encoding of the file contents. By default, the
   *     standard rule is applied to find out the encoding of the file.
   *)


val lookup_system_id_as_string :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;

  (* Looks up resolvers for SYSTEM identifiers: The catalog argument specifies
   * pairs (sysid, text) mapping SYSTEM identifiers to XML text (which must
   * begin with <?xml ...?>).
   *
   * ~fixenc: Overrides the encoding of the strings.
   *)


type combination_mode =
    Public_before_system    (* Try public identifiers first *)
  | System_before_public    (* Try system identifiers first *)
;;

class combine : 
        ?prefer:resolver -> 
	?mode:combination_mode ->
	resolver list -> 
	  resolver;;

  (* Combines several resolver objects. If a concrete entity with an
   * ext_id is to be opened, the combined resolver tries the contained
   * resolvers in turn until a resolver accepts opening the entity
   * (i.e. it does not raise Not_competent on open_in).
   *
   * If the ext_id is a public identifier Public(pubid,sysid), there are 
   * two possibilities:
   * (1) Try first to open as public identifier, and if that fails,
   *     fall back to the system identifier
   * (2) Try first to open as system identifier, and if that fails,
   *     fall back to the public identifier
   * You can select this by the ~mode argument. The default is to
   * try public identifiers first.
   *
   * Clones: If the 'clone' method is invoked before 'open_in', all contained
   * resolvers are cloned and again combined. If the 'clone' method is
   * invoked after 'open_in' (i.e. while the resolver is open), only the
   * active resolver is cloned.
   *
   * ~prefer: This is an internally used option.
   *)

(* ====================================================================== *)

(* EXAMPLES OF RESOLVERS:
 *
 * let r1 = new resolve_as_file ()
 *   - r1 can open all local files
 *
 * let r2 = new resolve_read_this_channel
 *            ~id:(System "file:/dir/f.xml")
 *            (open_in "/dir/f.xml")
 *   - r2 can only read /dir/f.xml of the local file system. If this file
 *     contains references to other files, r2 will fail.
 *     Note that the channel is automatically closed after XML parsing
 *     is done.
 *
 * let r3 = new combine [ r2; r1 ]
 *   - r3 reads /dir/f.xml of the local file system by calling r2, and all
 *     other files by calling r1. However, inner references within 
 *     /dir/f.xml still fail.
 *
 * let pid = Pxp_types.allocate_private_id() in
 * let r4 = new resolve_read_this_channel 
 *                ~id:(Private pid) 
 *                (open_in "/dir/f.xml")
 *   - r4 can only read from a so-called private ID. These are opaque
 *     identifiers that can be mapped to channels and files as needed.
 *     They do not have a textual representation, and they cannot be
 *     referred to from XML text.
 *
 * ----------------------------------------------------------------------
 * 
 * Now a bigger example. The task is to:
 *  - resolve the PUBLIC IDs P and Q to some files;
 *  - resolve the SYSTEM ID "http://r/s.dtd" to another file;
 *  - resolve all file SYSTEM IDs
 *  - start parsing with "f.xml" in the current directory
 *
 * let r =
 *   new combine 
 *     [ lookup_public_id_as_file 
 *         [ "P", "file_for_p";   "Q", "file_for_q" ];
 *       lookup_system_id_as_file
 *         [ "http://r/s.dtd", "file_for_this_dtd" ];
 *       new resolve_as_file()
 *     ]
 * in
 * (* The recommended way to create the start_id from file names: *)
 * let start_url =
 *   make_file_url "f.xml" in
 * let start_id = 
 *   System (Neturl.string_of_url url) in
 * let source = ExtID(start_id, r) in
 * parse_document_entity ... source ...
 *
 * ----------------------------------------------------------------------
 *
 * A variation:
 *
 *  - resolve the PUBLIC IDs P and Q to some files;
 *  - resolve the SYSTEM ID "http://r/s.dtd" to another file;
 *  - do not resolve any file URL
 *  - start parsing with "f.xml" in the current directory
 *
 * let start_id = allocate_private_id() in
 * let r =
 *   new combine 
 *     [ lookup_public_id_as_file 
 *         [ "P", "file_for_p";   "Q", "file_for_q" ];
 *       lookup_system_id_as_file
 *         [ "http://r/s.dtd", "file_for_this_dtd" ];
 *       resolve_read_any_channel
 *         ~channel_of_id: (fun xid ->
 *            if xid = start_id then 
 *              open_in_bin "f.xml", None  (* you may want to catch Sys_error *)
 *            else raise Not_competent)
 *         ();
 *     ]
 * in
 * let source = ExtID(start_id, r) in
 * parse_document_entity ... source ...
 *
 * ----------------------------------------------------------------------
 *
 * Three further examples can be found in the source of Pxp_yacc (file
 * pxp_yacc.m2y): the implementations of from_file, from_channel, and
 * from_string are also applications of the Pxp_reader objects.
 *)


(* ======================================================================
 * History:
 *
 * $Log: pxp_reader.mli,v $
 * Revision 1.9  2001/07/01 08:35:23  gerd
 * 	Instead of the ~auto_close argument, there is now a
 * ~close argument for several functions/classes. This allows some
 * additional action when the resolver is closed.
 *
 * Revision 1.8  2001/04/22 14:16:48  gerd
 * 	resolve_as_file: you can map private IDs to arbitrary channels.
 * 	resolve_read_url_channel: changed type of the channel_of_url
 * argument (ext_id is also passed)
 * 	More examples and documentation.
 *
 * Revision 1.7  2001/04/03 20:22:44  gerd
 * 	New resolvers for catalogs of PUBLIC and SYSTEM IDs.
 * 	Improved "combine": PUBLIC and SYSTEM IDs are handled
 * separately.
 * 	Rewritten from_file: Is now a simple application of the
 * Pxp_reader classes and functions. (The same has still to be done
 * for from_channel!)
 *
 * Revision 1.6  2001/02/01 20:38:49  gerd
 * 	New support for PUBLIC identifiers.
 *
 * Revision 1.5  2000/07/09 01:05:33  gerd
 * 	New methode 'close_all' that closes the clones, too.
 *
 * Revision 1.4  2000/07/08 16:24:56  gerd
 * 	Introduced the exception 'Not_resolvable' to indicate that
 * 'combine' should not try the next resolver of the list.
 *
 * Revision 1.3  2000/07/06 23:04:46  gerd
 * 	Quick fix for 'combine': The active resolver is "prefered",
 * but the other resolvers are also used.
 *
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
