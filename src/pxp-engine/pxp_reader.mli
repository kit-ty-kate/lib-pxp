(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* Purpose of this module: The Pxp_reader module allows you to exactly
 * specify how external identifiers (SYSTEM or PUBLIC) are mapped to
 * files or channels. This is normally only necessary for advanced
 * configurations, as the functions from_file, from_channel, and
 * from_string in Pxp_types often suffice.
 *
 * There are two ways to use this module. First, you can compose the
 * desired behaviour by combining several predefined resolver objects
 * or functions. See the example section at the end of the file.
 * Second, you can inherit from the classes (or define a resolver class
 * from scratch). I hope this is seldom necessary as this way is much
 * more complicated; however it allows you to implement any magic.
 *)


open Pxp_core_types;;

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


module ULB : sig
  type unicode_lexbuf =
      private
	{ mutable ulb_encoding : encoding;
                (* The character encoding of [ulb_rawbuf] *)
	  mutable ulb_encoding_start : int;
	        (* The first character position to which [ulb_encoding]
		 * applies
		 *)
	  mutable ulb_rawbuf : string;
                (* The encoded string to analyse *)
	  mutable ulb_rawbuf_len : int;
                (* The filled part of [ulb_rawbuf] *)
	  mutable ulb_rawbuf_end : int;
                (* The analysed part of [ulb_rawbuf]. We have always
	         * [ulb_rawbuf_end <= ulb_rawbuf_len]. The analysed part
	         * may be shorter than the filled part because there is
	         * not enough space in [ulb_chars], or because the filled
	         * part ends with an incomplete multi-byte character
	         *)
	  mutable ulb_rawbuf_const : bool;
	        (* Whether [ulb_rawbuf] is considered as a constant. If
		 * [true], it is never blitted.
		 *)
	  mutable ulb_chars : int array;
                (* The analysed part of [ulb_rawbuf] as array of Unicode
	         * code points. Only the positions 0 to [ulb_chars_len-1]
	         * of the array are filled.
	         *)
	  mutable ulb_chars_pos : int array;
                (* For every analysed character this array stores the
   	         * byte position where the character begins in [ulb_rawbuf].
	         * In addition, the array contains at [ulb_chars_len] the
	         * value of [ulb_rawbuf_end].
		 *
	         * This array is one element longer than [ulb_chars].
	         *)
	  mutable ulb_chars_len : int;
                (* The filled part of [ulb_chars] *)
	  mutable ulb_eof : bool;
                (* Whether EOF has been seen *)
	  mutable ulb_refill : string -> int -> int -> int;
	          (* The refill function *)
	  mutable ulb_enc_change_hook : unicode_lexbuf -> unit
	          (* This function is called when the encoding changes *)
	}

  val from_function : 
    ?raw_size:int -> 
    ?char_size:int -> 
    ?enc_change_hook:(unicode_lexbuf -> unit) ->
    refill:(string -> int -> int -> int) ->
    encoding -> 
      unicode_lexbuf
  (** Creates a [unicode_lexbuf] to analyse strings of the 
   * passed [encoding] coming from the [refill] function.
   *
   * @param raw_size The initial size for [ulb_rawbuf]. Defaults to 512
   * @param char_size The initial size for [ulb_chars]. Defaults to 256
   * @param enc_change_hook This function is called when the encoding
   *   is changed, either by this module, or by the user
   * @param refill This function is called with arguments [ulb_rawbuf],
   *   [ulb_rawbuf_len], and [l], where 
   *   [l = String.length ulb_rawbuf - ulb_rawbuf_len] is the free
   *   space in the buffer. The function should fill new bytes into
   *   this substring, and return the number of added bytes. The
   *   return value 0 signals EOF.
   *)

  val from_string :
        ?enc_change_hook:(unicode_lexbuf -> unit) ->
        encoding -> string -> unicode_lexbuf
  (** Creates a [unicode_lexbuf] analysing the passed string encoded in
   * the passed encoding. This function copies the input string.
   *
   * @param enc_change_hook This function is called when the encoding
   *   is changed, either by this module, or by the user
   *)

  val from_string_inplace :
        ?enc_change_hook:(unicode_lexbuf -> unit) ->
        encoding -> string -> unicode_lexbuf
  (** Creates a [unicode_lexbuf] analysing the passed string encoded in
   * the passed encoding. This function does not copy the input string,
   * but uses it directly as [ulb_rawbuf]. The string is not modified by ULB,
   * but the caller must ensure that other program parts do not
   * modify it either.
   *
   * @param enc_change_hook This function is called when the encoding
   *   is changed, either by this module, or by the user
   *)

(*
  val append :
        src:(int array) -> ?pos:int -> ?len:int -> unicode_lexbuf -> unit
  -- better we have something like from_unicode that refills directly into
     the ulb_chars array.
*)
  (** Appends a sub array of [src] to the [unicode_lexbuf].
   * When the buffer is already at EOF, the function fails.
   *
   * @param pos Where the sub array begins, by default 0
   * @param len The length of the sub array, by default [Array.length - pos]
   *)

  val delete :
        int -> unicode_lexbuf -> unit
  (** Deletes the number of characters from [unicode_lexbuf].
   *  These characters
   *  are removed from the beginning of the buffer, i.e.
   *  [ulb_chars.(n)] becomes the new first character of the
   *  buffer. All three buffers [ulb_rawbuf], [ulb_chars], and
   *  [ulb_chars_pos] are blitted as necessary.       
   *
   *  When the buffer is already at EOF, the function fails.
   *
   *  For efficiency, it should be tried to call [delete] as seldom as
   *  possible. Its speed is linear to the number of characters to move.
   *)

  val refill :
	unicode_lexbuf ->
	  unit
  (** Tries to add characters to the [unicode_lexbuf] by calling the 
   * [ulb_refill] function. When the buffer is already at EOF, the 
   * exception [End_of_file] is raised, and the buffer is not modified.
   * Otherwise, the [ulb_refill] function is called to
   * add new characters. If necessary, [ulb_rawbuf], [ulb_chars], and
   * [ulb_chars_pos] are enlarged such that it is ensured that either
   * at least one new character is added, or that EOF is found.
   * In the latter case, [ulb_eof] is set to [true] (and the next call
   * of [refill_unicode_lexbuf] will raise [End_of_file]).
   *)

  val set_encoding :
        encoding -> unicode_lexbuf -> unit
   (** Sets the [encoding] to the passed value. This only affects future
    * [refill] calls.
    *)

  val close :
        unicode_lexbuf -> unit
  (** Sets [ulb_eof] of the [unicode_lexbuf]. The rest of the buffer
   * is not modified
   *)

  val utf8_sub_string : int -> int -> unicode_lexbuf -> string
    (** The two [int] arguments are the position and length of a sub
     * string of the lexbuf that is returned as UTF8 string. Position
     * and length are given as character multiples.
     *)

  val utf8_sub_string_length : int -> int -> unicode_lexbuf -> int
    (** Returns [String.length(utf8_sub_string args)]. Tries not to
     * allocate the UTF-8 string.
     *)

end (* module ULB *)

module Ulexing : sig
  type lexbuf
  exception Error
  val from_ulb_lexbuf : ULB.unicode_lexbuf -> lexbuf
  val lexeme_start: lexbuf -> int
  val lexeme_end: lexbuf -> int
  val lexeme_length: lexbuf -> int
  val lexeme: lexbuf -> int array
  val lexeme_char: lexbuf -> int -> int
  val sub_lexeme: lexbuf -> int -> int -> int array
  val utf8_lexeme: lexbuf -> string
  val utf8_sub_lexeme: lexbuf -> int -> int -> string
  val utf8_sub_lexeme_length: lexbuf -> int -> int -> int
    
  (* "Internal" interface *)
  val start: lexbuf -> unit
  val next: lexbuf -> int
  val mark: lexbuf -> int -> unit
  val backtrack: lexbuf -> int
end


	  
(* One must only use either [lsrc_lexbuf], or [lsrc_unicode_lexbuf] ! *)
type lexer_source =
    { lsrc_lexbuf : Lexing.lexbuf Lazy.t;
      lsrc_unicode_lexbuf : ULB.unicode_lexbuf Lazy.t;
    }


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
     * done. The method 'open_rid' is called with the resolver ID as argument
     * and it must return the lexbuf reading from the external resource.
     * (There is also the old method 'open_in' that expects an ext_id as
     * argument. It is less powerful and should not be used any longer.)
     * The method 'close_in' does not require an argument.
     *
     * It is allowed to re-open a resolver after it has been closed. It is
     * forbidden to open a resolver again while it is open.
     * It is allowed to close a resolver several times: If 'close_in' is
     * invoked while the resolver is already closed, nothing happens.
     *
     * The method 'open_rid' may raise Not_competent to indicate that this
     * resolver is not able to open this type of IDs.
     *
     * If 'open_rid' gets a PUBLIC ID, it can be assumed that the string
     * is already normalized (whitespace).
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
    method init_warner : symbolic_warnings option -> collect_warnings -> unit

    method rep_encoding : rep_encoding

    method open_in : ext_id -> lexer_source
      (* This is the old method to open a resolver. It is superseded by
       * open_rid.
       * This method may raise Not_competent if the object does not know
       * how to handle this ext_id.
       *
       * PXP 1.2: Returns now a lexer_source, no longer a lexbuf
       *)

    method open_rid : resolver_id -> lexer_source
      (* This is the new method to open a resolver. It takes a resolver ID
       * instead of an ext_id but works in the same way.
       *
       * PXP 1.2: Returns now a lexer_source, no longer a lexbuf
       *)

    method close_in : unit
    method change_encoding : string -> unit


    (* Every resolver can be cloned. The clone does not inherit the connection
     * with the external object, i.e. it is initially closed.
     *)
    method clone : resolver

    method active_id : resolver_id
      (* Returns the actually used resolver ID. This is the ID passed to
       * open_rid where unused components have been set to None. The
       * resolver ID returned by [active_id] plays an important role when
       * expanding relative URLs.
       *)

(*  method close_all : unit *)
      (* Closes this resolver and every clone *)
      (* This method is no longer supported in PXP 1.2 *)

  end
;;


(* The next classes are resolvers for concrete input sources. *)

(* CHANGES IN PXP 1.2:
 *
 * All resolve_read_* classes are now deprecated. The new classes 
 * resolve_to_* base on the Netchannels classes as generalization of
 * input streams.
 *
 * Examples: To read from an in_channel, use:
 *
 *   let obj_channel = new Netchannels.input_channel in_channel in
 *   new Pxp_reader.resolve_to_this_obj_channel obj_channel
 *
 * To read from a string, use:
 *
 *   let obj_channel = new Netchannels.input_string string in
 *   new Pxp_reader.resolve_to_this_obj_channel obj_channel
 *
 * Furthermore, the new classes use the resolver_id record as generalized
 * names for entities. This solves most problems with relative URLs.
 *
 * The "Anonymous" ID: In previous versions of PXP, a resolver bound to
 * the Anonymous ID matched the Anonymous ID. This is no longer true.
 * The algebra has been changed such that Anonymous never matches, not 
 * even itself.
 * 
 *   Example: The new resolver
 *     let r = new resolve_to_this_obj_channel ~id:Anonymous ch 
 *   will never accept any ID. In contrast to this, the old, and now
 *   deprecated resolver
 *     let r' = new resolve_read_this_channel ~id:Anonymous ch
 *   accepted the ID Anonymous in previous versions of PXP.
 *
 * The rationale behind this change is that Anonymous acts now like 
 * an "empty set", and not like a concrete element. You can use Private
 * to create as many concrete elements as you want, so there is actually
 * no need for the old behaviour of Anonymous.
 *
 * Note that even the resolver classes provided for backwards compatibility
 * implement this change (to limit the confusion). This means that you
 * might have to change your application to use Private instead of 
 * Anonymous.
 * 
 *)

class resolve_to_this_obj_channel :
  ?id:ext_id ->
  ?rid:resolver_id ->
  ?fixenc:encoding ->
  ?close:(Netchannels.in_obj_channel -> unit) ->
 Netchannels.in_obj_channel -> 
   resolver;;

  (* Reads from the passed in_obj_channel. If the ~id or ~rid arguments
   * are passed to the object, the created resolver accepts only
   * these IDs (all mentioned private, system, or public IDs). Otherwise 
   * all IDs are accepted, even Anonymous.
   *
   * This resolver can only be used once (because the in_obj_channel
   * can only be used once). If it is opened a second time (either
   * in the base object or a clone), it will raise Not_competent.
   *
   * If you pass the ~fixenc argument, the encoding of the channel is
   * set to the passed value, regardless of any auto-recognition or
   * any XML declaration.
   *
   * When the resolver is closed, the function passed by the ~close
   * argument is called. By default, the channel is closed
   * (i.e. the default is: ~close:(fun ch -> ch # close_in)).
   *)

type accepted_id =
    Netchannels.in_obj_channel * encoding option * resolver_id option
  (* When a resolver accepts an ID, this triple specifies how to proceed.
   * The in_obj_channel is the channel to read data from, the encoding option
   * may enforce a certain character encoding, and the resolver_id option
   * may detail the ID (this ID will be returned by active_id).
   *
   * If None is passed as encoding option, the standard autodetection of
   * the encoding is performed.
   *
   * If None is passed as resolver_id option, the original ID is taken
   * unchanged.
   *)

class resolve_to_any_obj_channel :
  ?close:(Netchannels.in_obj_channel -> unit) ->
  channel_of_id:(resolver_id -> accepted_id) ->
  unit ->
  resolver;;

  (* This resolver calls the function channel_of_id to open a new channel for
   * the passed resolver_id. This function must either return the accepted_id,
   * or it must fail with Not_competent.
   *
   * When the resolver is closed, the function passed by the ~close
   * argument is called. By default, the channel is closed
   * (i.e. the default is: ~close:(fun ch -> ch # close_in)).
   *)


class resolve_to_url_obj_channel : 
  ?close:(Netchannels.in_obj_channel -> unit) ->
  url_of_id:(resolver_id -> Neturl.url) ->
  base_url_of_id:(resolver_id -> Neturl.url) ->
  channel_of_url:(resolver_id -> Neturl.url -> accepted_id) ->
  unit ->
    resolver;;

  (*
   * When this resolver gets an ID to read from, it calls the function
   * ~url_of_id to get the corresponding URL (such IDs are normally 
   * system IDs, but it is also possible to map system IDs to URLs). 
   * This URL may be a relative URL; however, a URL scheme must be used
   * which contains a path. The resolver converts the URL to an absolute 
   * URL if necessary.
   *
   * To do so, the resolver calls ~base_url_of_id to get the URL the relative
   * URL must be interpreted relative to. Usually, this function returns
   * the rid_system_base as URL. This URL must be absolute.
   *
   * The third function, ~channel_of_url, is fed with the absolute URL
   * as input. This function opens the resource to read from, and returns
   * the accepted_id like resolve_to_any_obj_channel does. The resolver ID 
   * passed to ~channel_of_url contains the string representation of the
   * absolute URL as system ID.
   *
   * Both functions, ~url_of_id and ~channel_of_url, can raise
   * Not_competent to indicate that the object is not able to read from
   * the specified resource. However, there is a difference: A Not_competent
   * from ~url_of_id is left as it is, but a Not_competent from ~channel_of_url
   * is converted to Not_resolvable. So only ~url_of_id decides which URLs
   * are accepted by the resolver and which not, and in the latter case,
   * other resolver can be tried. If ~channel_of_url raises Not_competent,
   * the whole resolution procedure will stop, and no other resolver will
   * be tried.
   *
   * When the resolver is closed, the function passed by the ~close
   * argument is called. By default, the channel is closed
   * (i.e. the default is: ~close:(fun ch -> ch # close_in())).
   *)


class resolve_as_file :
  ?file_prefix:[ `Not_recognized | `Allowed | `Required ] ->
  ?host_prefix:[ `Not_recognized | `Allowed | `Required ] ->
  ?system_encoding:encoding ->
  ?map_private_id:  (private_id -> Neturl.url) ->
  ?open_private_id: (private_id -> in_channel * encoding option) ->
  ?base_url_defaults_to_cwd: bool ->
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
   * Options ~map_private_id and ~open_private_id: THESE OPTIONS ARE
   * DEPRECATED! IT IS NOW POSSIBLE TO USE A COMBINED RESOLVER TO ACHIEVE
   * THE SAME EFFECT! - These must always be
   * used together. They specify an exceptional behaviour in case a private
   * ID is to be opened. map_private_id maps the private ID to an URL
   * (or raises Not_competent). However, instead of opening the URL 
   * the function open_private_id is called to get an in_channel to read
   * from and to get the character encoding. The URL is taken into account
   * when subsequently relative SYSTEM IDs must be resolved.
   *
   * Option ~base_url_defaults_to_cwd: If true (the default), relative URLs
   * are interpreted relative to the current working directory at the time
   * the class is instantiated, but only if there is no parent URL, i.e.
   * rid_system_base=None. If false, such URLs cannot be resolved. This 
   * option is selected by default because of backward compatibility. 
   * In general, it is better to set this option to false, and to
   * initialize rid_system_base properly.
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

class lookup_id :
  (ext_id * resolver) list ->    (* catalog *)
    resolver;;
  (* The general catalog class. The catalog argument specifies pairs (xid,r)
   * mapping external IDs xid to subresolvers r. The subresolver is invoked
   * if an entity with the corresponding xid is to be opened.
   *)


class lookup_id_as_file :
  ?fixenc:encoding ->
  (ext_id * string) list ->      (* catalog *)
    resolver;;

  (* The catalog argument specifies pairs (xid,file) mapping external IDs xid
   * to files. The file is read  if an entity with the corresponding xid is
   * to be opened.
   *
   * ~fixenc: Overrides the encoding of the file contents. By default, the
   *     standard rule is applied to find out the encoding of the file.
   *)


class lookup_id_as_string :
  ?fixenc:encoding ->
  (ext_id * string) list ->      (* catalog *)
    resolver;;

  (* The catalog argument specifies pairs (xid,s) mapping external IDs xid
   * to strings s. The string is read if an entity with the corresponding
   * xid is to be opened.
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



class lookup_public_id_as_file :
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


class lookup_public_id_as_string :
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
   * argument specifies pairs (sysid, r) mapping SYSTEM identifiers to 
   * subresolvers.
   * The subresolver is invoked if an entity with the corresponding SYSTEM
   * id is to be opened.
   *
   * Important note: Two SYSTEM IDs are considered as equal if they are
   * equal in their string representation. (This may not what you want
   * and may cause trouble... However, I currently do not know how to
   * implement a "semantic" comparison logic.)
   *)


class lookup_system_id_as_file :
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


class lookup_system_id_as_string :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;

  (* Looks up resolvers for SYSTEM identifiers: The catalog argument specifies
   * pairs (sysid, text) mapping SYSTEM identifiers to XML text (which must
   * begin with <?xml ...?>).
   *
   * ~fixenc: Overrides the encoding of the strings.
   *)


class norm_system_id : resolver -> resolver
  (* Normalizes the SYSTEM ID, and forwards the open request to the
   * passed resolver.
   *
   * Normalization includes:
   * - Relative URLs are made absolute. If this fails, the problematic
   *   relative URL will be rejected.
   * - .. and . and // are removed 
   * - Escaping of reserved characters is normalized
   *
   * Normalization is recommended for catalogs, e.g.
   * new norm_system_id
   *   (new lookup_system_id_as_file
   *      [ "http://h/p1", ...;
   *        "http://h/p2", ...;
   *      ])
   * First, the catalog now even works if the URL is written in an
   * unsual way, e.g. http://h/p1/../p2, or http://h/p%31. 
   * Second, relative URLs can be used. For instance, the document
   * referred to as http://h/p1 can now refer to the other document
   * as p2.
   *)


class rewrite_system_id :
        ?forward_unmatching_urls:bool ->
	(string * string) list ->
	resolver ->
	  resolver
  (* Rewrites the SYSTEM URL according to the list of pairs. The left
   * component is the pattern, the right component is the substitute.
   * For example,
   *
   * new rewrite_system_id
   *       [ "http://host/foo/", "file:///dir/" ]
   *       r
   *
   * rewrites all URLs beginning with http://host/foo/ to file:///dir/,
   * e.g. http://host/foo/x becomes file:///dir/x.
   *
   * If the pattern ends with a slash (as in the example), a prefix match
   * is performed, i.e. the whole directory hierarchy is rewritten.
   * If the pattern does not end with a slash, an exact match is performed,
   * i.e. only a single URL is rewritten.
   *
   * The class normalizes URLs as norm_system_id does, before the match
   * is tried.
   *
   * By default, URLs that do not match any pattern are rejected.
   *
   * The rewritten URL is only visible within the passed subresolver.
   * If the opened entity accesses other entities by relative URLs,
   * these will be resolved relative to the original URL as it was before
   * rewriting it. This gives some protection against unwanted accesses.
   * For example, if you map http://host/contents to file:///data/contents,
   * it will not be possible to access files outside this directory,
   * even if tricks are used like opening ../../etc/passwd relative to
   * http://host/contents.  Of course, this protection works only if
   * the resolver opening the file is a subresolver of rewrite_system_id.
   *
   * CHECK: Does this really work?
   *
   * Another application of this class is to use the identity as rewriting
   * rule. This resolver
   * 
   * new rewrite_system_id
   *       [ "file:///data/", "file:///data/" ]
   *       ( new resolve_as_file() )
   *
   * has the effect that only files under /data can be accessed, and
   * other such as /etc/passwd cannot.
   *
   * Option ~forward_unmatching_urls: If true, URLs that do not match any
   *   pattern are forwarded to the inner resolver. These URLs are not
   *   rewritten. NOTE THAT THE MENTIONED ACCESS RESTRICTIONS USUALLY DO
   *   NOT WORK ANYMORE IF THIS OPTION IS TURNED ON.
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
   * (i.e. it does not raise Not_competent on open_rid).
   *
   * If the entity to open has several names, e.g. a public name and
   * a system name, these names are tried in parallel by default (this
   * is possible in the PXP 1.2 model). For backward compatibility, the
   * ~mode argument allows one to specify a different order:
   *
   * (1) Try first to open as public identifier, and if that fails,
   *     fall back to the system identifier (Public_before_system)
   * (2) Try first to open as system identifier, and if that fails,
   *     fall back to the public identifier (System_before_public)
   *
   * Clones: If the 'clone' method is invoked before 'open_rid', all contained
   * resolvers are cloned and again combined. If the 'clone' method is
   * invoked after 'open_rid' (i.e. while the resolver is open), only the
   * active resolver is cloned.
   *
   * ~prefer: This is an internally used option.
   *)


(* ====================================================================== *)

(* TODO: The following examples recommend deprecated classes. *)

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

(**********************************************************************)
(* DEPRECATED CLASSES                                                 *)
(**********************************************************************)

class resolve_read_this_channel :
  ?id:ext_id -> ?fixenc:encoding -> ?close:(in_channel -> unit) ->
  in_channel -> resolver;;

  (* THIS CLASS IS DEPRECATED! USE resolve_to_this_obj_channel INSTEAD!
   *)

  (* Reads from the passed channel (it may be even a pipe). If the ~id
   * argument is passed to the object, the created resolver accepts only
   * this ID (except Anonymous). Otherwise all IDs are accepted, even
   * Anonymous.
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

  (* THIS CLASS IS DEPRECATED! USE resolve_to_any_obj_channel INSTEAD!
   *
   * Note: The function channel_of_id may be called several times to find
   * out the right ext_id from the current resolver_id. The first result
   * is taken that is not Not_competent.
   *)

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

  (* THIS CLASS IS DEPRECATED! USE resolve_to_url_obj_channel INSTEAD!
   *
   * Note: The function url_of_id may be called several times to find
   * out the right ext_id from the current resolver_id. The first result
   * is taken that is not Not_competent.
   *
   * Note: The optional argument base_url is ignored. The class uses always
   * the rid_system_base string to interpret relative URLs.
   *)

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
   * [Does not apply to current implementation but to former ones:]
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

  (* THIS CLASS IS DEPRECATED! USE resolve_to_this_obj_channel INSTEAD!
   *)

  (* Reads from the passed string. If the ~id
   * argument is passed to the object, the created resolver accepts only
   * this ID (except Anonymous). Otherwise all IDs are accepted, even
   * Anonymous.
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

  (* THIS CLASS IS DEPRECATED! USE resolve_to_any_obj_channel INSTEAD!
   *)

  (* resolver_read_any_string f_open ():
   * This resolver calls the function f_open to get the string for
   * the passed ext_id. This function must either return the string and
   * the encoding, or it must fail with Not_competent.
   * The function must return None as encoding if the default mechanism to
   * recognize the encoding should be used. It must return Some e if it is
   * already known that the encoding of the string is e.
   *)

val lookup_public_id_as_file :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;
  (* Same as the equally named class *)

val lookup_public_id_as_string :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;
  (* Same as the equally named class *)

val lookup_system_id_as_file :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;
  (* Same as the equally named class *)

val lookup_system_id_as_string :
  ?fixenc:encoding ->
  (string * string) list ->     (* catalog *)
    resolver;;
  (* Same as the equally named class *)

