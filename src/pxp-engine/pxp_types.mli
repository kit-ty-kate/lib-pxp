(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(** Type definitions used throughout PXP *)

(** This module re-exports all the types listed in
 * {!Pxp_core_types.S} (and finally defined in {!Pxp_core_types.I}), so 
 * the user only has to
 * [open Pxp_types] to get all relevant type definitions. The re-exported
 * definitions are shown here in the indented grey block:
 *)

(** {directinclude true} *)

include Pxp_core_types.S

(** {directinclude false}

    {knowntype Pxp_types.ext_id}
    {knowntype Pxp_types.private_id}
    {knowntype Pxp_types.resolver_id}
    {knowntype Pxp_types.dtd_id}
    {knowntype Pxp_types.content_model_type}
    {knowntype Pxp_types.mixed_spec}
    {knowntype Pxp_types.regexp_spec}
    {knowntype Pxp_types.att_type}
    {knowntype Pxp_types.att_default}
    {knowntype Pxp_types.att_value}
    {knowntype Pxp_types.collect_warnings}
    {knowntype Pxp_types.warning}
    {knowntype Pxp_types.symbolic_warnings}
    {knowntype Pxp_types.encoding}
    {knowntype Pxp_types.rep_encoding}
    {knowntype Pxp_types.output_stream}
    {knowntype Pxp_types.pool}

    {knownclass Pxp_types.drop_warnings}
 *)



(** {2 Configuration} *)

type config =
    { warner : collect_warnings;
         (** An object that collects warnings. *)

      swarner : symbolic_warnings option;
         (** Another object getting warnings expressed as polymorphic
	  * variants. This is especially useful to turn warnings into
	  * errors. If defined, the [swarner] gets the warning
	  * first before it is sent to the classic [warner].
	  *)

      (* errors_with_line_numbers : bool;
	 -- This option is no longer necessary due to code optimizations *)

      enable_pinstr_nodes : bool;
         (** if [true], processing instructions (PI's) are represented by
          * nodes of their own in the document tree. If not enabled, PI's
          * are attached to their surrounding elements, and the exact
          * location within the element is lost.
          *
          * For example, if the XML text
	  * is [<s><?x?>foo<?y?></s>], the parser normally produces only an element
	  * object for [s], and attaches the PIs [x] and [y] to it (without
	  * order), and the details of [x] and [y] can be only found out 
          * with the [pinstr] method of the surrounding element. The only
          * subelement is the data node for "foo".
          * If [enable_pinstr_nodes] the node for element [s] will contain
          * two additional subnodes of type [T_pinstr], one as left sibling
          * of "foo", and one as right sibling. 
          * Aany code processing such a tree must be prepared that 
          * processing instructions occur as normal tree members.
          *
          * The event-based parser reacts on the [enable_pinstr_nodes] mode
          * by emitting [E_pinstr] events at the locations where the PI's
          * occur in the text.
	  *)

      enable_comment_nodes : bool;
         (** When enabled, comments are represented as nodes with type
	  * [T_comment]. If not enabled, comments are ignored.
	  *
	  * Event-based parser: This flag controls whether E_comment events
	  * are generated.
	  *)

      enable_super_root_node : bool;
         (** The [enable_super_root_node] changes the layout of the document
          * tree: The top-most node is no longer the top-most element of the
          * document (i.e. the element root), but a special node called the
          * super root node ([T_super_root]). The top-most element is then
          * a child of the super
          * root node. The super root node can have further children, namely
          * comment nodes and processing instructions that are placed before
          * or after the top-most element in the XML text. However, the exact
          * behaviour depends on whether the other special modes in the
          * configuration are also enabled:
          * - If [enable_pinstr_nodes] is also true, processing instruction
          *   nodes ([T_pinstr]) can occur as children of the super root
          *   node when processing instructions occur before or after the
          *   root element. If [enable_pinstr_nodes] is false, these
          *   instructions are simply attached to the super root node as
          *   they would be attached to ordinary elements within the tree.
          *   Note that processing instructions in the DTD part of the XML
          *   text are not meant here (i.e. instructions between the square
          *   brackets, or in an external DTD). These instructions are always
          *   attached to the DTD object (see {!Pxp_dtd.dtd}).
          * - If [enable_comment_nodes] is also true, comment nodes can 
          *   occur as children of the super root node when comments 
          *   occur before or after the root element. If [enable_comment_nodes]
          *   is false, comments are ignored.
	  *)


      drop_ignorable_whitespace : bool;
        (** Ignorable whitespace is whitespace between XML nodes where
	 * the DTD does not specify that [#PCDATA] must be parsed. For example,
	 * if the DTD contains
         * {[
	 * <!ELEMENT a (b,c)>
	 * <!ELEMENT b (#PCDATA)*>
	 * <!ELEMENT c EMPTY>
         * ]}
	 * the XML text [<a><b> </b> <c></c></a>] is legal. There are two
	 * spaces:
	 * - Between [<b>] and [</b>]. Because [b] is declared with [#PCDATA],
	 *   this space character is not ignorable, and the parser will
	 *   create a data node containing the character
	 * - Between [</b>] and [<c>]. Because the declaration of [a] does not
	 *   contain the keyword [#PCDATA], character data is not expected
	 *   at this position. However, XML allows that whitespace can be
	 *   written here in order to improve the readability of the XML
	 *   text. Such whitespace material is considered as "ignorable
	 *   whitespace". If [drop_ignorable_whitespace] is true, the parser
	 *   will not create a data node containing the character.
	 *   Otherwise, the parser does create such a data node.
         * 
	 * Note that [c] is declared as [EMPTY]. XML does not allow space
	 * characters between [<c>] and [</c>] such that it is not the question
	 * whether such characters are to be ignored or not - they are
	 * simply illegal and will lead to a parsing error.
         *
	 *  In the well-formed mode, the parser treats every whitespace
	 * character occuring in an element as non-ignorable.
	 *
	 * Event-based parser: ignored. (Maybe there will be a stream filter
	 * with the same effect if I find time to program it.)
	 *)

      encoding : rep_encoding;
        (** Specifies the encoding used for the {b internal} representation
	 * of any character data.
	 *)

      recognize_standalone_declaration : bool;
        (** Whether the [standalone] declaration is recognized or not.
	 * This option does not have an effect on well-formedness parsing:
	 * in this case such declarations are never recognized.
	 *
	 * Recognizing the [standalone] declaration means that the 
	 * value of the declaration is scanned and passed to the DTD,
	 * and that the standalone-check is performed. 
	 *
	 * This means: If a document is flagged [standalone='yes']
	 * some additional constraints apply. The idea is that a parser
	 * without access to any external document subsets can still parse
	 * the document, and will still return the same values as the parser
	 * with such access. For example, if the DTD is external and if
	 * there are attributes with default values, it is checked that there
	 * is no element instance where these attributes are omitted - the
	 * parser would return the default value but this requires access to
	 * the external DTD subset.
	 *
	 * Event-based parser: The option has an effect if the [`Parse_xml_decl]
	 * entry flag is set. In this case, it is passed to the DTD whether
	 * there is a standalone declaration, ... and the rest is unclear.
	 *)

      store_element_positions : bool;
        (** Whether the file name, the line and the column of the
	 * beginning of elements are stored in the element nodes.
	 * This option may be useful to generate error messages.
	 * 
	 * Positions are only stored for:
	 * - Elements
	 * - Processing instructions if [T_pinstr] nodes are created for 
         *   them (see [enable_pinstr_nodes])
         *
	 * For all other node types, no position is stored.
	 *
	 * You can access positions by the method [position] of nodes.
	 *
	 * Event-based parser: If true, the [E_position] events will be 
	 * generated.
	 *)

      idref_pass : bool;
        (** Whether the parser does a second pass and checks that all
	 * [IDREF] and [IDREFS] attributes contain valid references.
	 * This option works only if an ID index is available. To create
	 * an ID index, pass an index object as [id_index] argument to the
	 * parsing functions (such as {!Pxp_tree_parser.parse_document_entity}).
	 *
	 * "Second pass" does not mean that the XML text is again parsed;
	 * only the existing document tree is traversed, and the check
	 * on bad [IDREF]/[IDREFS] attributes is performed for every node.
	 *
	 * Event-based parser: this option is ignored.
	 *)

      validate_by_dfa : bool;
        (** If true, and if DFAs are available for validation, the DFAs will
	 * actually be used for validation.
	 * If false, or if no DFAs are available, the standard backtracking
	 * algorithm will be used.
	 *
	 * DFAs are only available if [accept_only_deterministic_models] is
	 * true (because in this case, it is relatively cheap to construct
	 * the DFAs). DFAs are a data structure which ensures that validation
	 * can always be performed in linear time.
	 *
	 * I strongly recommend using DFAs; however, there are examples
	 * for which validation by backtracking is faster.
	 *
	 * Event-based parser: this option is ignored.
	 *)

      accept_only_deterministic_models : bool;
        (** Whether only deterministic content models are accepted in DTDs.
	 *
	 * Event-based parser: this option is ignored.
	 *)


      disable_content_validation : bool;
        (** When set to true, content validation is disabled; however,
	 * other validation checks remain activated.
	 * This option is intended to save time when a validated document
	 * is parsed and it can be assumed that it is valid.
	 *
	 * Do not forget to set [accept_only_deterministic_models] to false
	 * to save maximum time (or DFAs will be computed which is rather
	 * expensive).
	 *
	 * Event-based parser: this option is ignored.
	 *)

      name_pool : Pxp_core_types.I.pool;
      enable_name_pool_for_element_types    : bool;
      enable_name_pool_for_attribute_names  : bool;
      enable_name_pool_for_attribute_values : bool;
      (* enable_name_pool_for_notation_names   : bool; *)
      enable_name_pool_for_pinstr_targets   : bool;
        (** The name pool maps strings to pool strings such that strings with
	 * the same value share the same block of memory.
	 * Enabling the name pool saves memory, but makes the parser
	 * slower.
	 *
	 * Event-based parser: As far as I remember, some of the pool
	 * options are honoured, but not all.
	 *)

      enable_namespace_processing : Pxp_dtd.namespace_manager option;
        (** Setting this option to a [namespace_manager] enables namespace
	 * processing. This works only if the namespace-aware implementation
	 * [namespace_element_impl] of element nodes is used in the spec;
	 * otherwise you will get error messages complaining about missing
	 * methods.
	 *
	 * Note that PXP uses a technique called "prefix normalization" to
	 * implement namespaces on top of the plain document model. This means
	 * that the namespace prefixes of elements and attributes are changed
	 * to unique prefixes if they are ambiguous, and that these 
	 * "normprefixes" are actually stored in the document tree. Furthermore,
	 * the normprefixes are used for validation. (See
         * {!Intro_namespaces} for details.)
	 *
	 * Event-based parser: If true, the events [E_ns_start_tag] and
	 *    [E_ns_end_tag] are generated instead of [E_start_tag], and
	 *    [E_end_tag], respectively.
	 *)

      escape_contents : 
	             (Pxp_lexer_types.token -> Pxp_entity_manager.entity_manager -> 
			string) option;
        (** {b Experimental feature.}
         * If defined, the [escape_contents] function is called whenever 
	 * the tokens "\{", "\{\{", "\}", or "\}\}" are found in the context
	 * of character data contents. The first argument is the token.
	 * The second argument is the entity manager, it can be used to
	 * access the lexing buffer directly. The result of the function
	 * are the characters to substitute.
	 *
	 * "\{" is the token [Lcurly], "\{\{" is the token [LLcurly], "\}" is the
	 * token [Rcurly], and "\}\}" is the token [RRcurly].
	 *
	 * Event-based parser: this option works.
	 *)

      escape_attributes : 
	             (Pxp_lexer_types.token -> int -> Pxp_entity_manager.entity_manager -> 
			string) option;
        (** {b Experimental feature.}
         * If defined, the [escape_attributes] function is called whenever 
	 * the tokens "\{", "\{\{", "\}", or "\}\}" are found inside attribute
	 * values. The function takes three arguments: The token ([Lcurly],
	 * [LLcurly], [Rcurly] or [RRcurly]), the position in the attribute value,
	 * and the entity manager. 
	 * The result of the function is the string substituted for the
	 * token.
         *
	 * Example:
	 * The attribute is "a\{b\{\{c", and the function is called as
	 * follows:
	 * - [escape_attributes Lcurly 1 mng] -
	 *   result is "42" (or an arbitrary string, but in this example it
	 *   is "42")
	 * - [escape_attributes LLcurly 4 mng] -
	 *   result is "foo"
         *
	 * The resulting attribute value is then "a42bfooc".
	 * 
	 * See also [escape_contents].
	 *
	 * Event-based parser: this option works.
	 *)


      (* The following options are not implemented, or only for internal
       * use.
       *)

      debugging_mode : bool;
    }


val default_config : config
  (** Default configuration. This is a recommended set of options that works
   *  generally:
   * - Warnings are thrown away
   * - Error messages will contain line numbers
   * - Neither T_super_root nor T_pinstr nor T_comment nodes are generated
   * - The internal encoding is ISO-8859-1
   * - The standalone declaration is checked
   * - Element positions are stored
   * - The IDREF pass is left out
   * - If available, DFAs are used for validation
   * - Only deterministic content models are accepted
   * - Namespace processing is turned off
   *) 

val default_namespace_config : config
  (** {b Deprecated.}
      Same as [default_config], but namespace processing is turned on.
      Note however, that a globally defined namespace manager is used.
      Because of this, this [config] should no longer be used. Instead, do
      {[
         let m = Pxp_dtd.create_namespace_manager() in
         let namespace_config =
               { default_config with
                    enable_namespace_processing = Some m
               }
      ]}
      and take control of the scope of [m].
   *)


(**********************************************************************)
(*                            sources                                 *)
(**********************************************************************)

(** {2 Sources} *)

(** Sources specify where the XML text to parse comes from. The type
 * [source] is often not used directly, but sources are constructed
 * with the help of the functions [from_channel], [from_obj_channel],
 * [from_file], and [from_string] (see below). {b Note that you can
 * usually view the type [source] as an opaque type.} There is no need
 * to understand why it enumerates these three cases, or to use them
 * directly. Just create sources with one of the [from_*] functions.
 *
 * The type [source] is an abstraction on top of [resolver] (defined in
 * module {!Pxp_reader}). The [resolver] is a configurable object that knows 
 * how to access files that are
 * - identified by an XML ID (a [PUBLIC] or [SYSTEM] name)
 * - named relative to another file
 * - referred to by the special PXP IDs [Private] and [Anonymous].
 *
 * Furthermore, the [resolver] knows a lot about the character encoding
 * of the files. See {!Pxp_reader} for details.
 *
 * A [source] is a resolver that is applied to a certain ID that should
 * be initially opened.
 *)


type source = Pxp_dtd.source =
    Entity of ((Pxp_dtd.dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (Pxp_core_types.I.ext_id * Pxp_reader.resolver)
  | XExtID of (Pxp_core_types.I.ext_id * string option * Pxp_reader.resolver)
     (** *)
(** The three basic flavours of sources:
 * - [Entity(m,r)] is a very low-level way of denoting a source. After the
 *   parser has created the DTD object [d], it calls
 *   {[ e = m d ]}
 *   and uses the entity object [e] together with the resolver [r]. This kind
 *   of [source] is intended to implement customized versions of the entity
 *   classes. Use it only if there is a strong need to do so.
 * - [ExtID(xid,r)] is the normal way of denoting a source. The external entity
 *   referred to by the ID [xid] is opened by using the resolver [r].
 * - [XExtID(xid,sys_base,r)] is an extension of [ExtID]. The additional parameter
 *   [sys_base] is the base URI to assume if [xid] is a relative URI (i.e.
 *   a [SYSTEM] ID).
 *)


val from_channel : 
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      ?id:ext_id -> 
      ?system_encoding:encoding -> 
      in_channel -> 
        source
  (** This function creates a source that reads the XML text from the 
   * passed [in_channel]. By default, this [source] is not able to read
   * XML text from any other location (you cannot read from files etc.).
   * The optional arguments allow it to modify this behaviour.
   *
   * Keep the following in mind:
   * - Because this source reads from a channel, it can only be used once.
   * - The channel will be closed by the parser when the end of the channel
   *   is reached, or when the parser stops because of another reason.
   * - Unless the [alt] argument specifies something else, you cannot
   *   refer to entities by [SYSTEM] or [PUBLIC] names (error "no input method
   *   available")
   * - Even if you pass an [alt] method that can handle [SYSTEM], it is
   *   not immediately possible to open [SYSTEM] entities that are defined
   *   by a URL relative to the entity that is accessed over the [in_channel].
   *   You first must pass the [system_id]
   *   argument, so the parser knows the base name relative to which
   *   other [SYSTEM] entities can be resolved.
   * - For more instructions how to construct sources and resolvers
   *   look at {!Intro_resolution}.
   *
   * {b Arguments:}
   * - [alt]: A list of further resolvers that are used to open further entities
   *   referenced in the initially opened entity. For example, you can pass
   *    [new Pxp_reader.resolve_as_file()] to enable resolving of
   *    file names found in [SYSTEM] IDs.
   * - [system_id]: By default, the XML text found in the [in_channel] does not
   *    have any visible ID (to be exact, the [in_channel] has a private ID, but
   *    this is hidden). Because of this, it is not possible to open
   *    a second file by using a relative [SYSTEM] ID. The parameter [system_id]
   *    assigns the channel a [SYSTEM] ID that is only used to resolve 
   *    further relative [SYSTEM] IDs. -
   *    This parameter must be encoded as UTF-8 string.
   * - [fixenc]: By default, the character encoding of the XML text is 
   *    determined by looking at the XML declaration. Setting [fixenc]
   *    forces a certain character encoding. Useful if you can assume
   *    that the XML text has been recoded by the transmission media.
   *
   * {b Deprecated arguments:}
   * - [id]: This parameter assigns the channel an arbitrary ID (like [system_id],
   *    but [PUBLIC], anonymous, and private IDs are also possible - although
   *    not reasonable). Furthermore, setting [id] also enables resolving
   *    of file names. [id] has higher precedence than [system_id].
   * - [system_encoding]: (Only useful together with [id].) The character encoding
   *    used for file names. (UTF-8 by default.)
   *)


val from_obj_channel :
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      ?id:ext_id -> 
      ?system_encoding:encoding -> 
      Netchannels.in_obj_channel -> 
        source
  (** Similar to [from_channel], but reads from an Ocamlnet netchannel 
      instead. *)



val from_string :
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      string -> 
        source
  (** Similar to [from_channel], but reads from a string.
   *
   * Of course, it is possible to parse this source several times, unlike
   * the channel-based sources.
   *)


val from_file :
       ?alt:Pxp_reader.resolver list ->
       ?system_encoding:encoding -> ?enc:encoding -> string -> source
  (** This source reads initially from the file whose name is passed as 
   * string argument. The
   * filename must be UTF-8-encoded (so it can be correctly rewritten into
   * a URL).
   *
   * This source can open further files by default, and relative URLs
   * work immediately.
   *
   * {b Arguments:}
   * - [alt]: A list of further resolvers, especially useful to open 
   *    non-[SYSTEM] IDs, and non-file entities.
   * - [system_encoding]: The character encoding the system uses to represent
   *    filenames. By default, UTF-8 is assumed.
   * - [enc]: The character encoding of the string argument. As mentioned, this
   *    is UTF-8 by default.
   *)

(**
 * {b Examples.}
 *
 * - The source {[ from_file "/tmp/file.xml" ]} 
 *   reads from this file, which is assumed to have the ID 
 *   [SYSTEM "file://localhost/tmp/file.xml"]. It is no problem when
 *   other files are included by either absolute [SYSTEM] file name,
 *   or by a relative [SYSTEM].
 * - The source
 * {[ let ch = open_in "/tmp/file.xml" in
 * from_channel
 *    ~alt:[ new Pxp_reader.resolve_as_file() ] 
 *    ~system_id:"file://localhost/tmp/file.xml" ch]}
 *   does roughly the same, but uses a channel for the initially opened
 *   entity. Because of the [alt] argument, it is possible to reference
 *   other entities by absolute [SYSTEM] name. The [system_id] assignment
 *   makes it possible that [SYSTEM] names relative to the initially used
 *   entity are resolvable.
 * - The source
 *   {[ let cat = new Pxp_reader.lookup_id
 *                  [ Public("My Public ID",""),"/usr/share/xml/public.xml" ] in
 * from_file ~alt:[cat] "/tmp/file.xml"]}
 *   sets that the [PUBLIC] ID "My Public ID" is mapped to the
 *   shown file, i.e. this file is parsed when this [PUBLIC] ID occurs in
 *   the XML text. (Without mapping [PUBLIC] names these cannot be resolved.)
 *)


val open_source : 
    config -> source -> bool -> Pxp_dtd.dtd ->
      (Pxp_reader.resolver * Pxp_entity.entity)
    (** Returns the resolver and the entity for a source. The boolean arg
     * determines whether a document entity (true) or a normal external
     * entity (false) will be returned.
     *)


(** {2 Entities} *)

(** See {!Pxp_dtd.Entity} for functions dealing with entities. *)

type entity_id = Pxp_lexer_types.entity_id
    (** An [entity_id] is an identifier for an entity, or a fake identifier.
     *)

type entity = Pxp_entity.entity
    (** The representation of entities *)


(** {2 Event parsing} *)

type entry =
    [ `Entry_document     of [ `Val_mode_dtd | `Extend_dtd_fully | 
			       `Parse_xml_decl ] list
    | `Entry_declarations of [ `Val_mode_dtd | `Extend_dtd_fully ] list
    | `Entry_content      of [ `Dummy ] list
    | `Entry_expr         of [ `Dummy ] list
    ]
   (** Entry points for the parser (used to call [process_entity]):
    * - [`Entry_document]: The parser reads a complete document that
    *   must have a DOCTYPE and may have a DTD.
    * - [`Entry_declarations]: The parser reads the external subset
    *   of a DTD
    * - [`Entry_content]: The parser reads an entity containing contents,
    *   i.e. "misc* element misc*".
    * - [`Entry_expr]: The parser reads a single element, a single
    *   processing instruction or a single comment, or whitespace, whatever is
    *   found. In contrast to the other entry points, the expression
    *   need not to be a complete entity, but can start and end in 
    *   the middle of an entity
    *
    * More entry points might be defined in the future.
    *
    * The entry points have a list of flags. Note that [`Dummy] is
    * ignored and only present because O'Caml does not allow empty
    * variants. 
    * For [`Entry_document], and [`Entry_declarations], the flags determine
    * the kind of DTD object that is generated. 
    *
    * {b Without flags}, the DTD
    * object is configured for well-formedness mode:
    * - Elements, attributes, and notations found in the XML text are not 
    *   added to the DTD; entity declarations are added, however. Additionally,
    *   the DTD is configured such that it does not complain about missing
    *   elements, attributes, and notations ([dtd#arbitrary_allowed]).
    *
    * {b The flags} affecting the DTD have the following meaning.
    * Keep in mind that the event parser can only conduct some validation
    * checks because it does not represent the XML nodes as tree.
    *
    * - [`Extend_dtd_fully]: Elements, attributes, and notations are added
    *    to the DTD. The DTD mode [dtd#arbitrary_allowed] is enabled. 
    *    If the resulting event stream is validated later, this mode
    *    has the effect that the actually declared elements, attributes, 
    *    and notations are validated as declared. Also, non-declared
    *    elements, attributes, and notations are {b not rejected}, but
    *    handled as in well-formed mode.
    * - [`Val_mode_dtd]: The DTD object is set up for validation, i.e. all
    *    declarations are added to the DTD, and [dtd#arbitrary_allowed] is 
    *    disabled. Furthermore, some validation checks are already done
    *    for the DTD (e.g. whether the root element is declared).
    *    If the resulting event stream is validated later, all validation
    *    checks are conducted (except for the XML declaration - see the
    *    next flag - this check must be separately enabled).
    * - [`Parse_xml_decl]: By default, the XML declaration
    *   [<?xml version="1.0" encoding="..." standalone="..."?>] is
    *   ignored except for the encoding attribute. This flag causes
    *   that the XML declaration is completely parsed.
    *)


type event =
  | E_start_doc of (string * Pxp_dtd.dtd)
       (** Starts a document. The string is the XML version ("1.0") *)
  | E_end_doc of string
       (** Ends a document. The string is the literal name of the
           root element (without any normalization or transformation)
	*)
  | E_start_tag of (string * (string * string) list * 
		    Pxp_dtd.namespace_scope option *
		    Pxp_lexer_types.entity_id)
       (** [(name, attlist, scope_opt, entid)]: Starts an element [name]
           with an attribute list [attlist]. [scope_opt] is the scope
           object in namespace mode, otherwise [None]. [entid] identifies
           the identity where the start tag occurs
	*)
  | E_end_tag    of (string * Pxp_lexer_types.entity_id)
       (** [(name,entid)]: Ends the element [name] in entity [entid]. *)
  | E_char_data of  string
       (** Character data *)
  | E_pinstr of (string * string * Pxp_lexer_types.entity_id)
       (** A processing instruction [<?target value?>] as node *)
  | E_pinstr_member of (string * string * Pxp_lexer_types.entity_id)
       (** A processing instruction [<?target value?>] that is to be
           attached to the surrounding element or super root node
	*)
  | E_comment of string
       (** A comment node. The string does not include the delimiters *)
  | E_start_super
       (** Starts the super root *)
  | E_end_super
       (** Ends the super root *)
  | E_position of (string * int * int)
       (** [(entity,line,pos)]: Describes that the next element, which is
           either [E_start_tag], [E_pinstr], or [E_comment], is located
           in [entity] at [line] and character position [pos].
	*)
  | E_error of exn
       (** May occur as last event in a stream to describe an error *)
  | E_end_of_stream
       (** If the text can be parsed without error, this event is the
           last event of the stream
	*)
  (** The type of XML events. In event mode, the parser emits a stream
      of these events. The parser already checks that certain structural
      properties are met:
      - Start and end tags (including those of the super root) are properly 
        nested
      - Start and end tags of elements are in the same entity

      If a whole document is parsed (entry [`Entry_document]), the events
      of the text are surrounded by [E_start_doc] and [E_end_doc], i.e. 
      the overall structure is:

      - [E_start_doc]
      - Now the elements (or the super root)
      - [E_end_doc]
      - [E_error] or [E_end_of_stream]

      For the entries [`Entry_content] and [`Entry_expr] the document
      events are left out. The final [E_error] or [E_end_of_stream] event
      is nevertheless emitted.
   *)
