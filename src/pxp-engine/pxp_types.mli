(* $Id: pxp_types.mli,v 1.22 2003/06/29 15:44:30 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

include Pxp_core_types_type.CORE_TYPES
  (* This module defines and exports all the types listed in
   * Pxp_core_types_type.CORE_TYPES:
   *
   * type ext_id
   * type private_id
   * val allocate_private_id
   * type_resolver_id
   * val resolver_id_of_ext_id
   * type dtd_id
   * type content_model_type
   * type mixed_spec
   * type regexp_spec
   * type att_type
   * type att_default
   * type att_value
   * class type collect_warnings
   * class drop_warnings
   * class type symbolic_warnings
   * type warning
   * type encoding
   * type rep_encoding
   * exception Validation_error
   * exception WF_error
   * exception Namespace_error
   * exception Error
   * exception Character_not_supported
   * exception At
   * exception Undeclared
   * exception Method_not_applicable
   * exception Namespace_method_not_applicable
   * val string_of_exn
   * type output_stream
   * val write
   * type pool
   * val make_probabilistic_pool
   * val pool_string
   *
   * See the file pxp_core_types_type.mli for the exact definitions of
   * these types/values.
   *)


type config =
    { warner : collect_warnings;
         (* An object that collects warnings. *)

      swarner : symbolic_warnings option;
         (* Another object getting warnings expressed as polymorphic
	  * variants. This is especially useful to turn warnings into
	  * errors. If defined, the [swarner] gets the warning
	  * first before it is sent to the classic [warner].
	  *)

      (* errors_with_line_numbers : bool;
	 -- This option is no longer necessary due to code optimizations *)

      enable_pinstr_nodes : bool;
         (* true: turns a special mode for processing instructions on. Normally,
	  * you cannot determine the exact location of a PI; you only know
	  * in which element the PI occurs. This mode makes it possible
	  * to find the exact location out: Every PI is artificially wrapped
	  * by a special node with type T_pinstr. For example, if the XML text
	  * is <a><?x?><?y?></a>, the parser normally produces only an element
	  * object for "a", and puts the PIs "x" and "y" into it (without
	  * order). In this mode, the object "a" will contain two objects
	  * with type T_pinstr, and the first object will contain "x", and the
	  * second "y": the object tree looks like
	  * - Node with type = T_element "a"
	  *   - Node with type = T_pinstr "x"
	  *     + contains processing instruction "x"
	  *   - Node with type = T_pinstr "y"
	  *     + contains processing instruction "y"
	  *
	  * Notes:
	  * (1) In past versions of PXP this mode was called
	  *     processing_instructions_inline, and it produced nodes of
	  *     type T_element "-pi" instead of T_pinstr.
	  * (2) The T_pinstr nodes are created from the pinstr exemplars
	  *     in your spec
	  * (3) Event-based parser: This flag controls whether E_pinstr
	  *     events are generated or not.
	  *)

      enable_super_root_node : bool;
         (* true: the topmost element of the XML tree is not the root element,
	  * but the so-called super root. The root element is a son of the
	  * super root. The super root is a node with type T_super_root.
	  * The following behaviour changes, too:
	  * - PIs occurring outside the root element and outside the DTD are
	  *   added to the super root instead of the document object
	  * - If enable_pinstr_nodes is also turned on, the PI wrappers
	  *   are added to the super root
	  *
	  * For example, the document
	  *   <?x?><a>y</a><?y?>
	  * is normally represented by:
	  * - document object
	  *   + contains PIs x and y
	  *   - reference to root node with type = T_element "a"
	  *     - node with type = T_data: contains "y"
	  * With enabled super root node:
	  * - document object
	  *   - reference to super root node with type = T_super_root
	  *     + contains PIs x and y
	  *     - root node with type = T_element "a"
	  *       - node with type = T_data: contains "y"
	  * If also enable_pinstr_nodes:
	  * - document object
	  *   - reference to super root node with type = T_super_root
	  *     - node with type = T_pinstr "x"
	  *       + contains PI "x"
	  *     - root node with type = T_element "a"
	  *       - node with type = T_data: contains "y"
	  *     - node with type = T_pinstr "y"
	  *       + contains PI "y"
	  * Notes:
	  * (1) In previous versions of PXP this mode was called
	  *     virtual_root, and it produced an additional node of type
	  *     T_element "-vr" instead of T_super_root.
	  * (2) The T_super_root node is created from the super root exemplar
	  *     in your spec.
	  * (3) Event-based parser: no effect
	  *)

      enable_comment_nodes : bool;
         (* When enabled, comments are represented as nodes with type =
	  * T_comment.
	  * To access the contents of comments, use the method "comment"
	  * for the comment nodes. 
	  * These nodes behave like elements; however, they are normally
	  * empty and do not have attributes. Note that it is possible to
	  * add children to comment nodes and to set attributes, but it is
	  * strongly recommended not to do so. There are no checks on
	  * such abnormal use, because they would cost too
	  * much time, even when no comment nodes are generated at all.
	  *
	  * Comment nodes should be disabled unless you must parse a 
	  * third-party XML text which uses comments as another data
	  * container.
	  *
	  * The nodes of type T_comment are created from the comment exemplars
	  * in your spec.
	  *
	  * Event-based parser: This flag controls whether E_comment events
	  * are generated.
	  *)

      drop_ignorable_whitespace : bool;
        (* Ignorable whitespace is whitespace between XML nodes where
	 * the DTD does not specify that #PCDATA must be parsed. For example,
	 * if the DTD contains
	 * <!ELEMENT a (b,c)>
	 * <!ELEMENT b (#PCDATA)*>
	 * <!ELEMENT c EMPTY>
	 * the XML text <a><b> </b> <c></c></a> is legal. There are two
	 * spaces:
	 * (1) Between <b> and </b>. Because b is declared with #PCDATA,
	 *     this space character is not ignorable, and the parser will
	 *     create a data node containing the character
	 * (2) Between </b> and <c>. Because the declaration of a does not
	 *     contain the keyword #PCDATA, character data is not expected
	 *     at this position. However, XML allows that whitespace can be
	 *     written here in order to improve the readability of the XML
	 *     text. Such whitespace material is considered as "ignorable
	 *     whitespace". If drop_ignorable_whitespace=true, the parser
	 *     will not create a data node containing the character.
	 *     Otherwise, the parser creates such a data node anyway.
	 * Note that c is declared as EMPTY. XML does not allow space
	 * characters between <c> and </c> such that it is not the question
	 * whether such characters are to be ignored or not - they are
	 * simply illegal and will lead to a parsing error.
	 *    In the well-formed mode, the parser treats every whitespace
	 * character occuring in an element as non-ignorable.
	 *
	 * The default is to drop ignorable whitespace.
	 *
	 * Note that this option supersedes the effect of the method
	 * keep_always_whitespace_mode which was defined for document
	 * nodes in PXP 1.0.
	 *
	 * Event-based parser: ignored. (Maybe there will be a stream filter
	 * with the same effect if I find time to program it.)
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
	 * value of the declaration is scanned and passed to the DTD,
	 * and that the "standalone-check" is performed. 
	 *
	 * Standalone-check: If a document is flagged standalone='yes' 
	 * some additional constraints apply. The idea is that a parser
	 * without access to any external document subsets can still parse
	 * the document, and will still return the same values as the parser
	 * with such access. For example, if the DTD is external and if
	 * there are attributes with default values, it is checked that there
	 * is no element instance where these attributes are omitted - the
	 * parser would return the default value but this requires access to
	 * the external DTD subset.
	 *
	 * Event-based parser: The option has an effect if the `Parse_xml_decl
	 *   entry flag is set. In this case, it is passed to the DTD whether
	 *   there is a standalone declaration, ... and the rest is unclear.
	 *)

      store_element_positions : bool;
        (* Whether the file name, the line and the column of the
	 * beginning of elements are stored in the element nodes.
	 * This option may be useful to generate error messages.
	 * 
	 * Positions are only stored for:
	 * - Elements
	 * - Wrapped processing instructions (see enable_pinstr_nodes)
	 * For all other node types, no position is stored.
	 *
	 * You can access positions by the method "position" of nodes.
	 *
	 * Event-based parser: If true, the E_position events will be 
	 *   generated.
	 *)

      idref_pass : bool;
        (* Whether the parser does a second pass and checks that all
	 * IDREF and IDREFS attributes contain valid references.
	 * This option works only if an ID index is available. To create
	 * an ID index, pass an index object as id_index argument to the
	 * parsing functions (such as parse_document_entity; see below).
	 *
	 * "Second pass" does not mean that the XML text is again parsed;
	 * only the existing document tree is traversed, and the check
	 * on bad IDREF/IDREFS attributes is performed for every node.
	 *
	 * Event-based parser: this option is ignored.
	 *)

      validate_by_dfa : bool;
        (* If true, and if DFAs are available for validation, the DFAs will
	 * actually be used for validation.
	 * If false, or if no DFAs are available, the standard backtracking
	 * algorithm will be used.
	 * DFA = deterministic finite automaton.
	 *
	 * DFAs are only available if accept_only_deterministic_models is
	 * "true" (because in this case, it is relatively cheap to construct
	 * the DFAs). DFAs are a data structure which ensures that validation
	 * can always be performed in linear time.
	 *
	 * I strongly recommend using DFAs; however, there are examples
	 * for which validation by backtracking is faster.
	 *
	 * Event-based parser: this option is ignored.
	 *)

      accept_only_deterministic_models : bool;
        (* Whether only deterministic content models are accepted in DTDs.
	 *
	 * Event-based parser: this option is ignored.
	 *)


      disable_content_validation : bool;
        (* When set to 'true', content validation is disabled; however,
	 * other validation checks remain activated.
	 * This option is intended to save time when a validated document
	 * is parsed and it can be assumed that it is valid.
	 *
	 * Do not forget to set accept_only_deterministic_models to false
	 * to save maximum time (or DFAs will be computed which is rather
	 * expensive).
	 *
	 * Event-based parser: this option is ignored.
	 *)

      name_pool : Pxp_core_types.pool;
      enable_name_pool_for_element_types    : bool;
      enable_name_pool_for_attribute_names  : bool;
      enable_name_pool_for_attribute_values : bool;
      (* enable_name_pool_for_notation_names   : bool; *)
      enable_name_pool_for_pinstr_targets   : bool;
        (* The name pool maps strings to pool strings such that strings with
	 * the same value share the same block of memory.
	 * Enabling the name pool saves memory, but makes the parser
	 * slower.
	 *
	 * Event-based parser: As far as I remember, some of the pool
	 *   options are honoured, but not all.
	 *)

      enable_namespace_processing : Pxp_dtd.namespace_manager option;
        (* Setting this option to a namespace_manager enables namespace
	 * processing. This works only if the namespace-aware implementation
	 * namespace_element_impl of element nodes is used in the spec;
	 * otherwise you will get error messages complaining about missing
	 * methods.
	 *
	 * Note that PXP uses a technique called "prefix normalization" to
	 * implement namespaces on top of the plain document model. This means
	 * that the namespace prefixes of elements and attributes are changed
	 * to unique prefixes if they are ambiguous, and that these 
	 * "normprefixes" are actually stored in the document tree. Furthermore,
	 * the normprefixes are used for validation.
	 * 
	 * Every normprefix corresponds uniquely to a namespace URI, and 
	 * this mapping is controlled by the namespace_manager. It is possible
	 * to fill the namespace_manager before parsing starts such that
	 * the programmer knows which normprefix is used for which namespace
	 * URI. Example:
	 *
	 * let mng = new namespace_manager in
	 * mng # add "html" "http://www.w3.org/1999/xhtml";
	 * ...
	 *
	 * This forces that elements with the mentioned URI are rewritten
	 * to a form using the normprefix "html". For instance, "html:table" 
	 * always refers to the HTML table construct, independently of the
	 * prefix used in the parsed XML text.
	 *
	 * By default, namespace processing is turned off.
	 *
	 * Event-based parser: If true, the events E_ns_start_tag and
	 *    E_ns_end_tag are generated instead of E_start_tag, and
	 *    E_end_tag, respectively.
	 *)

      enable_namespace_info : bool;
        (* Whether to set the namespace_info slot of elements. 
	 * This option has only an effect if enable_namespace_processing is
	 * non-None.
	 *
	 * Warning! This option requires a lot of memory!
	 *
	 * Default: false
	 *
	 * Event-based parser: this option is ignored.
	 *)

      (* Experimental stuff: *)

      escape_contents : 
	             (Pxp_lexer_types.token -> Pxp_entity_manager.entity_manager -> 
			string) option;
        (* If defined, the [escape_contents] function is called whenever 
	 * the tokens "{", "{{", "}", or "}}" are found in the context
	 * of character data contents. The first argument is the token.
	 * The second argument is the entity manager, it can be used to
	 * access the lexing buffer directly. The result of the function
	 * are the characters to substitute.
	 *
	 * "{" is the token Lcurly, "{{" is the token LLcurly, "}" is the
	 * token Rcurly, and "}}" is the token RRcurly.
	 *
	 * Default: None
	 *
	 * Event-based parser: this option works.
	 *)

      escape_attributes : 
	             (Pxp_lexer_types.token -> int -> Pxp_entity_manager.entity_manager -> 
			string) option;
        (* If defined, the [escape_attributes] function is called whenever 
	 * the tokens "{", "{{", "}", or "}}" are found inside attribute
	 * values. The function takes three arguments: The token (Lcurly,
	 * LLcurly, Rcurly or RRcurly), the position in the attribute value,
	 * and the entity manager. 
	 * The result of the function is the string substituted for the
	 * token.
	 * Example:
	 * The attribute is "a{b{{c", and the function is called as
	 * follows:
	 * - escape_attributes Lcurly 1 mng 
	 *   Result is "42" (or an arbitrary string, but in this example it
	 *   is "42")
	 * - escape_attributes LLcurly 4 mng
	 *   Result is "foo"
	 * The resulting attribute value is "a42bfooc".
	 * 
	 * See also [escape_contents].
	 *
	 * Default: None
	 *
	 * Event-based parser: this option works.
	 *)


      (* The following options are not implemented, or only for internal
       * use.
       *)

      debugging_mode : bool;
    }


val default_config : config
  (* - Warnings are thrown away
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
  (* Same as default_config, but namespace processing is turned on *)


(**********************************************************************)
(*                            sources                                 *)
(**********************************************************************)

(* Sources specify where the XML text to parse comes from. The type
 * [source] is often not used directly, but sources are constructed
 * with the help of the functions [from_channel], [from_obj_channel],
 * [from_file], and [from_string] (see below).
 *
 * The type [source] is an abstraction on top of [resolver] (defined in
 * module Pxp_reader). The [resolver] is a configurable object that knows 
 * how to access files that are
 * - identified by an XML ID (a PUBLIC or SYSTEM name)
 * - named relative to another file
 * - referred to by the special PXP IDs "Private" and "Anonymous".
 * Furthermore, the [resolver] knows a lot about the character encoding
 * of the files. See Pxp_reader for details.
 *
 * A [source] is a resolver that is applied to a certain ID that should
 * be initially opened.
 *)


type source = Pxp_dtd.source =
    Entity of ((Pxp_dtd.dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (Pxp_core_types.ext_id * Pxp_reader.resolver)
  | XExtID of (Pxp_core_types.ext_id * string option * Pxp_reader.resolver)

(* The three basic flavours of sources:
 * - Entity(m,r) is a very low-level way of denoting a source. After the
 *   parser has created the DTD object d, it calls
 *   e = m d
 *   and uses the entity object e together with the resolver r. This kind
 *   of [source] is intended to implement customized versions of the entity
 *   classes. Use it only if there is a strong need to do so.
 * - ExtID(xid,r) is the normal way of denoting a source. The external entity
 *   referred to by the ID [xid] is opened by using the resolver [r].
 * - XExtID(xid,sys_base,r) is an extension of ExtID. The additional parameter
 *   [sys_base] is the base URI to assume if [xid] is a relative URI (i.e.
 *   a SYSTEM ID).
 *)


val from_channel : 
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      ?id:ext_id -> 
      ?system_encoding:encoding -> 
      in_channel -> 
        source
  (* This function creates a source that reads the XML text from the 
   * passed [in_channel]. By default, this [source] is not able to read
   * XML text from any other location (you cannot read from files etc.).
   * The optional arguments allow it to modify this behaviour.
   *
   * Keep the following in mind:
   * - Because this source reads from a channel, it can only be used once.
   * - The channel will be closed by the parser when the end of the channel
   *   is reached, or when the parser stops because of another reason.
   * - Unless the ~alt argument specifies something else, you cannot
   *   refer to entities by SYSTEM or PUBLIC names (error "no input method
   *   available")
   * - To make relative SYSTEM names work you must pass the ~system_id
   *   argument, so the parser knows relative to which base these names
   *   must be resolved.
   *
   * ~alt: A list of further resolvers. For example, you can pass
   *    [new Pxp_reader.resolve_as_file()] to enable resolving of
   *    file names found in SYSTEM IDs.
   * ~system_id: By default, the XML text found in the [in_channel] does not
   *    have any ID (to be exact, the [in_channel] has a private ID, but
   *    this is hidden). Because of this, it is not possible to open
   *    a second file by using a relative SYSTEM ID. The parameter ~system_id
   *    assigns the channel a SYSTEM ID that is only used to resolve 
   *    further relative SYSTEM IDs.
   *    This parameter must be encoded as UTF-8 string.
   * ~fixenc: By default, the character encoding of the XML text is 
   *    determined by looking at the XML declaration. Setting ~fixenc
   *    forces a certain character encoding. Useful if you can assume
   *    that the XML text has been recoded by the transmission media.
   *
   * THE FOLLOWING OPTIONS ARE DEPRECATED:
   *
   * ~id: This parameter assigns the channel an arbitrary ID (like ~system_id,
   *    but PUBLIC, anonmyous, and private IDs are also possible - although
   *    not reasonable). Furthermore, setting ~id also enables resolving
   *    of file names. 
   *    ~id has higher precedence than ~system_id.
   * ~system_encoding: (Only useful together with ~id.) The character encoding
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
  (* Similar to [from_channel], but reads from a netchannel instead. *)



val from_string :
      ?alt:Pxp_reader.resolver list ->
      ?system_id:string ->
      ?fixenc:encoding -> 
      string -> 
        source
  (* Similar to [from_channel], but reads from a string.
   *
   * Of course, it is possible to parse this source several times, unlike
   * the channel-based sources.
   *)


val from_file :
       ?alt:Pxp_reader.resolver list ->
       ?system_encoding:encoding -> ?enc:encoding -> string -> source
  (* The source is the file whose name is passed as string argument. The
   * filename must be UTF-8-encoded (so it can be correctly rewritten into
   * a URL).
   *
   * This source can open further files by default, and relative URLs
   * work.
   *
   * ~alt: A list of further resolvers, especially useful to open 
   *    non-SYSTEM IDs, and non-file entities.
   * ~system_encoding: The character encoding the system uses to represent
   *    filenames. By default, UTF-8 is assumed.
   * ~enc: The character encoding of the string argument. As mentioned, this
   *    is UTF-8 by default.
   *)

(*
 * EXAMPLES:
 *
 * from_file "/tmp/file.xml": 
 *   reads from this file, which is assumed to have the ID 
 *   SYSTEM "file://localhost/tmp/file.xml".
 *
 * let ch = open_in "/tmp/file.xml" in
 * from_channel ~alt:[ new Pxp_reader.resolve_as_file() ] 
 *              ~system_id:"file://localhost/tmp/file.xml" ch
 *   This does roughly the same, but uses a channel.
 *
 * let cat = new Pxp_reader.lookup_id
 *                 [ Public("My Public ID",""),"/usr/share/xml/public.xml" ] in
 * from_file ~alt:[cat] "/tmp/file.xml": 
 *   Additionally sets that the PUBLIC ID "My Public ID" is mapped to the
 *   shown file.
 *)


val open_source : 
    config -> source -> bool -> Pxp_dtd.dtd ->
      (Pxp_reader.resolver * Pxp_entity.entity)
    (* Returns the resolver and the entity for a source. The boolean arg
     * determines whether a document entity (true) or a normal external
     * entity (false) will be returned.
     *)


type entry =
    [ `Entry_document     of [ `Val_mode_dtd | `Extend_dtd_fully | 
			       `Parse_xml_decl ] list
    | `Entry_declarations of [ `Val_mode_dtd | `Extend_dtd_fully ] list
    | `Entry_content      of [ `Dummy ] list
    | `Entry_expr         of [ `Dummy ] list
    ]
   (* Entry points for the parser (used to call [process_entity]:
    * - `Entry_document: The parser reads a complete document that
    *   must have a DOCTYPE and may have a DTD.
    * - `Entry_declarations: The parser reads the external subset
    *   of a DTD
    * - `Entry_content: The parser reads an entity containing contents,
    *   i.e. misc* element misc*.
    * - `Entry_expr: The parser reads a single element, a single
    *   processing instruction or a single comment, or whitespace, whatever is
    *   found. In contrast to the other entry points, the expression
    *   need not to be a complete entity, but can start and end in 
    *   the middle of an entity
    * More entry points might be defined in the future.
    *
    * The entry points have a list of flags. Note that `Dummy is
    * ignored and only present because O'Caml does not allow empty
    * variants. 
    * For `Entry_document, and `Entry_declarations, the flags determine
    * the kind of DTD object that is generated. Without flags, the DTD
    * object is configured for well-formedness mode:
    *
    * - Elements, attributes, and notations found in the XML text are not 
    *   added to the DTD; entity declarations are added, however. Additionally,
    *   the DTD is configured such that it does not complain about missing
    *   elements, attributes, and notations (dtd#arbitrary_allowed).
    *
    * The flags affecting the DTD have the following meaning:
    *
    * - `Extend_dtd_fully: Elements, attributes, and notations are added
    *    to the DTD. The DTD mode dtd#arbitrary_allowed is enabled. If you
    *    validated the XML document later, this would mean that declared
    *    elements, attributes, and notations would actually be validated, but
    *    that non-declared objects would be handled like in well-formedness
    *    mode. Of course, you must pass the parsed events to a validator
    *    in order to validate them, this is not done implicitly.
    *
    * - `Val_mode_dtd: The DTD object is set up for validation, i.e. all
    *    declarations are added to the DTD, and dtd#arbitrary_allowed is 
    *    disabled. Furthermore, some validation checks are already done
    *    for the DTD (e.g. whether the root element is declared).
    *    Of course, you must pass the parsed events to a validator
    *    in order to validate them, this is not done implicitly.
    *
    * There is another option regarding the XML declaration:
    *
    * - `Parse_xml_decl: By default, the XML declaration
    *   <?xml version="1.0" encoding="..." standalone="..."?> is
    *   ignored except for the encoding attribute. This flags causes
    *   that the XML declaration is completely parsed.
    *)


type event =
  | E_start_doc of (string * bool * Pxp_dtd.dtd)
  | E_end_doc
  | E_start_tag of (string * (string * string) list * 
		    Pxp_lexer_types.entity_id)
  | E_ns_start_tag of (string * string * (string * string * string) list *
		       Pxp_lexer_types.entity_id)
  | E_end_tag    of (string * Pxp_lexer_types.entity_id)
  | E_ns_end_tag of (string * string * Pxp_lexer_types.entity_id)
  | E_char_data of  string
  | E_pinstr of (string * string)
  | E_comment of string
  | E_position of (string * int * int)
  | E_error of exn
  | E_end_of_stream
  (* may be extended in the future *)

  (* The type of XML events:
   * E_start_doc (xmlversion,standalone,dtd)
   * E_end_doc
   *
   * E_start_tag (name, attlist, entid):
   *    <name attlist>
   *    only used in non-namespace mode
   *
   * E_ns_start_tag (orig_name, norm_name, attlist, entid)
   *    only used in namespace mode; orig_name is the element as found
   *    in the XML text; norm_name is the normalized element name;
   *    attlist consists of triples (orig_name, norm_name, value).
   *
   * E_end_tag (name, entid):                 
   *    </name>
   *    only used in non-namespace mode
   *
   * E_ns_end_tag (orig_name, norm_name, entid):
   *    only used in namespace mode
   *
   * E_char_data data:
   *     The parser usually generates several E_char_data events for a
   *     longer section of character data.
   *
   * E_pinstr (target,value):                 
   *     <?target value?>
   *
   * E_comment value:
   *     <!--value-->
   *
   * E_position(entity,line,col):
   *    these events are only created if the next event will be
   *    E_start_tag, E_pinstr, or E_comment, and if
   *    the configuration option store_element_position is true.
   *
   * E_end_of_stream:                         
   *    this last event indicates that the parser has terminated without
   *    error
   *
   * E_error(exn):
   *    this last event indicates that the parser has terminated with
   *    error
   *)



(* ======================================================================
 * History:
 *
 * $Log: pxp_types.mli,v $
 * Revision 1.22  2003/06/29 15:44:30  gerd
 * 	New entry flag: `Val_mode_dtd
 *
 * Revision 1.21  2003/06/20 21:00:33  gerd
 * 	Moved events to Pxp_types.
 * 	Implementation of namespaces in event-based parsers.
 *
 * Revision 1.20  2003/06/20 15:14:14  gerd
 * 	Introducing symbolic warnings, expressed as polymorphic
 * variants
 *
 * Revision 1.19  2003/06/19 21:10:15  gerd
 * 	Revised the from_* functions.
 *
 * Revision 1.18  2003/06/15 18:19:56  gerd
 * 	Pxp_yacc has been split up
 *
 * Revision 1.17  2003/06/15 12:23:22  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.16  2003/01/21 00:18:09  gerd
 * 	New type resolver_id. It is related to ext_id but contains
 * more information.
 *
 * Revision 1.15  2002/08/28 23:54:34  gerd
 * 	Support for new lexer definition style.
 *
 * Revision 1.14  2001/06/27 23:33:53  gerd
 * 	Type output_stream is now a polymorphic variant
 *
 * Revision 1.13  2001/06/07 22:49:51  gerd
 * 	New namespace exceptions.
 *
 * Revision 1.12  2001/04/26 23:57:05  gerd
 * 	New exception Method_not_applicable. It is raised if there are
 * classes A and B both conforming to class type C, but A does not implement
 * a method required by the class type. In this case, invoking the method
 * in A raises Method_not_applicable.
 * 	This feature is mainly used in Pxp_document.
 *
 * Revision 1.11  2001/04/22 14:14:41  gerd
 * 	Updated to support private IDs.
 *
 * Revision 1.10  2001/02/01 20:37:38  gerd
 * 	Changed comment.
 *
 * Revision 1.9  2000/09/09 16:38:47  gerd
 * 	New type 'pool'.
 *
 * Revision 1.8  2000/08/14 22:24:55  gerd
 * 	Moved the module Pxp_encoding to the netstring package under
 * the new name Netconversion.
 *
 * Revision 1.7  2000/07/27 00:41:15  gerd
 * 	new 8 bit codes
 *
 * Revision 1.6  2000/07/16 18:31:09  gerd
 * 	The exception Illegal_character has been dropped.
 *
 * Revision 1.5  2000/07/16 16:34:21  gerd
 * 	Updated comments.
 *
 * Revision 1.4  2000/07/14 21:25:27  gerd
 * 	Simplified the type 'collect_warnings'.
 *
 * Revision 1.3  2000/07/08 16:23:50  gerd
 * 	Added the exception 'Error'.
 *
 * Revision 1.2  2000/07/04 22:08:26  gerd
 * 	type ext_id: New variant Anonymous. - The System and Public
 * variants are now encoded as UTF-8.
 * 	collect_warnings is now a class type only. New class
 * drop_warnings.
 * 	New functions  encoding_of_string and string_of_encoding.
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
 * Old logs from Markup_types.mli:
 *
 * Revision 1.7  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.6  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.5  2000/05/01 20:43:25  gerd
 *         New type output_stream; new function 'write'.
 *
 * Revision 1.4  1999/09/01 16:25:35  gerd
 * 	Dropped Illegal_token and Content_not_allowed_here. WF_error can
 * be used instead.
 *
 * Revision 1.3  1999/08/15 02:22:40  gerd
 *         Added exception Undeclared.
 *
 * Revision 1.2  1999/08/14 22:15:17  gerd
 *         New class "collect_warnings".
 *
 * Revision 1.1  1999/08/10 00:35:52  gerd
 * 	Initial revision.
 *
 *
 *)
