(* $Id: pxp_yacc.mli,v 1.23 2003/01/21 00:19:40 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


(*$ markup-yacc.mli *)

open Pxp_types
open Pxp_dtd
open Pxp_document

exception ID_not_unique        (* see class index below *)

class type [ 'ext ] index =
object 
  (* The type of indexes over the ID attributes of the elements. This type
   * is the minimum requirement needed by the parser to create such an index.
   *)
  constraint 'ext = 'ext node #extension
  method add : string -> 'ext node -> unit
    (* Add the passed node to the index. If there is already an ID with
     * the passed string value, the exception ID_not_unique should be
     * raised. (But the index is free also to accept several identical IDs.)
     *)
  method find : string -> 'ext node
    (* Finds the node with the passed ID value, or raises Not_found *)
end
;;


class [ 'ext ] hash_index : 
object 
  (* This is a simple implementation of 'index' using a hash table. *)
  constraint 'ext = 'ext node #extension
  method add : string -> 'ext node -> unit
    (* See above. *)
  method find : string -> 'ext node
    (* See above. *)
  method index : (string, 'ext node) Hashtbl.t
    (* Returns the hash table. *)
end
;;


type config =
    { warner : collect_warnings;
         (* An object that collects warnings. *)

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
	 *)

      accept_only_deterministic_models : bool;
        (* Whether only deterministic content models are accepted in DTDs. *)

      disable_content_validation : bool;
        (* When set to 'true', content validation is disabled; however,
	 * other validation checks remain activated.
	 * This option is intended to save time when a validated document
	 * is parsed and it can be assumed that it is valid.
	 *
	 * Do not forget to set accept_only_deterministic_models to false
	 * to save maximum time (or DFAs will be computed which is rather
	 * expensive).
	 *)

      name_pool : Pxp_types.pool;
      enable_name_pool_for_element_types    : bool;
      enable_name_pool_for_attribute_names  : bool;
      enable_name_pool_for_attribute_values : bool;
      (* enable_name_pool_for_notation_names   : bool; *)
      enable_name_pool_for_pinstr_targets   : bool;
        (* The name pool maps strings to pool strings such that strings with
	 * the same value share the same block of memory.
	 * Enabling the name pool saves memory, but makes the parser
	 * slower.
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
	 * always refers to the table construct.
	 *
	 * By default, namespace processing is turned off.
	 *)

      enable_namespace_info : bool;
        (* Whether to set the namespace_info slot of elements. 
	 * This option has only an effect if enable_namespace_processing is
	 * non-None.
	 *
	 * Warning! This option requires a lot of memory!
	 *
	 * Default: false
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
	 *)


      (* The following options are not implemented, or only for internal
       * use.
       *)

      debugging_mode : bool;
    }


type source = Pxp_dtd.source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (ext_id * Pxp_reader.resolver)
  | XExtID of (ext_id * string option * Pxp_reader.resolver)
      (* (ext_id, system_base, resolver) *)

val from_channel : 
      ?alt:Pxp_reader.resolver list ->
      ?system_encoding:encoding -> ?id:ext_id -> ?fixenc:encoding -> 
      in_channel -> source

val from_string :
      ?fixenc:encoding -> string -> source

val from_obj_channel :
      ?alt:Pxp_reader.resolver list ->
      ?system_encoding:encoding -> ?id:ext_id -> ?fixenc:encoding -> 
      Netchannels.in_obj_channel -> source


val from_file :
       ?alt:Pxp_reader.resolver list ->
       ?system_encoding:encoding -> ?enc:encoding -> string -> source

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
 *   passed to the function.
 *   Relative system IDs can be interpreted by this function.
 *   The ?system_encoding argument specifies the character encoding used
 *   for file names by the system (i.e. which encoding is used for
 *   the names stored on the disks). By default, UTF-8 is assumed.
 *   The ?enc argument specifies the character encoding of the filename
 *   argument (default: UTF-8).
 *
 * ~alt: This parameter may specify alternate resolvers. They are tried
 *   if the default resolving method is not applicable.
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

val default_extension : ('a node extension) as 'a
  (* A "null" extension; an extension that does not extend the functionality *)

val default_spec : ('a node extension as 'a) spec
  (* Specifies that you do not want to use extensions. *)

val default_namespace_spec : ('a node extension as 'a) spec
  (* Specifies that you want to use namespace, but not extensions *)



val parse_dtd_entity : config -> source -> dtd
  (* Parse an entity containing a DTD (external subset), and return this DTD. *)

val extract_dtd_from_document_entity : config -> source -> dtd
  (* Parses a closed document, i.e. a document beginning with <!DOCTYPE...>,
   * and returns the DTD contained in the document.
   * The parts of the document outside the DTD are actually not parsed,
   * i.e. parsing stops when all declarations of the DTD have been read.
   *)

val parse_document_entity : 
  ?transform_dtd:(dtd -> dtd) ->
  ?id_index:('ext index) ->
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
   *
   * If the optional argument ~id_index is present, the parser adds
   * any ID attribute to the passed index. An index is required to detect
   * violations of the uniqueness of IDs.
   *)

val parse_wfdocument_entity : 
  config -> source -> 'ext spec -> 'ext document
  (* Parse a closed document (see parse_document_entity), but do not
   * validate it. Only checks on well-formedness are performed.
   *)

val parse_content_entity  : 
  ?id_index:('ext index) ->
  config -> source -> dtd -> 'ext spec -> 'ext node
  (* Parse a file representing a well-formed fragment of a document. The
   * fragment must be a single element (i.e. something like <a>...</a>;
   * not a sequence like <a>...</a><b>...</b>). The element is validated
   * against the passed DTD, but it is not checked whether the element is
   * the root element specified in the DTD.
   *
   * If the optional argument ~id_index is present, the parser adds
   * any ID attribute to the passed index. An index is required to detect
   * violations of the uniqueness of IDs.
   *)

val parse_wfcontent_entity : 
  config -> source -> 'ext spec -> 'ext node
  (* Parse a file representing a well-formed fragment of a document
   * (see parse_content_entity). The fragment is not validated, only
   * checked for well-formedness.
   *)


(******************* Event-based interface **************************)

type event =
  | E_start_doc of (string * bool * dtd)
  | E_end_doc
  | E_start_tag of (string * (string * string) list * Pxp_lexer_types.entity_id)
  | E_end_tag   of (string * Pxp_lexer_types.entity_id)
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
   * E_start_tag (name, attlist, entid):      <name attlist>
   * E_end_tag (name, entid):                 </name>
   * E_char_data data:                        data
   * E_pinstr (target,value):                 <?target value?>
   * E_comment value:                         <!--value-->
   * E_position(entity,line,col):             (position of next event)
   * E_end_of_stream:                         always the last event
   *
   * E_position events are only created if the next event will be
   * E_start_tag, E_empty_tag, E_pinstr, or E_comment, and if
   * the configuration option store_element_position is true.
   *
   * The parser usually generates several E_char_data events for a
   * longer section of character data.
   *
   * MAYBE TODO: Events with preprocessed namespaces
   *)

val create_entity_manager :
      ?is_document:bool ->       (* default: true *)
      config -> 
      source -> 
        Pxp_entity_manager.entity_manager
  (* Creates an entity manager that is initialized with the toplevel
   * entity referenced by the source argument. The entity manager
   * can be used by [process_entity] below.
   *
   * The following configuration options are used:
   * - warner
   * - encoding
   * - debugging_mode
   *
   * ~is_document: Pass [true] if the entity to read is a complete
   *   document, and [false] otherwise. The value [true] enforces
   *   several restrictions on document entities, e.g. that 
   *   <![INCLUDE[..]> and <![IGNORE[..]> are not allowed and that additional
   *   nesting rules are respected by parameter entities.
   *)


type entry =
  [ `Entry_document     of [ `Extend_dtd_fully | `Parse_xml_decl ] list
  | `Entry_declarations of [ `Extend_dtd_fully ] list
  | `Entry_content      of [ `Dummy ] list
  | `Entry_expr         of [ `Dummy ] list
  ]
   (* Entry points for the parser (used to call [process_entity]:
    * - `Entry_document: The parser reads a complete document that
    *   may have a DOCTYPE and a DTD.
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
    * - `Extend_dtd_fully: By default, only the <!ENTITY> declarations
    *   are processed (because this is sufficient for well-formedness
    *   parsing). This flag causes that all declarations are processed;
    *   this does not validate the document, however, but is a necessary
    *   step to do so.
    * - `Parse_xml_decl: By default, the XML declaration
    *   <?xml version="1.0" encoding="..." standalone="..."?> is
    *   ignored except for the encoding attribute. This flags causes
    *   that the XML declaration is completely parsed.
    *)


val process_entity :
      config -> 
      entry ->
      Pxp_entity_manager.entity_manager ->
      (event -> unit) ->
        unit
  (* Parses a document or a document fragment, but do not validate
   * it. Only checks on well-formedness are performed.
   * While parsing, events are generated and the passed function is
   * called for every event. The parsed text is read from the
   * current entity of the entity manager. It is allowed that the
   * current entity is open or closed.
   * 
   * The entry point to the parsing rules can be specified.
   * Notes to entry points:
   * - Entry_document:
   *   The first generated event is always E_start_doc,
   *   it contains the whole DTD as object (no events are generated
   *   during DTD parsing, only the result is passed back). The
   *   events for the contents follow, terminated by E_end_doc and
   *   E_end_of_stream.
   * - Entry_content:
   *   Only events for contents are generated. They are terminated
   *   by E_end_of_stream.
   * - Entry_declaration:
   *   Currently not supported.
   *
   * Only the following config options have an effect:
   * - warner
   * - encoding
   * - enable_pinstr_nodes
   * - enable_comment_nodes
   * - store_element_positions
   * - name_pool and all name pool options
   * 
   * In the future, enable_namespace_processing might be interpreted, too.
   *
   * If an error happens, the callback function is invoked exactly once
   * with the E_error event. The error is additionally passed to the caller
   * by letting the exception fall through to the caller. It is not possible
   * to resume parsing after an error.
   *
   * The idea behind this special error handling is that the callback
   * function should always be notified when the parser stops, no matter
   * whether it is successful or not. So the last event passed to the
   * callback function is either E_end_of_stream or E_error. You can
   * imagine that process_entity follows this scheme:
   *
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
   *
   * Note that there is always an At(_,_) exception that wraps the exception
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
  (* This is a special parsing function that corresponds to the entry
   * Entry_expr, i.e. it parses a single element, processing instruction,
   * or comment. In contrast to [process_entity], the current entity
   * is not opened, but it is expected that the entity is already open.
   * Of course, the entity is not closed after parsing (except an error
   * happens).
   *
   * ~first_token: This token is prepended to the tokens read from the
   *    entity manager.
   * ~following_token: The token following the last parsed token is
   *    optionally stored into this variable.
   *    Note: By design the parser _always_ reads the following token.
   *    I know that this may lead to serious problems when it is tried
   *    to integrate this parser with another parser. It is currently
   *    hard to change!
   *)

val create_pull_parser :
      config -> 
      entry ->
      Pxp_entity_manager.entity_manager ->
        ('a -> event option)
  (* let next_event = create_pull_parser cfg entry mng in
   * let ev = next_event()
   *
   * This function parses the XML document in "pull mode". [next_event]
   * should be invoked repeatedly until it returns [None], indicating the
   * end of the document. The events are encoded as [Some ev].
   *
   * The function returns exactly the same events as [process_entity].
   *
   * In contrast to [process_entity], no exception is raised when an
   * error happens. Only the E_error event is generated (as last event).
   *
   * To create a stream of events, just do:
   * let stream = Stream.from(create_pull_parser cfg entry mng)
   *)

(*$-*)


(* ======================================================================
 * History:
 *
 * $Log: pxp_yacc.mli,v $
 * Revision 1.23  2003/01/21 00:19:40  gerd
 * 	Support for the new resolvers.
 *
 * Revision 1.22  2002/10/22 23:29:48  gerd
 * 	More docs for process_entity
 *
 * Revision 1.21  2002/08/17 23:51:56  gerd
 * 	Added pull parsing.
 *
 * Revision 1.20  2002/08/17 19:53:53  gerd
 * 	Changed type [entry] into a polymorphic variant. New variant
 * `Entry_expr. New: flags for [entry].
 * 	New: [process_expr].
 *
 * Revision 1.19  2002/08/05 22:34:29  gerd
 * 	escape_attributes: this config option has an additional
 * argument "position".
 *
 * Revision 1.18  2002/08/03 17:55:59  gerd
 * 	Support for event-based parsing of attribute values: New config
 * option escape_attributes.
 *
 * Revision 1.17  2002/07/14 23:05:01  gerd
 * 	Event-based interface.
 *
 * Revision 1.16  2002/03/10 23:40:52  gerd
 * 	type source is now primarily defined in Pxp_dtd.
 *
 * Revision 1.15  2001/06/08 01:15:47  gerd
 * 	Moved namespace_manager from Pxp_document to Pxp_dtd. This
 * makes it possible that the DTD can recognize the processing instructions
 * <?pxp:dtd namespace prefix="..." uri="..."?>, and add the namespace
 * declaration to the manager.
 *
 * Revision 1.14  2001/06/07 22:53:20  gerd
 * 	New config option: drop_ignorable_whitespace.
 * 	New defaults: default_namespace_config, default_namespace_spec.
 *
 * Revision 1.13  2001/05/17 21:39:15  gerd
 * 	New options: enable_namespace_processing, enable_namespace_info.
 *
 * Revision 1.12  2001/04/24 21:07:13  gerd
 * 	New option ~alt in from_channel and from_file.
 *
 * Revision 1.11  2001/04/03 20:22:44  gerd
 * 	New resolvers for catalogs of PUBLIC and SYSTEM IDs.
 * 	Improved "combine": PUBLIC and SYSTEM IDs are handled
 * separately.
 * 	Rewritten from_file: Is now a simple application of the
 * Pxp_reader classes and functions. (The same has still to be done
 * for from_channel!)
 *
 * Revision 1.10  2000/10/01 19:48:25  gerd
 * 	Option errors_with_line_numbers has been removed.
 *
 * Revision 1.9  2000/09/21 21:30:46  gerd
 * 	New option: disable_content_validation
 *
 * Revision 1.8  2000/09/09 16:41:03  gerd
 * 	Effort to reduce the amount of allocated memory: The number of
 * instance variables in document nodes has been miminized; the class
 * default_ext no longer stores anything; string pools have been implemented.
 *
 * Revision 1.7  2000/08/18 20:15:43  gerd
 * 	Config options:
 * - enable_super_root_nodes: new name for virtual_root
 * - enable_pinstr_nodes: new name for processing_instructions_inline
 * - enable_comment_nodes: new option
 * 	Updated comments for various options.
 *
 * Revision 1.6  2000/07/23 02:16:33  gerd
 * 	Support for DFAs.
 *
 * Revision 1.5  2000/07/14 13:57:29  gerd
 * 	Added the id_index feature.
 *
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
