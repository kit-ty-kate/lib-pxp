(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(*$ markup-dtd1.mli *)

(**********************************************************************)
(*                                                                    *)
(* Pxp_dtd:                                                           *)
(*     Object model of document type declarations                     *)
(*                                                                    *)
(**********************************************************************)

(* ======================================================================
 * OVERVIEW
 *
 * class dtd ............... represents the whole DTD, including element
 *                           declarations, entity declarations, notation
 *                           declarations, and processing instructions
 * class dtd_element ....... represents an element declaration consisting
 *                           of a content model and an attribute list
 *                           declaration
 * class dtd_notation ...... represents a notation declaration
 * class proc_instruction .. represents a processing instruction
 * class namespace_manager . manages the mapping from normalized prefixes
 *                           to namespace IDs
 * ======================================================================
 *
 *)


type validation_record =
    { content_model   : Pxp_core_types.content_model_type;
      content_dfa     : Pxp_dfa.dfa_definition option Lazy.t;
      id_att_name     : string option;
      idref_att_names : string list;
      att_lookup      : int Pxp_aux.Str_hashtbl.t;
      init_att_vals   : (string * Pxp_core_types.att_value) array;
      att_info        : (Pxp_core_types.att_type * bool) array;
      att_required    : int list;
      accept_undeclared_atts : bool;
    }
  (* This is an internally structure used to pass validation information 
   * efficiently from the DTD to the document nodes.
   * Please do not use this type in your own programs.
   *)


class namespace_manager :
  (* This class manages mappings from URIs to normalized prefixes. For every
   * namespace a namespace_manager object contains a set of mappings
   * uri1 |-> np, uri2 |-> np, ..., uriN |-> np.
   * The normalized prefix np is characterical of the namespace, and
   * identifies the namespace uniquely.
   * The first URI uri1 is the primary URI, the other URIs are aliases.
   * The following operations are supported:
   * - add_uri np uri: The passed uri is added to the already existing
   *   namespace which is identified by the normprefix np. This means
   *   that the precondition is that there is already some mapping
   *   uri' |-> np, and that there is no mapping for uri. Postcondition
   *   is that uri |-> np is a new mapping.
   *   add_uri thus adds a new alias URI for an existing namespace.
   * - add_namespace np uri: Precondition is that neither np nor uri
   *   are used in the namespace_manager object. The effect is that the
   *   mapping uri |-> np is added.
   * - lookup_or_add_namespace p uri: If there is already some mapping
   *   uri |-> np, the normprefix np is simply returned ("lookup"). In this
   *   case p is ignored. Otherwise uri is not yet mapped, and in this
   *   case some unique np must be found such that uri |-> np can be
   *   added ("add_namespace"). First, the passed prefix p is tried.
   *   If p is free, it can be taken as new normprefix: np = p. Otherwise
   *   some number n is found such that the concatenation p + n is free:
   *   np = p + n. The operation returns np.
   *)
  object
    method add_uri : string -> string -> unit
      (* add_uri np uri: adds uri as alias URI to the namespace identified
       * by the normprefix np (see above for detailed semantics). The method
       * raises Not_found if the normprefix np is unknown to the object,
       * and it fails (Namespace_error) if the uri is member of a
       * different namespace. Nothing happens if the uri is already member
       * of the namespace np.
       *)
    method add_namespace : string -> string -> unit
      (* add_namespace np uri: adds a new namespace to the object. The
       * namespace is identified by the normprefix np and contains initially
       * the primary URI uri.
       * The method fails (Namespace_error) if either np already identifies
       * some namespace or if uri is already member of some namespace.
       * Nothing happens if uri is the sole member of the namespace np.
       * It is required that np <> "".
       *)
    method lookup_or_add_namespace : string -> string -> string
      (* lookup_or_add_namespace p uri: first, the method looks up if
       * the namespace for uri does already exist. If so, p is ignored,
       * and the method returns the normprefix identifying the namespace.
       * Otherwise, a new namespace is added for some normprefix np which
       * initially contains uri. The normprefix np is calculated upon p
       * serving as suggestion for the normprefix. The method returns
       * the normprefix.
       *)
    method get_primary_uri : string -> string
      (* Return the primary URI for a normprefix, or raises Not_found.
       * get_uri "" raises always Not_found.
       *)
    method get_uri_list : string -> string list
      (* Return all URIs for a normprefix, or [] if the normprefix is
       * unused. get_uri_list "" returns always []. The last URI of the
       * returned list is the primary URI.
       *)
    method get_normprefix : string -> string
      (* Return the normprefix for a URI, or raises Not_found *)
    method iter_namespaces : (string -> unit) -> unit
      (* Iterates over all namespaces contained in the object, and
       * calls the passed function for every namespace. The argument of the
       * invoked function is the normprefix of the namespace.
       *)

    (* Encodings: prefixes and URIs are always encoded in the default
     * encoding of the document
     *)
  end
;;


class dtd :
  (* Creation:
   *   new dtd
   * creates a new, empty DTD object without any declaration, without a root
   * element, without an ID.
   *)
  ?swarner:Pxp_core_types.symbolic_warnings ->
  Pxp_core_types.collect_warnings -> 
  Pxp_core_types.rep_encoding ->
  object
    method root : string option
      (* get the name of the root element if present. This is the name
       * following "<!DOCTYPE". If there is no DOCTYPE declaration, 
       * this method will return None.
       *)

    method set_root : string -> unit
      (* set the name of the root element. This method can be invoked 
       * only once (usually by the parser)
       *)

    method id : Pxp_core_types.dtd_id option
      (* get the identifier for this DTD. Possible return values:
       * None: There is no DOCTYPE declaration, or only
       *    "<!DOCTYPE name>"
       * Some Internal: There is a DOCTYPE declaration with material
       *    in brackets like "<!DOCTYPE name [ declarations ... ]>"
       * Some(External xid): There is a DOCTYPE declaration with
       *    a SYSTEM or PUBLIC identifier (described by xid), but without
       *    brackets, i.e. "<!DOCTYPE name SYSTEM '...'>" or 
       *    "<!DOCTYPE name PUBLIC '...' '...'>".
       * Some(Derived xid): There is a DOCTYPE declaration with
       *    a SYSTEM or PUBLIC identifier (described by xid), _and_ with
       *    brackets
       *)

    method set_id : Pxp_core_types.dtd_id -> unit
      (* set the identifier. This method can be invoked only once *)

    method encoding : Pxp_core_types.rep_encoding
      (* returns the encoding used for character representation *)


    method allow_arbitrary : unit
      (* After this method has been invoked, the object changes its behaviour:
       * - elements and notations that have not been added may be used in an
       *   arbitrary way; the methods "element" and "notation" indicate this
       *   by raising Undeclared instead of Validation_error.
       *)

    method disallow_arbitrary : unit

    method arbitrary_allowed : bool
      (* Returns whether arbitrary contents are allowed or not. *)

    method standalone_declaration : bool
      (* Whether there is a 'standalone' declaration or not. Strictly 
       * speaking, this declaration is not part of the DTD, but it is
       * included here because of practical reasons. 
       * If not set, this property defaults to 'false'.
       *)

    method set_standalone_declaration : bool -> unit
      (* Sets the 'standalone' declaration. *)


    method namespace_manager : namespace_manager
      (* For namespace-aware implementations of the node class, this method
       * returns the namespace manager. If the namespace manager has not been
       * set, the exception Not_found is raised.
       *)

    method set_namespace_manager : namespace_manager -> unit
      (* Sets the namespace manager as returned by namespace_manager.
       *)

    method add_element : dtd_element -> unit
      (* add the given element declaration to this DTD. Raises Not_found
       * if there is already an element declaration with the same name.
       *)

    method add_gen_entity : Pxp_entity.entity -> bool -> unit
      (* add_gen_entity e extdecl:
       * add the entity 'e' as general entity to this DTD (general entities
       * are those represented by &name;). If there is already a declaration
       * with the same name, the second definition is ignored; as exception from
       * this rule, entities with names "lt", "gt", "amp", "quot", and "apos"
       * may only be redeclared with a definition that is equivalent to the
       * standard definition; otherwise a Validation_error is raised.
       *
       * 'extdecl': 'true' indicates that the entity declaration occurs in
       * an external entity. (Used for the standalone check.)
       *)

    method add_par_entity : Pxp_entity.entity -> unit
      (* add the given entity as parameter entity to this DTD (parameter
       * entities are those represented by %name;). If there is already a 
       * declaration with the same name, the second definition is ignored.
       *)

    method add_notation : dtd_notation -> unit
      (* add the given notation to this DTD. If there is already a declaration
       * with the same name, a Validation_error is raised.
       *)

    method add_pinstr : proc_instruction -> unit
      (* add the given processing instruction to this DTD. *)

    method element : string -> dtd_element
      (* looks up the element declaration with the given name. Raises 
       * Validation_error if the element cannot be found. (If "allow_arbitrary"
       * has been invoked before, Undeclared is raised instead.)
       *)

    method element_names : string list
      (* returns the list of the names of all element declarations. *)

    method gen_entity : string -> (Pxp_entity.entity * bool)
      (* let e, extdecl = obj # gen_entity n:
       * looks up the general entity 'e' with the name 'n'. Raises
       * WF_error if the entity cannot be found.
       * 'extdecl': indicates whether the entity declaration occured in an 
       * external entity.
       *)

    method gen_entity_names : string list
      (* returns the list of all general entity names *)

    method par_entity : string -> Pxp_entity.entity
      (* looks up the parameter entity with the given name. Raises
       * WF_error if the entity cannot be found.
       *)

    method par_entity_names : string list
      (* returns the list of all parameter entity names *)

    method notation : string -> dtd_notation
      (* looks up the notation declaration with the given name. Raises
       * Validation_error if the notation cannot be found. (If "allow_arbitrary"
       * has been invoked before, Unrestricted is raised instead.)
       *)

    method notation_names : string list
      (* Returns the list of the names of all added notations *)

    method pinstr : string -> proc_instruction list
      (* looks up all processing instructions with the given target.
       * The "target" is the identifier following "<?".
       * Note: It is not possible to find out the exact position of the
       * processing instruction.
       *)

    method pinstr_names : string list
      (* Returns the list of the names (targets) of all added pinstrs *)

    method validate : unit
      (* ensures that the DTD is valid. This method is optimized such that
       * actual validation is only performed if DTD has changed.
       * If the DTD is invalid, mostly a Validation_error is raised,
       * but other exceptions are possible, too.
       *)

    method only_deterministic_models : unit
      (* Succeeds if all regexp content models are deterministic. 
       * Otherwise Validation_error.
       *)

    method write : 
             Pxp_core_types.output_stream -> 
	     Pxp_core_types.encoding -> 
	     bool -> 
	       unit
      (* write os enc doctype:
       * Writes the DTD as 'enc'-encoded string to 'os'. If 'doctype', a 
       * DTD like <!DOCTYPE root [ ... ]> is written. If 'not doctype',
       * only the declarations are written (the material within the
       * square brackets).
       * The entity definitions are not written. However, it is ensured that
       * the generated string does not contain any reference to an entity.
       * The reason for the omission of the entites is that there is no
       * generic way of writing references to external entities.
       *)

    method write_ref : 
             Pxp_core_types.output_stream -> 
	     Pxp_core_types.encoding -> 
	       unit
     (* write_ref os enc:
      * Writes a reference to the DTD as 'enc'-encoded string to 'os'.
      * The reference looks as follows:
      *   <!DOCTYPE root SYSTEM ... > or
      *   <!DOCTYPE root PUBLIC ... >
      * Of course, the DTD must have an external ID:
      * - dtd#id = External(System ...) or
      * - dtd#id = External(Public ...)
      * If the DTD is internal or mixed, the method [write_ref] will fail.
      * If the ID is anonymous or private, the method will fail, too.
      *)

    (*----------------------------------------*)
    method invalidate : unit
      (* INTERNAL METHOD *)
    method warner : Pxp_core_types.collect_warnings
      (* INTERNAL METHOD *)
    method swarner : Pxp_core_types.symbolic_warnings option
      (* INTERNAL METHOD *)
  end

(*$-*)

(*$ markup-dtd2.mli *)

(* ---------------------------------------------------------------------- *)

and dtd_element : dtd -> string -> 
  (* Creation:
   *   new dtd_element init_dtd init_name:
   * creates a new dtd_element object for init_dtd with init_name.
   * The strings are represented in the same encoding as init_dtd.
   *)
  object

    method name : string
      (* returns the name of the declared element *)

    method externally_declared : bool
      (* returns whether the element declaration occurs in an external
       * entity.
       *)

    method content_model : Pxp_core_types.content_model_type
      (* get the content model of this element declaration, or Unspecified *)

    method content_dfa : Pxp_dfa.dfa_definition option
      (* return the DFA of the content model if there is a DFA, or None.
       * A DFA exists only for regexp style content models which are
       * deterministic.
       *)

    method set_cm_and_extdecl : 
             Pxp_core_types.content_model_type -> bool -> unit
      (* set_cm_and_extdecl cm extdecl:
       * set the content model to 'cm'. Once the content model is not 
       * Unspecified, it cannot be set to a different value again.
       * Furthermore, it is set whether the element occurs in an external
       * entity ('extdecl').
       *)

    method encoding : Pxp_core_types.rep_encoding
      (* Return the encoding of the strings *)

    method allow_arbitrary : unit
      (* After this method has been invoked, the object changes its behaviour:
       * - attributes that have not been added may be used in an
       *   arbitrary way; the method "attribute" indicates this
       *   by raising Undeclared instead of Validation_error.
       *)

    method disallow_arbitrary : unit

    method arbitrary_allowed : bool
      (* Returns whether arbitrary attributes are allowed or not. *)

    method attribute : string -> 
                         Pxp_core_types.att_type * Pxp_core_types.att_default
      (* get the type and default value of a declared attribute, or raise
       * Validation_error if the attribute does not exist.
       * If 'arbitrary_allowed', the exception Undeclared is raised instead
       * of Validation_error.
       *)

    method attribute_violates_standalone_declaration : 
               string -> string option -> bool
      (* attribute_violates_standalone_declaration name v:
       * Checks whether the attribute 'name' violates the "standalone"
       * declaration if it has value 'v'.
       * The method returns true if:
       * - The attribute declaration occurs in an external entity, 
       * and if one of the two conditions holds:
       * - v = None, and there is a default for the attribute value
       * - v = Some s, and the type of the attribute is not CDATA,
       *   and s changes if normalized according to the rules of the
       *   attribute type.
       *
       * The method raises Validation_error if the attribute does not exist.
       * If 'arbitrary_allowed', the exception Undeclared is raised instead
       * of Validation_error.
       *)

    method attribute_names : string list
      (* get the list of all declared attributes *)

    method names_of_required_attributes : string list
      (* get the list of all attributes that are specified as required 
       * attributes
       *)

    method id_attribute_name : string option
      (* Returns the name of the attribute with type ID, or None. *)

    method idref_attribute_names : string list
      (* Returns the names of the attributes with type IDREF or IDREFS. *)

    method add_attribute : string -> 
                           Pxp_core_types.att_type -> 
			   Pxp_core_types.att_default -> 
			   bool ->
			     unit
      (* add_attribute name type default extdecl:
       * add an attribute declaration for an attribute with the given name,
       * type, and default value. If there is more than one declaration for
       * an attribute name, the first declaration counts; the other declarations
       * are ignored.
       * 'extdecl': if true, the attribute declaration occurs in an external
       * entity. This property is used to check the "standalone" attribute.
       *)

    method validate : unit
      (* checks whether this element declaration (i.e. the content model and
       * all attribute declarations) is valid for the associated DTD.
       * Raises mostly Validation_error if the validation fails.
       *)

    method write : 
             Pxp_core_types.output_stream -> Pxp_core_types.encoding -> unit
      (* write os enc:
       * Writes the <!ELEMENT ... > declaration to 'os' as 'enc'-encoded string.
       *)

    method internal_vr : validation_record
      (* INTERNAL METHOD: Returns the validation record for this element type. 
       *)
  end

(* ---------------------------------------------------------------------- *)

and dtd_notation : 
       string -> Pxp_core_types.ext_id -> Pxp_core_types.rep_encoding ->
  (* Creation:
   *    new dtd_notation a_name an_external_ID init_encoding
   * creates a new dtd_notation object with the given name and the given
   * external ID.
   *)
  object
    method name : string
    method ext_id : Pxp_core_types.ext_id
    method encoding : Pxp_core_types.rep_encoding

    method write : 
             Pxp_core_types.output_stream -> Pxp_core_types.encoding -> unit
      (* write_compact_as_latin1 os enc:
       * Writes the <!NOTATION ... > declaration to 'os' as 'enc'-encoded 
       * string.
       *)

  end

(* ---------------------------------------------------------------------- *)

and proc_instruction : string -> string -> Pxp_core_types.rep_encoding ->
  (* Creation:
   *   new proc_instruction a_target a_value
   * creates a new proc_instruction object with the given target string and
   * the given value string. 
   * Note: A processing instruction is written as <?target value?>. 
   *)
  object
    method target : string
    method value : string
    method encoding : Pxp_core_types.rep_encoding

    method write : 
             Pxp_core_types.output_stream -> Pxp_core_types.encoding -> unit
      (* write os enc:
       * Writes the <?...?> PI to 'os' as 'enc'-encoded string.
       *)

    method parse_pxp_option : (string * string * (string * string) list)
      (* Parses a PI containing a PXP option. Such PIs are formed like:
       *   <?target option-name option-att="value" option-att="value" ... ?>
       * The method returns a triple
       *   (target, option-name, [option-att, value; ...])
       * or raises Error.
       *)

  end

;;

(*$-*)

(* ---------------------------------------------------------------------- *)

type source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (Pxp_core_types.ext_id * Pxp_reader.resolver)
  | XExtID of (Pxp_core_types.ext_id * string option * Pxp_reader.resolver)
  (* Sources are pairs of (1) names of entities to open, and (2) methods
   * of opening entities. See Pxp_yacc for more documentation.
   *)

(* ---------------------------------------------------------------------- *)

(* Useful properties of entities: The following submodule exports all 
 * stable properties of the entity classes. Please use this module, and
 * not Pxp_entity to access entities.
 *)

module Entity : sig
  val get_name : Pxp_entity.entity -> string
      (* Return the name of the entity. *)

  val get_full_name : Pxp_entity.entity -> string
      (* The full name includes the ID, too (for diagnostics messages) *)
 
  val get_encoding : Pxp_entity.entity -> Pxp_core_types.rep_encoding
      (* Return the encoding of the internal representation of the entity *)

  val get_type : Pxp_entity.entity -> 
                   [ `External | `Internal | `NDATA ]
      (* Returns the type of the entity. *)

  val replacement_text : Pxp_entity.entity -> string
      (* Return the replacement text of the entity. Works for both
       * internal and external entities.
       *)

  val get_xid : Pxp_entity.entity -> Pxp_core_types.ext_id option
      (* Returns the external ID for external and NDATA entities, and None
       * for internal entities
       * TRAP: The external ID may be a relative SYSTEM ID, and it is not
       * known to which base ID the relative ID must be resolved. So the
       * external ID may be meaningless.
       *)

  val get_resolver_id : Pxp_entity.entity -> Pxp_core_types.resolver_id option
      (* Returns the resolver ID for external entities, and None for other
       * entities. This is the version as returned by the [active_id] method
       * by the resolver.
       * The resolver ID contains more information than the external ID,
       * for example the base URL relative to which SYSTEM IDs should
       * be interpreted.
       *)

  (* CHECK: There is still no base URL for NDATA entities *)

  val get_notation : Pxp_entity.entity -> string option
      (* Returns the notation of NDATA entities, and None for the other
       * entity types
       *)

  val create_internal_entity : 
      name:string -> value:string -> dtd -> Pxp_entity.entity
      (* Creates an internal entity. The name and the value must be
       * encoded in the same encoding as the DTD.
       * Note that if the entity is to be used as parameter entity,
       * the first and the last characters of the value should be 
       * spaces.
       *)

  val create_ndata_entity :
      name:string -> xid:Pxp_core_types.ext_id -> notation:string -> dtd -> 
	Pxp_entity.entity
      (* Creates an NDATA entity. The name and the notation must be encoded
       * in the same encoding as the DTD. The external ID must be encoded
       * as UTF-8 string (like all external IDs).
       *)

  val create_external_entity :
      ?doc_entity:bool ->
      ?system_base:string ->
      name:string -> 
      xid:Pxp_core_types.ext_id -> 
      resolver:Pxp_reader.resolver ->
      dtd ->
	Pxp_entity.entity
      (* Creates a reference to an external entity. The name must be encoded
       * in the same encoding as the DTD. The external ID must be encoded
       * as UTF-8 string (like all external IDs).
       *
       * ~doc_entity: If true, the entity is a document entity. XML requires
       *   some additional restrictions for document entities. The default for
       *   the argument is false.
       * ~system_base: The base URL if SYSTEM identifiers are passed 
       *   as [xid]
       *)

  val from_external_source :
      ?doc_entity:bool ->
      name:string -> 
      dtd -> 
      source -> 
	Pxp_entity.entity
      (* Creates an external entity that reads from the passed source *)

end
;;



