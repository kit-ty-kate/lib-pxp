(* $Id: pxp_dtd.mli,v 1.6 2000/07/23 02:16:33 gerd Exp $
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
 * ======================================================================
 *
 *)


class dtd :
  (* Creation:
   *   new dtd
   * creates a new, empty DTD object without any declaration, without a root
   * element, without an ID.
   *)
  Pxp_types.collect_warnings -> 
  Pxp_types.rep_encoding ->
  object
    method root : string option
      (* get the name of the root element if present *)

    method set_root : string -> unit
      (* set the name of the root element. This method can be invoked 
       * only once
       *)

    method id : Pxp_types.dtd_id option
      (* get the identifier for this DTD *)

    method set_id : Pxp_types.dtd_id -> unit
      (* set the identifier. This method can be invoked only once *)

    method encoding : Pxp_types.rep_encoding
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
       * has been invoked before, Unrestricted is raised instead.)
       *)

    method element_names : string list
      (* returns the list of the names of all element declarations. *)

    method gen_entity : string -> (Pxp_entity.entity * bool)
      (* let e, extdecl = obj # gen_entity n:
       * looks up the general entity 'e' with the name 'n'. Raises
       * Validation_error if the entity cannot be found.
       * 'extdecl': indicates whether the entity declaration occured in an 
       * external entity.
       *)

    method gen_entity_names : string list
      (* returns the list of all general entity names *)

    method par_entity : string -> Pxp_entity.entity
      (* looks up the parameter entity with the given name. Raises
       * Validation_error if the entity cannot be found.
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

    method write : Pxp_types.output_stream -> Pxp_types.encoding -> bool -> unit
      (* write_compact_as_latin1 os enc doctype:
       * Writes the DTD as 'enc'-encoded string to 'os'. If 'doctype', a 
       * DTD like <!DOCTYPE root [ ... ]> is written. If 'not doctype',
       * only the declarations are written (the material within the
       * square brackets).
       *)

    method write_compact_as_latin1 : Pxp_types.output_stream -> bool -> unit
      (* DEPRECATED METHOD; included only to keep compatibility with
       * older versions of the parser
       *)


    (*----------------------------------------*)
    method invalidate : unit
      (* INTERNAL METHOD *)
    method warner : Pxp_types.collect_warnings
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

    method content_model : Pxp_types.content_model_type
      (* get the content model of this element declaration, or Unspecified *)

    method content_dfa : Pxp_dfa.dfa_definition option
      (* return the DFA of the content model if there is a DFA, or None.
       * A DFA exists only for regexp style content models which are
       * deterministic.
       *)

    method set_cm_and_extdecl : Pxp_types.content_model_type -> bool -> unit
      (* set_cm_and_extdecl cm extdecl:
       * set the content model to 'cm'. Once the content model is not 
       * Unspecified, it cannot be set to a different value again.
       * Furthermore, it is set whether the element occurs in an external
       * entity ('extdecl').
       *)

    method encoding : Pxp_types.rep_encoding
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
                         Pxp_types.att_type * Pxp_types.att_default
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
                           Pxp_types.att_type -> 
			   Pxp_types.att_default -> 
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

    method write : Pxp_types.output_stream -> Pxp_types.encoding -> unit
      (* write_compact_as_latin1 os enc:
       * Writes the <!ELEMENT ... > declaration to 'os' as 'enc'-encoded string.
       *)

    method write_compact_as_latin1 : Pxp_types.output_stream -> unit
      (* DEPRECATED METHOD; included only to keep compatibility with
       * older versions of the parser
       *)
  end

(* ---------------------------------------------------------------------- *)

and dtd_notation : string -> Pxp_types.ext_id -> Pxp_types.rep_encoding ->
  (* Creation:
   *    new dtd_notation a_name an_external_ID init_encoding
   * creates a new dtd_notation object with the given name and the given
   * external ID.
   *)
  object
    method name : string
    method ext_id : Pxp_types.ext_id
    method encoding : Pxp_types.rep_encoding

    method write : Pxp_types.output_stream -> Pxp_types.encoding -> unit
      (* write_compact_as_latin1 os enc:
       * Writes the <!NOTATION ... > declaration to 'os' as 'enc'-encoded 
       * string.
       *)

    method write_compact_as_latin1 : Pxp_types.output_stream -> unit
      (* DEPRECATED METHOD; included only to keep compatibility with
       * older versions of the parser
       *)

  end

(* ---------------------------------------------------------------------- *)

and proc_instruction : string -> string -> Pxp_types.rep_encoding ->
  (* Creation:
   *   new proc_instruction a_target a_value
   * creates a new proc_instruction object with the given target string and
   * the given value string. 
   * Note: A processing instruction is written as <?target value?>. 
   *)
  object
    method target : string
    method value : string
    method encoding : Pxp_types.rep_encoding

    method write : Pxp_types.output_stream -> Pxp_types.encoding -> unit
      (* write_compact_as_latin1 os enc:
       * Writes the <?...?> PI to 'os' as 'enc'-encoded string.
       *)

    method write_compact_as_latin1 : Pxp_types.output_stream -> unit
      (* DEPRECATED METHOD; included only to keep compatibility with
       * older versions of the parser
       *)

  end

;;

(*$-*)

(* ======================================================================
 * History:
 * 
 * $Log: pxp_dtd.mli,v $
 * Revision 1.6  2000/07/23 02:16:33  gerd
 * 	Support for DFAs.
 *
 * Revision 1.5  2000/07/16 16:34:41  gerd
 * 	New method 'write', the successor of 'write_compact_as_latin1'.
 *
 * Revision 1.4  2000/07/14 13:56:49  gerd
 * 	Added methods id_attribute_name and idref_attribute_names.
 *
 * Revision 1.3  2000/07/09 00:13:37  gerd
 * 	Added methods gen_entity_names, par_entity_names.
 *
 * Revision 1.2  2000/06/14 22:19:06  gerd
 * 	Added checks such that it is impossible to mix encodings.
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
 * Old logs from markup_dtd.ml:
 *
 * Revision 1.11  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.10  2000/05/27 19:20:38  gerd
 * 	Changed the interfaces for the standalone check: New
 * methods: standalone_declaration, set_standalone_declaration,
 * externally_declared, attribute_violates_standalone_declaration.
 * 	The method set_content_model has been renamed to
 * set_cm_and_extdecl; it now initializes also whether the element
 * has been declared in an external entity.
 * 	Methods add_gen_entity and gen_entity pass an additional
 * boolean argument containing whether the declaration of the
 * general entity happened in an external entity.
 * 	Method add_attribute expects this argument, too, which
 * states whether the declaration of the attribute happened in an
 * external entity.
 *
 * Revision 1.9  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.8  2000/05/06 23:10:26  gerd
 * 	allow_arbitrary for elements, too.
 *
 * Revision 1.7  2000/05/01 20:42:52  gerd
 *         New method write_compact_as_latin1.
 *
 * Revision 1.6  2000/03/11 22:58:15  gerd
 * 	Updated to support Markup_codewriter.
 *
 * Revision 1.5  2000/02/22 02:32:02  gerd
 * 	Updated.
 *
 * Revision 1.4  1999/11/09 22:15:41  gerd
 * 	Added method "arbitrary_allowed".
 *
 * Revision 1.3  1999/09/01 16:21:56  gerd
 * 	"dtd" classes have now an argument that passes a "warner".
 *
 * Revision 1.2  1999/08/15 02:20:23  gerd
 *         New feature: a DTD can allow arbitrary elements.
 *
 * Revision 1.1  1999/08/10 00:35:51  gerd
 * 	Initial revision.
 *
 * 
 *)
