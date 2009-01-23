(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


(* The module type CORE_TYPES is the signature of Pxp_core_types, and part of
 * the signature of Pxp_types.
 *)

(** Types that are used both internally and in the programmer's interface *)
module type CORE_TYPES = sig
  module StringMap : Map.S with type key = string
    (** For maps with string keys *)

  (** {2 Identifiers} *)

  type ext_id = Pxp_type_anchor.ext_id =
      System of string
    | Public of (string * string)
    | Anonymous
    | Private of private_id  (** *)
  (** External identifiers are names for documents. A [System] identifier is
      a URL. PXP (without extensions) only supports file URLs in the form
      [file:///directory/directory/.../file]. Note that the percent encoding
      (% plus two hex digits) is supported in file URLs. A public identifier
      can be looked up in a catalog to find a local copy of the file; this
      type is mostly used for well-known documents (e.g. after
      standardization). A public identifier can be accompanied by a
      system identifier ([Public(pubid,sysid)]), but the system identifier
      can be the empty string. The value [Anonymous] should not be used
      to identify a real document; it is more thought as a placeholder when
      an ID is not yet known. [Private] identifiers are used by PXP internally.
      These identifiers have, unlike system or public IDs, no textual
      counterparts.

      The identifiers are encoded as UTF-8 strings.
   *)

  and private_id = Pxp_type_anchor.private_id
  (** A private ID is an opaque identifier *)

  val allocate_private_id : unit -> private_id
    (** Get a new unique private ID *)

  type resolver_id = Pxp_type_anchor.resolver_id =
      { rid_private: private_id option;
	rid_public:  string option;
	rid_system:  string option;
	rid_system_base: string option;  (* when rid_system is relative *)
      }
    (** A resolver ID is a version of external identifiers used during
        resolving (i.e. the process of mapping the identifier to a real
        resource). The same entity can have several names during resolving:
        one private ID, one public ID, and one system ID. For resolving
        system IDs, the base URL is also remembered (usually the system ID
        of the opener of the entity).
     *)

  val resolver_id_of_ext_id : ext_id -> resolver_id
    (** The standard way of converting an ext_id into a resolver ID.
     * A [System] ID is turned into a [resolver_id] where only [rid_system] is
     * set. A [Public] ID is turned into a [resolver_id] where both [rid_public]
     * and [rid_system] are set. A [Private] ID is turned into a [resolver_id]
     * where only [rid_private] is set. An [Anonymous] ID is turned into a
     * [resolver_id] without any value (all components are None).
     *)


  type dtd_id = Pxp_type_anchor.dtd_id =
      External of ext_id       (** DTD is completely external *)
    | Derived of ext_id        (** DTD is derived from an external DTD *)
    | Internal                 (** DTD is completely internal *)
    (** Identifier for DTDs *)

  (** {2 Content models (in DTDs)} *)

  type content_model_type = Pxp_type_anchor.content_model_type =
    | Unspecified              (** A specification of the model has not yet been
				* found
				*)
    | Empty                    (** Nothing is allowed as content *)
    | Any                      (** Everything is allowed as content *)
    | Mixed of mixed_spec list (** The contents consist of elements and [PCDATA]
				* in arbitrary order. What is allowed in
				* particular is given as [mixed_spec].
				*)
    | Regexp of regexp_spec    (** The contents are elements following this
				* regular expression
				*)
    (** Element declaration in a DTD *)

  and mixed_spec = Pxp_type_anchor.mixed_spec =
      MPCDATA                  (** [PCDATA] children are allowed *)
    | MChild of string         (** This kind of Element is allowed *)
    (** Children of an element in "mixed"-style declaration *)

  and regexp_spec = Pxp_type_anchor.regexp_spec =
      Optional of regexp_spec  (** subexpression? *)
    | Repeated of regexp_spec  (** subexpression* *)
    | Repeated1 of regexp_spec (** subexpression+ *)
    | Alt of regexp_spec list  (** subexpr1 | subexpr2 | ... | subexprN *)
    | Seq of regexp_spec list  (** subexpr1 , subexpr2 , ... , subexprN *)
    | Child of string          (** This kind of Element is allowed here *)
    (** Children of an element in a regexp-style declaration *)

  type att_type = Pxp_type_anchor.att_type =
      A_cdata                    (** [CDATA] *)
    | A_id                       (** [ID] *)
    | A_idref                    (** [IDREF] *)
    | A_idrefs                   (** [IDREFS] *)
    | A_entity                   (** [ENTITY] *)
    | A_entities                 (** [ENTITIES] *)
    | A_nmtoken                  (** [NMTOKEN] *)
    | A_nmtokens                 (** [NMTOKENS] *)
    | A_notation of string list  (** [NOTATION] (name1 | name2 | ... | nameN) *)
    | A_enum of string list      (** (name1 | name2 | ... | nameN) *)
    (** Attribute declaration in a DTD *)

  type att_default = Pxp_type_anchor.att_default =
      D_required           (** [#REQUIRED] *)
    | D_implied            (** [#IMPLIED] *)
    | D_default of string  (** a value default -- the value is already expanded *)
    | D_fixed of string    (** [FIXED] value default -- the value is already expanded *)
    (** Default value of an attribute *)

  (** {2 Attribute value} *)

  type att_value = Pxp_type_anchor.att_value =
      Value of string
    | Valuelist of string list
    | Implied_value (** *)
	(** Enumerates the possible values of an attribute:
	 *   - [Value s]: The attribute is declared as a non-list type, or the
	 *     attribute is undeclared; and the attribute is either defined with
	 *     value ["s"], or it is missing but has the default value [s].
	 *   - [[Valuelist [s1;...;sk]]]: The attribute is declared as a list type,
	 *     and the attribute is either defined with value ["s1 ... sk"]
         *     (space-separated words),
	 *     or it is missing but has the default value ["s1 ... sk"].
	 *   - [Implied_value]: The attribute is declared without default value,
	 *     and there is no definition for the attribute.
	 *)

  (** {2 Warnings} *)

  class type collect_warnings =
  object
    method warn : string -> unit
  end
  (** This object is sometimes used for outputting user warnings *)


  class drop_warnings : collect_warnings
  (** Drop any warnings *)


  type warning =
      [ `W_code_point_cannot_be_represented of int
      | `W_name_is_reserved_for_extensions of string
      | `W_multiple_ATTLIST_declarations of string
      | `W_multiple_attribute_declarations of string * string
      | `W_element_mentioned_but_not_declared of string
      | `W_entity_declared_twice of string
      | `W_XML_version_not_supported of string
      ]
    (** Kinds of warnings *)

  class type symbolic_warnings =
  object
    method warn : warning -> unit
  end
  (** This object is sometimes used for outputting user warnings *)


  val string_of_warning : warning -> string
    (** Turn the warning into a human-readable message *)

  val warn : symbolic_warnings option -> collect_warnings -> warning -> unit
    (** Send a warning to the [symbolic_warnings] object, and then to the
     * [collect_warnings] object.
     *)


  (** {2 Encoding} *)

  type encoding = Netconversion.encoding
    (** For the representation of external resources (files etc.)
        we accept all encodings for character sets which are defined in
        Netconversion (package netstring).
     *)

  type rep_encoding =
      [ `Enc_utf8       (* UTF-8 *)
      | `Enc_usascii
      | `Enc_iso88591   (* ISO-8859-1 *)
      | `Enc_iso88592
      | `Enc_iso88593
      | `Enc_iso88594
      | `Enc_iso88595
      | `Enc_iso88596
      | `Enc_iso88597
      | `Enc_iso88598
      | `Enc_iso88599
      | `Enc_iso885910
      | `Enc_iso885913
      | `Enc_iso885914
      | `Enc_iso885915
      | `Enc_iso885916
      | `Enc_koi8r
      | `Enc_windows1250
      | `Enc_windows1251
      | `Enc_windows1252
      | `Enc_windows1253
      | `Enc_windows1254
      | `Enc_windows1255
      | `Enc_windows1256
      | `Enc_windows1257
      | `Enc_windows1258
      | `Enc_cp437
      | `Enc_cp737
      | `Enc_cp775
      | `Enc_cp850
      | `Enc_cp852
      | `Enc_cp855
      | `Enc_cp856
      | `Enc_cp857
      | `Enc_cp860
      | `Enc_cp861
      | `Enc_cp862
      | `Enc_cp863
      | `Enc_cp864
      | `Enc_cp865
      | `Enc_cp866
      | `Enc_cp869
      | `Enc_cp874
      | `Enc_cp1006
      | `Enc_macroman
      ]
    (** The subset of [encoding] that may be used for the internal representation
     * of strings. The common property of the  following encodings is that
     * they are ASCII-compatible - the PXP code relies on that.
     *)


  (** {2 Exceptions} *)

  exception Validation_error of string
    (** Violation of a validity constraint *)

  exception WF_error of string
    (** Violation of a well-formedness constraint *)

  exception Namespace_error of string
    (** Violation of a namespace constraint *)

  exception Error of string
    (** Other error *)

  exception Character_not_supported

  exception At of (string * exn)
    (** The string is a description where the exn happened. The exn value can
     * again be [At(_,_)] (for example, when an entity within an entity causes
     * the error).
     *)

  exception Undeclared
    (** Indicates that no declaration is available and because of this every kind
     * of usage is allowed. (Raised by some DTD methods.)
     *)

  exception Method_not_applicable of string
    (** Indicates that a method has been called that is not applicable for
     * the class. The argument is the name of the method.
     *)

  exception Namespace_method_not_applicable of string
    (** Indicates that the called method is a namespace method but that the
     * object does not support namespaces. The argument is the name of the method.
     *)

  exception Not_competent
    (** The resolver cannot open this kind of entity ID *)

  exception Not_resolvable of exn
    (** While opening the entity, the nested exception occurred *)

  exception Namespace_not_managed of string
    (** A namespace URI is used but not declared in the namespace manager.
     * The string argument is the URI in question.
     *)

  exception Namespace_prefix_not_managed of string
    (** A namespace prefix is used but not declared in the namespace manager.
     * The string argument is the prefix in question.
     *)

  exception Namespace_not_in_scope of string
    (** The namespace scope does not know the URI *)


  val string_of_exn : exn -> string
    (** Converts a PXP exception into a readable string *)


  (** {2 Output destination} *)

  type output_stream =
      [ `Out_buffer of Buffer.t
      | `Out_channel of out_channel
      | `Out_function of (string -> int -> int -> unit)
      | `Out_netchannel of Netchannels.out_obj_channel
      ]
    (** Designates an output destination for several printers:
        - [`Out_buffer b]: Output to buffer [b]
        - [`Out_channel ch]: Output to channel [ch]
        - [`Out_function f]: Output to function [f]. The function [f] is
          used like [Pervasives.output_string].
        - [`Out_netchannel n]: Output to the ocamlnet channel [n]
     *)

  val write : output_stream -> string -> int -> int -> unit
    (** [write os s pos len]: Writes the string (portion) to the
         buffer/channel/stream *)

  (** {2 Pools} *)

  type pool = Pxp_type_anchor.pool
      (** A pool designates a way to increase string sharing *)

  val make_probabilistic_pool : ?fraction:float -> int -> pool
	(** A probalistic string pool tries to map strings to pool strings in order
	 * to make it more likely that equal strings are stored in the same memory
	 * block.
	 * The int argument is the size of the pool; this is the number of entries
	 * of the pool. However, not all entries of the pool are used; the fraction
	 * argument (default: 0.3) determines the fraction of the actually used
	 * entries. The higher the fraction is, the more strings can be managed
	 * at the same time; the lower the fraction is, the more likely it is that
	 * a new string can be added to the pool.
	 *)

  val pool_string : pool -> string -> string
	(** Tries to find the passed string in the pool; if the string is in the
	 * pool, the pool string is returned. Otherwise, the function tries to
	 * add the passed string to the pool, and the passed string is returned.
	 *)

end (* of CORE_TYPES *)

