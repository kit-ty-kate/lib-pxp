(* $Id: pxp_core_types_type.mli,v 1.2 2003/06/15 18:19:56 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


(* The module type CORE_TYPES is the signature of Pxp_core_types, and part of
 * the signature of Pxp_types.
 *)

module type CORE_TYPES = sig
  type ext_id = Pxp_type_anchor.ext_id =
      System of string
    | Public of (string * string)
    | Anonymous
    | Private of private_id
	
  and private_id = Pxp_type_anchor.private_id

  (* External identifiers are either "system identifiers" (filenames or URLs),
   * or "public identifiers" Public(id,sysid) where "id" is the representation
   * of the public ID, and "sysid" a fallback system ID, or the empty string.
   *
   * New in PXP 1.0: Sometimes the external ID is not known. This case can be
   * referred to as Anonymous ID (e.g. to initialize a fresh variable).
   *
   * New in PXP 1.1: Sometimes the external ID needs some special encoding.
   * Private IDs can be used in these cases. Private IDs can be allocated
   * using allocate_private_id (below), and the IDs will be unique and
   * different from all other IDs. This makes it simpler to add hooks
   * recognizing the special IDs they are competent for.
   *
   * Encoding: The identifiers are _always_ encoded as UTF8 strings,
   * regardless of whether another encoding is configured for the parser.
   *)

  val allocate_private_id : unit -> private_id
    (* Get a new unique private ID *)

  type resolver_id = Pxp_type_anchor.resolver_id =
      { rid_private: private_id option;
	rid_public:  string option;
	rid_system:  string option;
	rid_system_base: string option;  (* when rid_system is relative *)
      }
      (* resolver IDs are used instead of ext_id in resolvers. The difference
       * is that an entity can have three names at the same time: a private
       * name, a PUBLIC name, and a SYSTEM name. Furthermore, the base URL
       * of the system names is stored which is usually just the system name
       * of the opener.
       *)

  val resolver_id_of_ext_id : ext_id -> resolver_id
    (* The standard method of converting an ext_id into a resolver ID.
     * A System ID is turned into a resolver_id where only rid_system is
     * set. A Public ID is turned into a resolver_id where both rid_public
     * and rid_system are set. A Private ID is turned into a resolver_id
     * where only rid_private is set. An Anonymous ID is turned into a
     * resolver_id without any value (all components are None).
     *)


  type dtd_id = Pxp_type_anchor.dtd_id =
      External of ext_id       (* DTD is completely external *)
    | Derived of ext_id        (* DTD is derived from an external DTD *)
    | Internal                 (* DTD is completely internal *)

  type content_model_type = Pxp_type_anchor.content_model_type =
      Unspecified              (* A specification of the model has not yet been
				* found
				*)
    | Empty                    (* Nothing is allowed as content *)
    | Any                      (* Everything is allowed as content *)
    | Mixed of mixed_spec list (* The contents consist of elements and PCDATA
				* in arbitrary order. What is allowed in
				* particular is given as mixed_spec.
				*)
    | Regexp of regexp_spec    (* The contents are elements following this
				* regular expression
				*)

  and mixed_spec = Pxp_type_anchor.mixed_spec =
      MPCDATA                  (* PCDATA children are allowed *)
    | MChild of string         (* This kind of Element is allowed *)

  and regexp_spec = Pxp_type_anchor.regexp_spec =
      Optional of regexp_spec  (* subexpression? *)
    | Repeated of regexp_spec  (* subexpression* *)
    | Repeated1 of regexp_spec (* subexpression+ *)
    | Alt of regexp_spec list  (* subexpr1 | subexpr2 | ... | subexprN *)
    | Seq of regexp_spec list  (* subexpr1 , subexpr2 , ... , subexprN *)
    | Child of string          (* This kind of Element is allowed here *)


  type att_type = Pxp_type_anchor.att_type =
      A_cdata                    (* CDATA *)
    | A_id                       (* ID *)
    | A_idref                    (* IDREF *)
    | A_idrefs                   (* IDREFS *)
    | A_entity                   (* ENTITY *)
    | A_entities                 (* ENTiTIES *)
    | A_nmtoken                  (* NMTOKEN *)
    | A_nmtokens                 (* NMTOKENS *)
    | A_notation of string list  (* NOTATION (name1 | name2 | ... | nameN) *)
    | A_enum of string list      (* (name1 | name2 | ... | nameN) *)


  type att_default = Pxp_type_anchor.att_default =
      D_required           (* #REQUIRED *)
    | D_implied            (* #IMPLIED *)
    | D_default of string  (* <value> -- The value is already expanded *)
    | D_fixed of string    (* FIXED <value> -- The value is already expanded *)
  

  type att_value = Pxp_type_anchor.att_value =
      Value of string
    | Valuelist of string list
    | Implied_value
	(* <ID:type-att-value>
	 * <TYPE:type>
	 * <CALL>   [att_value]
	 * <SIG>    AUTO
	 * <DESCR>  Enumerates the possible values of an attribute:
	 *   - [Value s]: The attribute is declared as a non-list type, or the
	 *     attribute is undeclared; and the attribute is either defined with
	 *     value ["s"], or it is missing but has the default value [s].
	 *   - [[Valuelist [s1;...;sk]]]: The attribute is declared as a list type,
	 *     and the attribute is either defined with value ["s1 ... sk"],
	 *     or it is missing but has the default value ["s1 ... sk"]. The
	 *     components of the list must be separated by whitespace.
	 *   - [Implied_value]: The attribute is declared without default value,
	 *     and there is no definition for the attribute.
	 *     --
	 * </ID>
	 *)


  class type collect_warnings =
  object
    method warn : string -> unit
  end


  class drop_warnings : collect_warnings


  type encoding = Netconversion.encoding
    (* We accept all encodings for character sets which are defined in
     * Netconversion (package netstring).
     *)

  type rep_encoding =
    (* The subset of 'encoding' that may be used for the internal representation
     * of strings.
     * Note: The following encodings are ASCII-compatible! This is an important
     * property used throughout the whole PXP code.
     *)
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


  exception Validation_error of string
    (* Violation of a validity constraint *)

  exception WF_error of string
    (* Violation of a well-formedness constraint *)

  exception Namespace_error of string
    (* Violation of a namespace constraint *)

  exception Error of string
    (* Other error *)

  exception Character_not_supported

  exception At of (string * exn)
    (* The string is a description where the exn happened. The exn value can
     * again be At(_,_) (for example, when an entity within an entity causes
     * the error).
     *)

  exception Undeclared
    (* Indicates that no declaration is available and because of this every kind
     * of usage is allowed. (Raised by some DTD methods.)
     *)

  exception Method_not_applicable of string
    (* Indicates that a method has been called that is not applicable for
     * the class. The argument is the name of the method.
     * (New in PXP 1.1)
     *)

  exception Namespace_method_not_applicable of string
    (* Indicates that the called method is a namespace method but that the
     * object does not support namespaces. The argument is the name of the method.
     * (New in PXP 1.1)
     *)

  val string_of_exn : exn -> string
	(* Converts a PXP exception into a readable string *)


  type output_stream =
      [ `Out_buffer of Buffer.t
      | `Out_channel of out_channel
      | `Out_function of (string -> int -> int -> unit)
      ]

  val write : output_stream -> string -> int -> int -> unit
	(* write os s pos len: Writes the string to the buffer/channel/stream *)


  type pool = Pxp_type_anchor.pool

  val make_probabilistic_pool : ?fraction:float -> int -> pool
	(* A probalistic string pool tries to map strings to pool strings in order
	 * to make it more likely that equal strings are stored in the same memory
	 * block.
	 * The int argument is the size of the pool; this is the number of entries
	 * of the pool. However, not all entries of the pool are used; the ~fraction
	 * argument (default: 0.3) determines the fraction of the actually used
	 * entries. The higher the fraction is, the more strings can be managed
	 * at the same time; the lower the fraction is, the more likely it is that
	 * a new string can be added to the pool.
	 *)

  val pool_string : pool -> string -> string
	(* Tries to find the passed string in the pool; if the string is in the
	 * pool, the pool string is returned. Otherwise, the function tries to
	 * add the passed string to the pool, and the passed string is returned.
	 *)

end (* of CORE_TYPES *)

(* ======================================================================
 * History:
 * 
 * $Log: pxp_core_types_type.mli,v $
 * Revision 1.2  2003/06/15 18:19:56  gerd
 * 	Pxp_yacc has been split up
 *
 * Revision 1.1  2003/06/15 12:22:41  gerd
 * 	Initial revision
 *
 * 
 *)
