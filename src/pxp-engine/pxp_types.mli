(* $Id: pxp_types.mli,v 1.9 2000/09/09 16:38:47 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)


type ext_id =
    System of string
  | Public of (string * string)
  | Anonymous

  (* external identifiers are either "system identifiers" (filenames or URLs),
   * or "public identifiers" Public(id,sysid) where "id" is the representation
   * of the public ID, and "sysid" a fallback system ID, or the empty string.
   *
   * New in PXP: Sometimes the external ID is not known. This case can be
   * referred to as Anonymous ID.
   *
   * Encoding: The identifiers are _always_ encoded as UTF8 strings,
   * regardless of whether another encoding is configured for the parser.
   * TODO: umsetzen
   *)


type dtd_id =
    External of ext_id       (* DTD is completely external *)
  | Derived of ext_id        (* DTD is derived from an external DTD *)
  | Internal                 (* DTD is completely internal *)
;;

type content_model_type =
    Unspecified              (* A specification of the model has not yet been
			      * found
			      *)
  | Empty                    (* Nothing is allowed as content *)
  | Any                      (* Everything is allowed as content *)
  | Mixed of mixed_spec list (* The contents consist of elements and PCDATA 
			      * in arbitrary order. What is allowed in
			      * particular is given as mixed_spec.
			      *)
  | Regexp of regexp_spec    (* The contents are elements following this regular
			      * expression
			      *)

and mixed_spec =
    MPCDATA                  (* PCDATA children are allowed *)
  | MChild of string         (* This kind of Element is allowed *)

and regexp_spec =
    Optional of regexp_spec  (* subexpression? *)
  | Repeated of regexp_spec  (* subexpression* *)
  | Repeated1 of regexp_spec (* subexpression+ *)
  | Alt of regexp_spec list  (* subexpr1 | subexpr2 | ... | subexprN *)
  | Seq of regexp_spec list  (* subexpr1 , subexpr2 , ... , subexprN *)
  | Child of string          (* This kind of Element is allowed here *)
;;


type att_type =
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
;;


type att_default =
    D_required           (* #REQUIRED *)
  | D_implied            (* #IMPLIED *)
  | D_default of string  (* <value> -- The value is already expanded *)
  | D_fixed of string    (* FIXED <value> -- The value is already expanded *)
;;


type att_value =
    Value of string           (* a single value *)
  | Valuelist of string list  (* a list of values *)
  | Implied_value             (* a value left out *)
;;


class type collect_warnings =
  object 
    method warn : string -> unit
  end
;;


class drop_warnings : collect_warnings;;


type encoding = Netconversion.encoding;;
  (* We accept all encodings for character sets which are defined in
   * Netconversion (package netstring).
   *)

type rep_encoding =
  (* The subset of 'encoding' that may be used for internal representation
   * of strings.
   * Note: The following encodings are ASCII-compatible! This is an important
   * property used throghout the whole PXP code.
   *)
  [ `Enc_utf8       (* UTF-8 *)
  | `Enc_iso88591   (* ISO-8859-1 *)
  ]
;;


exception Validation_error of string
  (* Violation of a validity constraint *)

exception WF_error of string
  (* Violation of a well-formedness constraint *)

exception Error of string
  (* Other error *)

exception Character_not_supported

exception At of (string * exn)
  (* The string is a description where the exn happened. The exn value can
   * again be At(_,_) (for example, when an entity within an entity causes
   * the error).
   *)

exception Undeclared
  (* Indicates that declaration is available and because of this every kind
   * of usage is allowed.
   *)

val string_of_exn : exn -> string
  (* Converts a Markup exception into a readable string *)


type output_stream =
    Out_buffer of Buffer.t
  | Out_channel of out_channel
  | Out_function of (string -> int -> int -> unit)

val write : output_stream -> string -> int -> int -> unit
  (* write os s pos len: Writes the string to the buffer/channel/stream *)


type pool 

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

(* ======================================================================
 * History:
 * 
 * $Log: pxp_types.mli,v $
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
