(* $Id: pxp_types.mli,v 1.1 2000/05/29 23:48:38 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)


type ext_id =
    System of string
  | Public of (string * string)

  (* external identifiers are either "system identifiers" (filenames or URLs),
   * or "public identifiers" Public(id,sysid) where "id" is the representation
   * of the public ID, and "sysid" a fallback system ID, or the empty string.
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


class collect_warnings :
  object 
    method warn : string -> unit
    method print_warnings : string
    method reset : unit
  end
;;


type encoding =
  [  `Enc_utf8       (* UTF-8 *)
  |  `Enc_utf16      (* UTF-16 with unspecified endianess (restricted usage) *)
  |  `Enc_utf16_le   (* UTF-16 little endian *)
  |  `Enc_utf16_be   (* UTF-16 big endian *)
  |  `Enc_iso88591   (* ISO-8859-1 *)
  ]
;;


type rep_encoding =
  (* The subset of 'encoding' that may be used for internal representation
   * of strings.
   *)
  [ `Enc_utf8       (* UTF-8 *)
  |  `Enc_iso88591   (* ISO-8859-1 *)
  ]
;;


exception Illegal_character of int
  (* at this position relative to the current lexeme *)

exception Validation_error of string

exception WF_error of string

exception Character_not_supported

exception Bad_character_stream
  (* Cannot decode character stream *)

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

(* ======================================================================
 * History:
 * 
 * $Log: pxp_types.mli,v $
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
