(* $Id: pxp_types.ml,v 1.1 2000/05/29 23:48:38 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright 1999 by Gerd Stolpmann. See LICENSE for details.
 *)

type ext_id =
    System of string
  | Public of (string * string)


type dtd_id =
    External of ext_id
  | Derived of ext_id
  | Internal
;;

type content_model_type =
    Unspecified
  | Empty
  | Any
  | Mixed of mixed_spec list
  | Regexp of regexp_spec

and mixed_spec =
    MPCDATA
  | MChild of string

and regexp_spec =
    Optional of regexp_spec
  | Repeated of regexp_spec
  | Repeated1 of regexp_spec
  | Alt of regexp_spec list
  | Seq of regexp_spec list
  | Child of string
;;


type att_type =
    A_cdata
  | A_id
  | A_idref
  | A_idrefs
  | A_entity
  | A_entities
  | A_nmtoken
  | A_nmtokens
  | A_notation of string list
  | A_enum of string list
;;


type att_default =
    D_required
  | D_implied
  | D_default of string  (* The default value is already expanded *)
  | D_fixed of string    (* The default value is already expanded *)
;;


type att_value =
    Value of string
  | Valuelist of string list
  | Implied_value
;;


class collect_warnings =
  object 
    val mutable wlist = []
    method warn w =
      wlist <- ("WARNING: " ^ w) :: wlist
    method print_warnings =
      String.concat "\n" (List.rev wlist)
    method reset =
      wlist <- []
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
  [  `Enc_utf8       (* UTF-8 *)
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

exception Undeclared


let rec string_of_exn x0 =
  match x0 with
      At (s, x) ->
        s ^ string_of_exn x
    | Illegal_character i ->
        "ERROR: Illegal character"
    | Validation_error s ->
        "ERROR (Validity constraint): "  ^ s
    | WF_error s ->
        "ERROR (Well-formedness constraint): " ^ s
    | Character_not_supported ->
        "RESTRICTION: Character not supported"
    | Bad_character_stream ->
        "ERROR: Bad character stream"
    | Undeclared ->
        "INFORMATION: Undeclared"
    | _ ->
        "Other exception: " ^ Printexc.to_string x0
;;


type output_stream =
    Out_buffer of Buffer.t
  | Out_channel of out_channel
  | Out_function of (string -> int -> int -> unit)
;;


let write os str pos len =
  match os with
      Out_buffer b -> Buffer.add_substring b str pos len
    | Out_channel ch -> output ch str pos len
    | Out_function f -> f str pos len
;;

(* ======================================================================
 * History:
 *
 * $Log: pxp_types.ml,v $
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
 * Old logs from markup_types.ml:
 *
 * Revision 1.7  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.6  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.5  2000/05/01 20:43:19  gerd
 * 	New type output_stream; new function 'write'.
 *
 * Revision 1.4  1999/09/01 16:25:35  gerd
 * 	Dropped Illegal_token and Content_not_allowed_here. WF_error can
 * be used instead.
 *
 * Revision 1.3  1999/08/15 02:22:33  gerd
 * 	Added exception Undeclared.
 *
 * Revision 1.2  1999/08/14 22:14:58  gerd
 * 	New class "collect_warnings".
 *
 * Revision 1.1  1999/08/10 00:35:52  gerd
 * 	Initial revision.
 *
 *
 *)