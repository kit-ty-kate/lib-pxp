(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

module StringMap = Map.Make(String);;

type private_id = Pxp_type_anchor.private_id

type ext_id = Pxp_type_anchor.ext_id =
    System of string
  | Public of (string * string)
  | Anonymous
  | Private of private_id


let allocate_private_id = Pxp_type_anchor.allocate_private_id;;

type resolver_id = Pxp_type_anchor.resolver_id =
    { rid_private: private_id option;
      rid_public:  string option;
      rid_system:  string option;
      rid_system_base: string option;
    }


let anonymous =
  { rid_private = None;
    rid_public = None;
    rid_system = None;
    rid_system_base = None;
  }

let resolver_id_of_ext_id =
  function
      System sys_id -> 
	{ anonymous with rid_system = Some sys_id }
    | Public(pub_id, sys_id) ->
	{ anonymous with rid_public = Some pub_id; rid_system = Some sys_id }
    | Private p ->
	{ anonymous with rid_private = Some p }
    | Anonymous ->
	anonymous
;;


type dtd_id = Pxp_type_anchor.dtd_id =
    External of ext_id
  | Derived of ext_id
  | Internal
;;

type content_model_type = Pxp_type_anchor.content_model_type =
    Unspecified
  | Empty
  | Any
  | Mixed of mixed_spec list
  | Regexp of regexp_spec

and mixed_spec = Pxp_type_anchor.mixed_spec =
    MPCDATA
  | MChild of string

and regexp_spec = Pxp_type_anchor.regexp_spec =
    Optional of regexp_spec
  | Repeated of regexp_spec
  | Repeated1 of regexp_spec
  | Alt of regexp_spec list
  | Seq of regexp_spec list
  | Child of string
;;


type att_type = Pxp_type_anchor.att_type =
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


type att_default = Pxp_type_anchor.att_default =
    D_required
  | D_implied
  | D_default of string  (* The default value is already expanded *)
  | D_fixed of string    (* The default value is already expanded *)
;;


type att_value = Pxp_type_anchor.att_value =
    Value of string
  | Valuelist of string list
  | Implied_value
;;


class type collect_warnings =
  object 
    method warn : string -> unit
  end
;;


class drop_warnings =
  object 
    method warn (w:string) = ()
  end
;;

type warning =
    [ `W_code_point_cannot_be_represented of int
    | `W_name_is_reserved_for_extensions of string
    | `W_multiple_ATTLIST_declarations of string
    | `W_multiple_attribute_declarations of string * string
    | `W_element_mentioned_but_not_declared of string
    | `W_entity_declared_twice of string
    | `W_XML_version_not_supported of string
    ]


class type symbolic_warnings =
object
  method warn : warning -> unit
end
	  

let string_of_warning : warning -> string =
  function
      `W_code_point_cannot_be_represented n ->
	"Code point cannot be represented: " ^ string_of_int n
    | `W_name_is_reserved_for_extensions name ->
	"Name is reserved for future extensions: " ^ name
    | `W_multiple_ATTLIST_declarations name ->
	"More than one ATTLIST declaration for element type `" ^ name ^ "'"
    | `W_multiple_attribute_declarations(elname,attname) ->
	"More than one declaration for attribute `" ^ attname ^ 
	"' of element type `" ^ elname ^ "'"
    | `W_element_mentioned_but_not_declared name ->
	"Element type `" ^ name ^ "' mentioned but not declared"
    | `W_entity_declared_twice name ->
	"Entity `" ^ name ^ "' declared twice"
    | `W_XML_version_not_supported v ->
	"XML version '" ^ v ^ "' not supported"
;;


let warn swarner warner warning =
  ( match swarner with
	Some sw -> sw # warn warning
      | None -> ()
  );
  let msg = string_of_warning warning in
  warner # warn msg
;;


type encoding = Netconversion.encoding;;

type rep_encoding =
  (* The subset of 'encoding' that may be used for internal representation
   * of strings.
   *)
  [  `Enc_utf8       (* UTF-8 *)
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
;;


exception Validation_error of string

exception WF_error of string

exception Namespace_error of string

exception Error of string

exception Character_not_supported

exception At of (string * exn)

exception Undeclared

exception Method_not_applicable of string

exception Namespace_method_not_applicable of string

exception Not_competent

exception Not_resolvable of exn

exception Namespace_not_managed of string

exception Namespace_prefix_not_managed of string

exception Namespace_not_in_scope of string


let rec string_of_exn x0 =
  match x0 with
      At (s, x) ->
        s ^ string_of_exn x
    | Validation_error s ->
        "ERROR (Validity constraint): "  ^ s
    | WF_error s ->
        "ERROR (Well-formedness constraint): " ^ s
    | Error s ->
	"ERROR: " ^ s
    | Character_not_supported ->
        "RESTRICTION: Character not supported"
    | Netconversion.Malformed_code ->
        "ERROR: Bad character stream"
    | Undeclared ->
        "INFORMATION: Undeclared"
    | Method_not_applicable mname ->
	"INTERNAL ERROR (method `" ^ mname ^ "' not applicable)"
    | Namespace_method_not_applicable mname ->
	"INTERNAL ERROR (namespace method `" ^ mname ^ "' not applicable)"
    | Parsing.Parse_error ->
	"SYNTAX ERROR"
    | Not_competent ->
	"NO COMPETENT RESOLVER FOUND"
    | Not_resolvable x ->
	"NOT RESOLVABLE: " ^ string_of_exn x
    | Namespace_not_managed uri ->
	"ERROR: NAMESPACE NOT MANAGED: " ^ uri
    | Namespace_prefix_not_managed p ->
	"ERROR: NAMESPACE PREFIX NOT MANAGED: " ^ p
    | Namespace_not_in_scope uri ->
	"ERROR: NAMESPACE NOT IN SCOPE: " ^ uri
    | _ ->
        "Other exception: " ^ Printexc.to_string x0
;;


type output_stream =
  [ `Out_buffer of Buffer.t
  | `Out_channel of out_channel
  | `Out_function of (string -> int -> int -> unit)
  | `Out_netchannel of Netchannels.out_obj_channel
  ]
;;


let write os str pos len =
  match os with
      `Out_buffer b -> Buffer.add_substring b str pos len
    | `Out_channel ch -> output ch str pos len
    | `Out_function f -> f str pos len
    | `Out_netchannel ch -> ch # really_output str pos len
;;


type pool = Pxp_type_anchor.pool

let make_probabilistic_pool = Pxp_type_anchor.make_probabilistic_pool

let pool_string = Pxp_type_anchor.pool_string



