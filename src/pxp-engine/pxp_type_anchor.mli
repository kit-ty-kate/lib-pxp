(* $Id: pxp_type_anchor.mli,v 1.1 2003/06/15 18:18:34 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


(* INTERNAL PXP INTERFACE!
 *
 * The intention of this module is to abbreviate the type definitions.
 * All types and functions of this module are also exported by Pxp_types,
 * so please use only the latter one in your sources, because this module
 * may vanish at any time.
 *)

type ext_id =
    System of string
  | Public of (string * string)
  | Anonymous
  | Private of private_id
      
and private_id

val allocate_private_id : unit -> private_id

type resolver_id =
    { rid_private: private_id option;
      rid_public:  string option;
      rid_system:  string option;
      rid_system_base: string option;  (* when rid_system is relative *)
    }

type dtd_id =
    External of ext_id
  | Derived of ext_id
  | Internal

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

type att_default =
    D_required
  | D_implied
  | D_default of string  (* The default value is already expanded *)
  | D_fixed of string    (* The default value is already expanded *)

type att_value =
    Value of string
  | Valuelist of string list
  | Implied_value

type pool


val make_probabilistic_pool : ?fraction:float -> int -> pool

val pool_string : pool -> string -> string

(* ======================================================================
 * History:
 * 
 * $Log: pxp_type_anchor.mli,v $
 * Revision 1.1  2003/06/15 18:18:34  gerd
 * 	Initial revision
 *
 * 
 *)
