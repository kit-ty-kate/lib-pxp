(* $Id: pxp_types.ml,v 1.13 2001/06/27 23:33:53 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

class private_id = object end;;

type ext_id =
    System of string
  | Public of (string * string)
  | Anonymous
  | Private of private_id


let allocate_private_id () = new private_id;;
  (* private_id is a class because allocate_private_id becomes thread-safe in
   * this case (without needing to lock a global variable).
   * Note that when you compare objects (=, < etc) you actually compare
   * the object IDs. Hence, private_id has the intended semantics.
   *)


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


type encoding = Netconversion.encoding;;

type rep_encoding =
  (* The subset of 'encoding' that may be used for internal representation
   * of strings.
   *)
  [  `Enc_utf8       (* UTF-8 *)
  |  `Enc_iso88591   (* ISO-8859-1 *)
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
    | Parsing.Parse_error ->
	"SYNTAX ERROR"
    | _ ->
        "Other exception: " ^ Printexc.to_string x0
;;


type output_stream =
  [ `Out_buffer of Buffer.t
  | `Out_channel of out_channel
  | `Out_function of (string -> int -> int -> unit)
  ]
;;


let write os str pos len =
  match os with
      `Out_buffer b -> Buffer.add_substring b str pos len
    | `Out_channel ch -> output ch str pos len
    | `Out_function f -> f str pos len
;;


type pool =
    { mutable pool_start : pool_node;
      mutable pool_end   : pool_node;
      mutable pool_array : pool_node_record array;
      mutable pool_count : int;
      mutable pool_max   : int;
    }
and pool_node =
    No_node
  | Pool_node of pool_node_record
and pool_node_record =
    { mutable previous_pool_node : pool_node;
      mutable next_pool_node : pool_node;
      mutable pool_string : string option;
    }
;;


let make_probabilistic_pool ?(fraction = 0.3) size =
  let make_node _ =
    { previous_pool_node = No_node;
      next_pool_node = No_node;
      pool_string = None;
    }
  in
  let m = truncate (fraction *. float size) in
  if size < 0 || m < 1 || m > size then
    invalid_arg "make_probabilistic_pool";
  { pool_start = No_node;
    pool_end = No_node;
    pool_array = Array.init size make_node;
    pool_count = 0;
    pool_max = m;
  }
;;


let pool_string p s =
  let size = Array.length p.pool_array in
  let h = Hashtbl.hash s mod size in
  let n =  p.pool_array.(h) in
  match n.pool_string with
      None ->
	(* The pool node is free. We occupy the node, and insert it at the
	 * beginning of the node list. 
	 *)
	n.pool_string <- Some s;
	n.previous_pool_node <- No_node;
	n.next_pool_node <- p.pool_start;
	begin match p.pool_start with
	    No_node -> ()
	  | Pool_node start ->
	      start.previous_pool_node <- Pool_node n
	end;
	p.pool_start <- Pool_node n;
	if p.pool_end = No_node then p.pool_end <- Pool_node n;
	p.pool_count <- p.pool_count + 1;
	(* If the node list is longer than pool_max, the last node of the
	 * list is deleted.
	 *)
	if p.pool_count > p.pool_max then begin
	  (* ==> p.pool_count >= 2 *)
	  let last = 
	    match p.pool_end with
		No_node -> assert false
	      | Pool_node x -> x in
	  let last' =
	    match last.previous_pool_node with
		No_node -> assert false
	      | Pool_node x -> x in
	  last'.next_pool_node <- No_node;
	  p.pool_end <- Pool_node last';
	  p.pool_count <- p.pool_count - 1;
	  last.pool_string <- None;
	end;
	s

    | Some s' ->
	if s = s' then begin
	  (* We have found the pool string. To make it more unlikely that n
	   * is removed from the pool, n is moved to the beginning of the
	   * node list.
	   *)
	  begin match n.previous_pool_node with
	      No_node ->
		(* n is already at the beginning *)
		()
	    | Pool_node n_pred ->
		n_pred.next_pool_node <- n.next_pool_node;
		begin match n.next_pool_node with
		    No_node ->
		      (* n is at the end of the list *)
		      p.pool_end <- Pool_node n_pred;
		  | Pool_node n_succ ->
		      n_succ.previous_pool_node <- Pool_node n_pred
		end;
		(* Now n is deleted from the list. Insert n again at the
		 * beginning of the list.
		 *)
		n.previous_pool_node <- No_node;
		n.next_pool_node <- p.pool_start;
		begin match p.pool_start with
		    No_node -> ()
		  | Pool_node start ->
		      start.previous_pool_node <- Pool_node n
		end;
		p.pool_start <- Pool_node n;
	  end;
	  (* Return the found pool string: *)
	  s'             
	end
	else begin
	  (* n contains a different string which happened to have the same
	   * hash index.
	   *)
	  s
	end
;;


(* ======================================================================
 * History:
 *
 * $Log: pxp_types.ml,v $
 * Revision 1.13  2001/06/27 23:33:53  gerd
 * 	Type output_stream is now a polymorphic variant
 *
 * Revision 1.12  2001/06/07 22:49:51  gerd
 * 	New namespace exceptions.
 *
 * Revision 1.11  2001/04/26 23:57:04  gerd
 * 	New exception Method_not_applicable. It is raised if there are
 * classes A and B both conforming to class type C, but A does not implement
 * a method required by the class type. In this case, invoking the method
 * in A raises Method_not_applicable.
 * 	This feature is mainly used in Pxp_document.
 *
 * Revision 1.10  2001/04/22 14:14:41  gerd
 * 	Updated to support private IDs.
 *
 * Revision 1.9  2000/09/17 00:12:19  gerd
 * 	Bugfix in the pool implementation.
 *
 * Revision 1.8  2000/09/09 16:38:47  gerd
 * 	New type 'pool'.
 *
 * Revision 1.7  2000/08/14 22:24:55  gerd
 * 	Moved the module Pxp_encoding to the netstring package under
 * the new name Netconversion.
 *
 * Revision 1.6  2000/07/27 00:41:15  gerd
 * 	new 8 bit codes
 *
 * Revision 1.5  2000/07/16 18:31:09  gerd
 * 	The exception Illegal_character has been dropped.
 *
 * Revision 1.4  2000/07/14 21:25:27  gerd
 * 	Simplified the type 'collect_warnings'.
 *
 * Revision 1.3  2000/07/08 16:23:50  gerd
 * 	Added the exception 'Error'.
 *
 * Revision 1.2  2000/07/04 22:14:05  gerd
 * 	Implemented the changes of rev. 1.2 of pxp_types.mli.
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
