(* $Id: pxp_type_anchor.ml,v 1.1 2003/06/15 18:18:34 gerd Exp $
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
 
type resolver_id =
    { rid_private: private_id option;
      rid_public:  string option;
      rid_system:  string option;
      rid_system_base: string option;
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

(* ======================================================================
 * History:
 * 
 * $Log: pxp_type_anchor.ml,v $
 * Revision 1.1  2003/06/15 18:18:34  gerd
 * 	Initial revision
 *
 * 
 *)
