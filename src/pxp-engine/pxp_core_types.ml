(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

module A = struct

  module StringMap = Map.Make(String);;

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
    match (os:output_stream) with
	`Out_buffer b -> Buffer.add_substring b str pos len
      | `Out_channel ch -> output ch (Bytes.unsafe_of_string str) pos len
           (* FIXME: use output_substring instead *)
      | `Out_function f -> f str pos len
      | `Out_netchannel ch -> ch # really_output (Bytes.unsafe_of_string str) pos len
          (* FIXME: use really_output_string instead *)
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

end


module type S = sig
  module StringMap : Map.S with type key = string

  type ext_id = A.ext_id =
    | System of string
    | Public of (string * string)
    | Anonymous
    | Private of private_id

  and private_id = A.private_id

  val allocate_private_id : unit -> private_id

  type resolver_id = A.resolver_id =
      { rid_private: private_id option;
	rid_public:  string option;
	rid_system:  string option;
	rid_system_base: string option;
      }

  val resolver_id_of_ext_id : ext_id -> resolver_id

  type dtd_id = A.dtd_id =
    | External of ext_id
    | Derived of ext_id
    | Internal

  type content_model_type = A.content_model_type =
    | Unspecified
    | Empty
    | Any
    | Mixed of mixed_spec list
    | Regexp of regexp_spec

  and mixed_spec = A.mixed_spec =
      MPCDATA
    | MChild of string

  and regexp_spec = A.regexp_spec =
      Optional of regexp_spec
    | Repeated of regexp_spec
    | Repeated1 of regexp_spec
    | Alt of regexp_spec list
    | Seq of regexp_spec list
    | Child of string

  type att_type = A.att_type =
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

  type att_default = A.att_default =
      D_required
    | D_implied
    | D_default of string
    | D_fixed of string

  type att_value = A.att_value =
      Value of string
    | Valuelist of string list
    | Implied_value
      
  class type collect_warnings =
  object
    method warn : string -> unit
  end

  class drop_warnings : collect_warnings

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

  val string_of_warning : warning -> string

  val warn : symbolic_warnings option -> collect_warnings -> warning -> unit

  type encoding = Netconversion.encoding

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
    
  val string_of_exn : exn -> string
    
  type output_stream =
      [ `Out_buffer of Buffer.t
      | `Out_channel of out_channel
      | `Out_function of (string -> int -> int -> unit)
      | `Out_netchannel of Netchannels.out_obj_channel
      ]

  val write : output_stream -> string -> int -> int -> unit
  
  type pool = A.pool

  val make_probabilistic_pool : ?fraction:float -> int -> pool

  val pool_string : pool -> string -> string
end


module I = A
  (* life can be so easy *)
