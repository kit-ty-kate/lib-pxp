(* $Id: pxp_document.ml,v 1.23 2001/06/09 22:33:14 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* TODO: Fehler in internal_adopt? *)

open Pxp_types
open Pxp_lexer_types
open Pxp_dtd
open Pxp_aux
open Pxp_dfa


let method_na s =
  raise (Method_not_applicable s)
;;


let nsmethod_na s =
  (* Use this only if the method is not available AND if the method is a
   * namespace method AND if the class is not namespace-aware
   *)
  raise (Namespace_method_not_applicable s)
;;


exception Skip

type node_type =
    T_element of string
  | T_data
  | T_super_root
  | T_pinstr of string
  | T_comment
  | T_none
  | T_attribute of string
  | T_namespace of string
;;


type data_node_classification =
    CD_normal
  | CD_other
  | CD_empty
  | CD_ignorable
  | CD_error of exn
;;


class type ['node] extension =
  object ('self)
    method clone : 'self
    method node : 'node
    method set_node : 'node -> unit
  end
;;


class type [ 'ext ] node =
  object ('self)
    constraint 'ext = 'ext node #extension
    method extension : 'ext
    method delete : unit
    method delete_nodes : ?pos:int -> ?len:int -> unit -> unit
    method parent : 'ext node
    method root : 'ext node
    method orphaned_clone : 'self
    method orphaned_flat_clone : 'self
    method classify_data_node : 'ext node -> data_node_classification
    method append_node : 'ext node -> unit
    method insert_nodes : ?pos:int -> 'ext node list -> unit
    method add_node : ?force:bool -> 'ext node -> unit
    method add_pinstr : proc_instruction -> unit
    method pinstr : string -> proc_instruction list
    method pinstr_names : string list
    method node_position : int
    method node_path : int list
    method sub_nodes : 'ext node list
    method iter_nodes : ('ext node -> unit) -> unit
    method iter_nodes_sibl :
      ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
    method nth_node : int -> 'ext node
    method previous_node : 'ext node
    method next_node : 'ext node
    method set_nodes : 'ext node list -> unit
    method data : string
    method set_data : string -> unit
    method node_type : node_type
    method position : (string * int * int)
    method attribute : string -> att_value
    method attribute_names : string list
    method attribute_type : string -> att_type
    method attributes : (string * Pxp_types.att_value) list
    method required_string_attribute : string -> string
    method required_list_attribute : string -> string list
    method optional_string_attribute : string -> string option
    method optional_list_attribute : string -> string list
    method id_attribute_name : string
    method id_attribute_value : string
    method idref_attribute_names : string list
    method quick_set_attributes : (string * Pxp_types.att_value) list -> unit
    method set_attributes : (string * Pxp_types.att_value) list -> unit
    method attributes_as_nodes : 'ext node list
    method set_comment : string option -> unit
    method comment : string option
    method normprefix : string
    method localname : string
    method namespace_uri : string
    method namespace_info : 'ext namespace_info
    method set_namespace_info : 'ext namespace_info option -> unit
    method namespace_manager : namespace_manager
    method dtd : dtd
    method encoding : rep_encoding
    method create_element :
                   ?name_pool_for_attribute_values:pool ->
                   ?position:(string * int * int) ->
		   ?valcheck:bool ->
		   ?att_values:( (string * att_value) list) ->
                   dtd -> node_type -> (string * string) list -> 'ext node
    method create_data : dtd -> string -> 'ext node
    method local_validate : ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
    method validate_contents : ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
    method complement_attlist : unit -> unit
    method validate_attlist : unit -> unit
    method validate : unit -> unit
    method write : ?prefixes:string list -> output_stream -> encoding -> unit
    method internal_adopt : 'ext node option -> int -> unit
    method internal_set_pos : int -> unit
    method internal_delete : 'ext node -> unit
    method internal_init : (string * int * int) ->
                           pool option -> bool ->
                           dtd -> string -> (string * string) list -> 
			   (string * att_value) list -> unit
    method internal_init_other : (string * int * int) ->
                                 dtd -> node_type -> unit
    method dump : Format.formatter -> unit
  end

and ['ext] namespace_info =
  object
    method srcprefix : string
      (* Returns the prefix before it is normalized *)

    method declaration : 'ext node list
      (* Returns the currently active namespace declaration. The list
       * enumerates all namespace objects with
       *   namespace # node_type = T_namespace "srcprefix"
       * meaning that the srcprefix is declared to correspond to the
       * namespace URI
       *   namespace # namespace_uri.
       * This list always declares the prefix "xml". If there is a default
       * namespace, it is declared for the prefix "".
       *)
  end
;;

type 'ext spec_table =
    { mapping : (string, 'ext node) Hashtbl.t;
      data_node : 'ext node;
      default_element : 'ext node;
      super_root_node : 'ext node option;
      pinstr_mapping : (string, 'ext node) Hashtbl.t;
      default_pinstr_node : 'ext node option;
      comment_node : 'ext node option;
    }
;;

type 'ext spec =
  Spec_table of 'ext spec_table
;;


let make_spec_from_mapping
      ?super_root_exemplar
      ?comment_exemplar
      ?default_pinstr_exemplar
      ?pinstr_mapping
      ~data_exemplar ~default_element_exemplar ~element_mapping () =
  Spec_table
    { mapping = element_mapping;
      data_node = data_exemplar;
      default_element = default_element_exemplar;
      super_root_node = super_root_exemplar;
      comment_node = comment_exemplar;
      default_pinstr_node = default_pinstr_exemplar;
      pinstr_mapping =
	(match pinstr_mapping with
	     None -> Hashtbl.create 1
	   | Some m -> m
	)
    }
;;


let make_spec_from_alist
      ?super_root_exemplar
      ?comment_exemplar
      ?default_pinstr_exemplar
      ?(pinstr_alist = [])
      ~data_exemplar ~default_element_exemplar ~element_alist () =
  let m = List.length  pinstr_alist in
  let pinstr_mapping = Hashtbl.create m in
  List.iter
    (fun (name,ex) -> Hashtbl.add pinstr_mapping name ex)
    pinstr_alist;
  let n = List.length  element_alist in
  let element_mapping = Hashtbl.create m in
  List.iter
    (fun (name,ex) -> Hashtbl.add element_mapping name ex)
    element_alist;
  make_spec_from_mapping
    ?super_root_exemplar:      super_root_exemplar
    ?comment_exemplar:         comment_exemplar
    ?default_pinstr_exemplar:  default_pinstr_exemplar
    ~pinstr_mapping:           pinstr_mapping
    ~data_exemplar:            data_exemplar
    ~default_element_exemplar: default_element_exemplar
    ~element_mapping:          element_mapping
    ()
;;

let rec string_mem_assoc (x : string) = function
  | [] -> false
  | (a, b) :: l -> a = x || string_mem_assoc x l
;;

(* Note: string_mem_assoc is faster than List.mem_assoc applied to strings
 * because the generic comparison is avoided.
 *)

let rec simple_check_att l =
  (* If there are several attributes with the same name --> Wf_error *)
  match l with
      [] -> ()
    | (n,att_val) :: l' ->
	if string_mem_assoc n l' then
	  raise (WF_error("Attribute `" ^ n ^
			  "' occurs twice"));
	simple_check_att l'
;;


let rec hashtbl_check_att att_names l =
  match l with
      [] -> ()
    | (n,att_val) :: l' ->
	if Str_hashtbl.mem att_names n then
	  raise (WF_error("Attribute `" ^ n ^
			  "' occurs twice"));
	Str_hashtbl.add att_names n ();
	hashtbl_check_att att_names l'
;;


(**********************************************************************)

exception Found;;

let rec is_empty_node_list cl =
  (* Whether the node list counts as empty or not. *)
  match cl with
      [] -> true
    | n :: cl' ->
	( match n # node_type with
	    | T_element _     -> false
	    | _               -> is_empty_node_list cl' (* ignore other nodes *)
	)
;;


let rec run_regexp cl ml =
  (* Validates regexp content models ml against instances cl. This
   * function works for deterministic and non-determninistic models.
   * The implementation uses backtracking and may sometimes be slow.
   *
   * cl:   the list of children that will have to be matched
   * ml:   the list of regexps that will have to match (to be read as
   *       sequence)
   * returns () meaning that no match has been found, or raises Found.
   *)
  match ml with
      [] ->
	if cl = [] then raise Found;      (* Frequent case *)
	if is_empty_node_list cl then raise Found;  (* General condition *)
    | Seq seq :: ml' ->
	assert (seq <> []);     (* necessary to ensure termination *)
	run_regexp cl (seq @ ml')
    | Alt alts :: ml' ->
	let rec find alts =
	  match alts with
	      [] -> ()
	    | alt :: alts' ->
		run_regexp cl (alt :: ml');
		find alts'
	in
	assert (alts <> []);      (* Alt [] matches nothing *)
	find alts
    | Repeated re :: ml' ->
	let rec norm re =     (* to avoid infinite loops *)
	  match re with
	      Repeated subre  -> norm subre    (* necessary *)
	    | Optional subre  -> norm subre    (* necessary *)
	    | Repeated1 subre -> norm subre    (* an optimization *)
	    | _               -> re
	in
	let re' = norm re in
	run_regexp cl (re' :: Repeated re' :: ml');
	run_regexp cl ml'
    | Repeated1 re :: ml' ->
	run_regexp cl (re :: Repeated re :: ml')
    | Optional re :: ml' ->
	run_regexp cl (re :: ml');
	run_regexp cl ml';
    | Child chld :: ml' ->
	match cl with
	    [] ->
	      ()
	  | sub_el :: cl' ->
	      begin match sub_el # node_type with
		  T_data ->                       (* Ignore data *)
		    run_regexp cl' ml
		      (* Note: It can happen that we find a data node here
		       * if the 'keep_always_whitespace' mode is turned on.
		       *)
		| T_element nt ->
		    if nt = chld then run_regexp cl' ml'
		| _ ->                            (* Ignore this element *)
		    run_regexp cl' ml
	      end
;;


let run_dfa cl dfa =
  (* Validates regexp content models ml against instances cl. This
   * function works ONLY for deterministic models.
   * The implementation executes the automaton.
   *)
  let current_vertex = ref dfa.dfa_start in
  let rec next_step cl =
    match cl with
	el :: cl' ->
	  begin match el # node_type with
	      T_data ->                       (* Ignore data *)
		next_step cl'
		  (* Note: It can happen that we find a data node here
		   * if the 'keep_always_whitespace' mode is turned on.
		   *)
	    | T_element nt ->
		begin try
		  current_vertex := Graph.follow_edge !current_vertex nt;
		  next_step cl'
		with
		    Not_found -> false
		end
	    | _ ->                         (* Ignore this node *)
		next_step cl'
	  end
      | [] ->
	  VertexSet.mem !current_vertex dfa.dfa_stops
  in
  next_step cl
;;


let validate_content ?(use_dfa=None) model (el : 'a node) =
  (* checks that the nodes of 'el' matches the DTD. Returns 'true'
   * on success and 'false' on failure.
   *)
  match model with
      Unspecified -> true
    | Any -> true
    | Empty ->
	let cl = el # sub_nodes in
	is_empty_node_list cl
    | Mixed (MPCDATA :: mix) ->
	let mix' = List.map (function
				 MPCDATA -> assert false
			       | MChild x -> x)
		            mix in
	begin try
	  el # iter_nodes
	    (fun sub_el ->
	       let nt = sub_el # node_type in
	       match nt with
	       | T_element name ->
		   if not (List.mem name mix') then raise Not_found;
	       | _ -> ()
	    );
	  true
	with
	    Not_found ->
	      false
	end
    | Regexp re ->
	let cl = el # sub_nodes in
	begin match use_dfa with
	    None ->
	      (* General backtracking implementation: *)
	      begin try
		run_regexp cl [re];
		false
	      with
		  Found -> true
	      end
	  | Some dfa ->
	      run_dfa cl dfa
	end

    | _ -> assert false
;;

(**********************************************************************)
(*
 * CLASS HIERARCHY: 
 *
 * node_impl 
 *   |
 *   +- data_impl
 *   +- markup_impl
 *       |
 *       +- element_impl
 *       |   |
 *       |   +- namespace_element_impl
 *       |
 *       +- comment_impl
 *       +- pinstr_impl
 *       +- super_root_impl
 *
 * attribute_impl
 *   |
 *   +- namespace_attribute_impl
 *
 * namespace_impl
 *
 * Note: this is only the class hierarchy, and not the type hierarchy.
 * Actually, there is not a type hierarchy, because all classes have
 * the same type  'ext node
 *)
(**********************************************************************)


class virtual ['ext] node_impl an_ext =
  object (self)
    constraint 'ext = 'ext node #extension

    val mutable parent = (None : 'ext node option)
    val mutable node_position = -1
    val mutable dtd = (None : dtd option)
    val mutable extension = an_ext

    initializer
      extension # set_node (self : 'ext #node  :> 'ext node)


    (************* METHODS WITH A GENERIC IMPLEMENTATION ***************)


    method extension = (extension : 'ext)

    method delete =
      match parent with
	  None -> ()
	| Some p -> p # internal_delete (self : 'ext #node :> 'ext node)

    method parent =
      match parent with
	  None -> raise Not_found
	| Some p -> p

    method root =
      match parent with
	  None -> (self : 'ext #node :> 'ext node)
	| Some p -> p # root

    method node_position =
      if node_position >= 0 then node_position else
	raise Not_found

    method node_path =
      let rec collect n path =
	try
	  let p = n # node_position in
	  collect (n # parent) (p :: path)
	with
	    Not_found ->
	      (* n is the root *)
	      path
      in
      collect (self : 'ext #node :> 'ext node) []

    method previous_node =
      self # parent # nth_node (self # node_position - 1)

    method next_node =
      self # parent # nth_node (self # node_position + 1)

    method orphaned_clone =
      let x = extension # clone in
      let n =
	{< parent = None;
	   node_position = -1;
	   extension = x;
	>} in
      x # set_node (n : 'ext #node  :> 'ext node);
      n

    method orphaned_flat_clone =
      let x = extension # clone in
      let n =
	{< parent = None;
	   node_position = -1;
	   extension = x;
	>} in
      x # set_node (n : 'ext #node  :> 'ext node);
      n

    method dtd =
      match dtd with
	  None -> failwith "Pxp_document.node_impl#dtd: No DTD available"
	| Some d -> d

    method encoding =
      match dtd with
	  None -> failwith "Pxp_document.node_impl#encoding: No DTD available"
	| Some d -> d # encoding

    method internal_adopt (new_parent : 'ext node option) pos =
      begin match parent with
	  None -> ()
	| Some p ->
	    if new_parent <> None then
	      failwith "Pxp_document.node_impl#internal_adopt: Tried to add a bound element"
      end;
      parent <- new_parent;
      node_position <- pos

    method internal_set_pos pos =
      node_position <- pos


    (************** METHODS THAT ARE USUALLY N/A ****************)

    method comment : string option                = method_na "comment"
    method set_comment (c : string option) : unit = method_na "set_comment"
    method normprefix : string                    = nsmethod_na "normprefix"
    method localname : string                     = nsmethod_na "localname"
    method namespace_uri : string                 = nsmethod_na "namespace_uri"
    method namespace_info : 'ext namespace_info   = nsmethod_na "namespace_info"
    method set_namespace_info (info : 'ext namespace_info option) : unit 
            = nsmethod_na "set_namespace_info"
    method namespace_manager                      
            = nsmethod_na "namespace_manager"


    (************* METHODS THAT NEED TO BE DEFINED **************)

    method virtual delete_nodes : ?pos:int -> ?len:int -> unit -> unit
    method virtual classify_data_node : 'ext node -> data_node_classification
    method virtual append_node : 'ext node -> unit
    method virtual insert_nodes : ?pos:int -> 'ext node list -> unit
    method virtual add_node : ?force:bool -> 'ext node -> unit
    method virtual add_pinstr : proc_instruction -> unit
    method virtual sub_nodes : 'ext node list
    method virtual pinstr : string -> proc_instruction list
    method virtual pinstr_names : string list
    method virtual iter_nodes : ('ext node -> unit) -> unit
    method virtual iter_nodes_sibl : ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
    method virtual nth_node : int -> 'ext node
    method virtual set_nodes : 'ext node list -> unit
    method virtual data : string
    method virtual set_data : string -> unit
    method virtual node_type : node_type
    method virtual position : (string * int * int)
    method virtual attribute : string -> att_value
    method virtual attribute_names : string list
    method virtual attribute_type : string -> att_type
    method virtual attributes : (string * Pxp_types.att_value) list
    method virtual required_string_attribute : string -> string
    method virtual required_list_attribute : string -> string list
    method virtual optional_string_attribute : string -> string option
    method virtual optional_list_attribute : string -> string list
    method virtual quick_set_attributes : (string * Pxp_types.att_value) list -> unit
    method virtual set_attributes : (string * Pxp_types.att_value) list -> unit
    method virtual attributes_as_nodes : 'ext node list
    method virtual create_element :
                   ?name_pool_for_attribute_values:pool ->
                   ?position:(string * int * int) ->
		   ?valcheck:bool ->
		   ?att_values:((string * att_value) list) ->
                   dtd -> node_type -> (string * string) list -> 'ext node
    method virtual create_data : dtd -> string -> 'ext node
    method virtual write : ?prefixes:string list -> output_stream -> encoding -> unit
    method virtual local_validate : ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
    method virtual validate_contents : ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
    method virtual internal_delete : 'ext node -> unit
    method virtual internal_init : (string * int * int) ->
                                   pool option -> bool -> 
                                   dtd -> string -> (string * string) list ->
				   (string * Pxp_types.att_value) list ->
                                       unit
    method virtual internal_init_other : (string * int * int) ->
                                         dtd -> node_type -> unit


    method virtual dump : Format.formatter -> unit

  end
;;


(**********************************************************************)

let no_position = ("?", 0, 0) ;;

let format_att_value fmt v =
  match v with
      Implied_value -> Format.pp_print_string fmt "Implied_value"
    | Value s -> Format.pp_print_string fmt ("Value \"" ^ String.escaped s ^ "\"")
    | Valuelist l ->
	Format.pp_print_string fmt "Valuelist [";
	Format.pp_print_string fmt (String.concat "; " 
				      (List.map 
					 (fun s -> 
					    "\"" ^ String.escaped s ^ "\""
					 )
					 l
				      )
				   );
	Format.pp_print_string fmt "]";
;;

let format_pinstr fmt pinstr =
  match pinstr with
      None -> ()
    | Some pl ->
	Hashtbl.iter
	  (fun _ pi ->
	     Format.pp_print_cut fmt ();
	     Format.pp_print_string fmt "+ <?";
	     Format.pp_print_string fmt (pi # target);
	     Format.pp_print_string fmt " ";
	     Format.pp_print_string fmt (pi # value);
	     Format.pp_print_string fmt "?>"
	  )
	  pl
;;


class ['ext] data_impl an_ext : ['ext] node =
  object (self)
    inherit ['ext] node_impl an_ext
    val mutable content = ("" : string)

    method position = no_position

    method delete_nodes ?pos ?len () = method_na "delete_nodes"
    method classify_data_node _ = method_na "classify_data_node"
    method append_node _ = method_na "append_node"
    method insert_nodes ?pos _ = method_na "insert_nodes"
    method add_node ?(force=false) _ = method_na "add_node"
    method add_pinstr _ = method_na "add_pinstr"
    method pinstr _ = []
    method pinstr_names = []
    method sub_nodes = []
    method iter_nodes _ = ()
    method iter_nodes_sibl _ = ()
    method nth_node _ = raise Not_found
    method set_nodes _ = method_na "set_nodes"
    method data = content
    method node_type = T_data
    method attribute _ = raise Not_found
    method attribute_names = []
    method attribute_type _ = raise Not_found
    method attributes = []
    method required_string_attribute _ =
      failwith "Pxp_document.data_impl#required_string_attribute: not found"
    method required_list_attribute _ =
      failwith "Pxp_document.data_impl#required_list_attribute: not found"
    method optional_string_attribute _ = None
    method optional_list_attribute _ = []
    method id_attribute_name = raise Not_found
    method id_attribute_value = raise Not_found
    method idref_attribute_names = []
    method quick_set_attributes _ = method_na "quick_set_attributes"
    method set_attributes _ = method_na "set_attributes"
    method attributes_as_nodes = []

    method create_element ?name_pool_for_attribute_values ?position 
                          ?valcheck ?att_values _ _ _ =
      method_na "create_element"

    method create_data new_dtd new_str =
      let x = extension # clone in
      let n =
      ( {< parent = None;
	   extension = x;
	   dtd = Some new_dtd;
	   content = new_str;
	>}
	: 'ext #node :> 'ext node) in
      x # set_node n;
      n

    method set_data str =
      content <- str

    method local_validate ?use_dfa ?check_data_nodes () = ()
    method validate_contents ?use_dfa ?check_data_nodes () = ()
    method complement_attlist() = ()
    method validate_attlist () = ()
    method validate () = ()


    method write ?(prefixes = ([]: string list)) os enc =
      let encoding = self # encoding in
      write_data_string ~from_enc:encoding ~to_enc:enc os content


    method internal_delete _ =           assert false
    method internal_init _ _ _ _ _ _ _ = assert false
    method internal_init_other _ _ _ =   assert false

    method dump fmt =
      Format.pp_open_hbox fmt ();
      Format.pp_print_string fmt "* T_data \"";
      Format.pp_print_string fmt (String.escaped content);
      Format.pp_print_string fmt "\"";
      Format.pp_close_box fmt ();

  end
;;


(**********************************************************************)

class ['ext] attribute_impl ~element ~name value dtd =
  (object (self)
     val mutable parent = (None : 'ext node option)
     val mutable dtd = dtd
     val mutable node_position = -1
     val mutable element_name = element
     val mutable att_name = name
     val mutable att_value = value

     method parent =
       match parent with
	   None -> raise Not_found
	 | Some p -> p

     method root =
       match parent with
	   None -> (self : 'ext #node :> 'ext node)
	 | Some p -> p # root

    method node_position =
      if node_position >= 0 then node_position else
	raise Not_found

    method node_path =
      let rec collect n path =
	try
	  let p = n # node_position in
	  collect (n # parent) (p :: path)
	with
	    Not_found ->
	      (* n is the root *)
	      path
      in
      try
	collect (self # parent) [ -1; self # node_position ]
      with
	  Not_found -> [ self # node_position ]

     method internal_adopt new_parent pos =
       parent <- new_parent;
       node_position <- pos;

     method orphaned_clone =
       {< parent = None >}

     method orphaned_flat_clone =
       {< parent = None >}

     method dtd = dtd

     method encoding = dtd # encoding

     method node_type = T_attribute att_name

     method attribute n =
       if n = att_name then att_value else raise Not_found

     method attribute_names = [ att_name ]

     method attribute_type n =
       let eltype = dtd # element element_name in
       ( try
	   let atype, adefault = eltype # attribute n in
	   atype
	 with
	     Undeclared ->
	       A_cdata
       )

     method attributes = [ att_name, att_value ]

     method required_string_attribute n =
       if n = att_name then
	 match att_value with
	     Value s -> s
	   | Valuelist l -> String.concat " " l
	   | Implied_value -> raise Not_found
       else
	 failwith "Pxp_document.attribute_impl#required_string_attribute: not found"


     method required_list_attribute n =
       if n = att_name then
	 match att_value with
	     Value s -> [ s ]
	   | Valuelist l -> l
	   | Implied_value -> raise Not_found
       else
	 failwith "Pxp_document.attribute_impl#required_list_attribute: not found"

     method optional_string_attribute n =
       if n = att_name then
	 match att_value with
	     Value s -> Some s
	   | Valuelist l -> Some(String.concat " " l)
	   | Implied_value -> None
       else
	 None

     method optional_list_attribute n =
       if n = att_name then
	 match att_value with
	     Value s -> [ s ]
	   | Valuelist l -> l
	   | Implied_value -> []
       else
	 []

     method data =
       match att_value with
	   Value s -> s
	 | Valuelist l -> String.concat " " l
	 | Implied_value -> raise Not_found

    method dump fmt =
      Format.pp_open_hbox fmt ();
      Format.pp_print_string fmt "+ T_attribute ";
      Format.pp_print_string fmt att_name;
      Format.pp_print_string fmt "=";
      format_att_value fmt att_value;
      Format.pp_close_box fmt ();

    (* Senseless methods: *)

     method sub_nodes = []
     method pinstr _ = []
     method pinstr_names = []
     method iter_nodes _ = ()
     method iter_nodes_sibl _ = ()
     method nth_node _ = raise Not_found
     method position = no_position
     method comment = None
     method local_validate ?use_dfa ?check_data_nodes () = ()
     method validate_contents ?use_dfa ?check_data_nodes () = ()
     method complement_attlist () = ()
     method validate_attlist () = ()
     method validate () = ()

    (* Non-applicable methods: *)

     method extension =          method_na "extension"
     method delete =             method_na "delete"
     method delete_nodes ?pos ?len _ = method_na "delete_nodes"
     method previous_node =      method_na "previous_node"
     method next_node =          method_na "next_node"
     method internal_set_pos _ = method_na "internal_set_pos"
     method internal_delete _ =  method_na "internal_delete"
     method internal_init _ _ _ _ _ _ _ = method_na "internal_init"
     method internal_init_other _ _ _ = method_na "internal_init_other"
     method classify_data_node _      = method_na "classify_data_node"
     method append_node _     =  method_na "append_node"
     method insert_nodes ?pos _       =  method_na "append_node"
     method add_node ?force _ =  method_na "add_node"
     method add_pinstr _ =       method_na "add_pinstr"
     method set_nodes _ =        method_na "set_nodes"
     method quick_set_attributes _ =    method_na "quick_set_attributes"
     method set_attributes _ =   method_na "set_attributes"
     method attributes_as_nodes =       method_na "attributes_as_nodes"
     method set_comment c =      method_na "set_comment"
     method set_data _ =         method_na "set_data"
     method create_element ?name_pool_for_attribute_values ?position 
                           ?valcheck ?att_values _ _ _ =
                                 method_na "create_element"
     method create_data _ _ =    method_na "create_data"
     method write ?prefixes _ _ = method_na "write"
     method id_attribute_name =  method_na "id_attribute_name"
     method id_attribute_value = method_na "id_attribute_value"
     method idref_attribute_names =     method_na "idref_attribute_names"
     method normprefix =         nsmethod_na "normprefix"
     method localname =          nsmethod_na "localname"
     method namespace_uri =      nsmethod_na "namespace_uri"
     method namespace_info =     nsmethod_na "namespace_info"
     method set_namespace_info info =
                                 nsmethod_na "set_namespace_info"
     method namespace_manager =  nsmethod_na "namespace_manager"
   end
     : ['ext] node)
;;

let attribute_name n =
  match n # node_type with
      T_attribute name -> name
    | _ -> invalid_arg "Pxp_document.attribute_name"
;;


let attribute_value n =
  match n # node_type with
      T_attribute name -> n # attribute name
    | _ -> invalid_arg "Pxp_document.attribute_value"
;;


let attribute_string_value n =
  match n # node_type with
      T_attribute name -> n # data
    | _ -> invalid_arg "Pxp_document.attribute_value"
;;



(**********************************************************************)

let flag_ext_decl = 1;;
    (* Whether the element is externally declared *)

(* REMOVED in PXP 1.1: *)
(* let flag_keep_always_whitespace = 2;; *)
    (* Whether the "keep whitespace mode" is on *)


let no_validation =
  { content_model = Any;
    content_dfa = lazy None;
    id_att_name = None;
    idref_att_names = [];
    init_att_vals = [| |];
    att_lookup = Str_hashtbl.create 1;
    att_info = [| |];
    att_required = [];
    accept_undeclared_atts = true;
  }
;;


type 'a list_or_array =
    LA_not_available
  | LA_list of 'a list
  | LA_array of 'a array
;;
(* Perhaps we need also the hybrid LA_list_array storing both representations.
 *)

type 'ext attlist =
    No_atts
  | Att of (string * att_value * 'ext attlist)
  | Att_with_node of (string * att_value * 'ext node * 'ext attlist)
;;
  (* The most compact representation of attribute lists.
   * An attribute list should either consist only of Att cells or of
   * Att_with_node cells, but not contain a mixture of both cell types.
   *)


let rec att_assoc n l =
  match l with
      No_atts -> raise Not_found
    | Att (an,ax,l') ->
	if an = n then ax else att_assoc n l'
    | Att_with_node (an,ax,_,l') ->
	if an = n then ax else att_assoc n l'
;;


let rec att_map f l =
  match l with
      No_atts -> []
    | Att (an,ax,l') ->
	f an ax :: att_map f l'
    | Att_with_node (an,ax,_,l') ->
	f an ax :: att_map f l'
;;


let rec att_iter f l =
  match l with
      No_atts -> ()
    | Att (an,ax,l') ->
	f an ax;
	att_iter f l'
    | Att_with_node (an,ax,_,l') ->
	f an ax;
	att_iter f l'
;;


let rec att_make l =
  match l with
      [] -> No_atts
    | (n,v) :: l' -> Att (n,v, att_make l')
;;


let rec att_map_make f l =
  match l with
      [] -> No_atts
    | (n,v) :: l' ->
	let v' = f v in
	Att (n,v', att_map_make f l')
;;


let att_add_array l x =
  for k = Array.length x - 1 downto 0 do
    let (n,v) = x.( k ) in
    l := Att(n,v,!l);
  done
;;


let att_add_list l x =
  List.iter
    (fun (n,v) ->
       l := Att(n,v,!l);
    )
    x
;;


let rec att_add_nodes f l k =
  match l with
      No_atts      -> No_atts
    | Att (n,v,l') -> Att_with_node(n, v, f n v k, att_add_nodes f l' (k+1))
    | Att_with_node (_,_,_,_) -> assert false
;;


let rec att_return_nodes l =
  match l with
      No_atts      -> []
    | Att (n,v,l') -> assert false
    | Att_with_node (_,_,n,l') -> n :: att_return_nodes l'
;;


let rec att_remove_nodes l =
  match l with
      No_atts      -> No_atts
    | Att (n,v,l') -> Att (n,v, att_remove_nodes l')
    | Att_with_node (n,v,_,l') -> Att(n,v, att_remove_nodes l')
;;


let att_have_nodes l =
  match l with
      Att_with_node (_,_,_,_) -> true
    | _ -> false
;;


let list_split n l =
  (* Returns l1, l2 with l = List.rev l1 @ l2 and length l1 = n *)
  let rec split n l h =
    if n = 0 then
      h, l
    else
      match l with
	  [] -> 
	    failwith "list_split"
	| x :: l' ->
	    split (n-1) l' (x::h)
  in
  split n l []
;;


let only_whitespace error_name s =
  (* Checks that the string "s" contains only whitespace. On failure,
   * Validation_error is raised.
   *)
  if not (Pxp_lib.only_whitespace s) then
    raise(Validation_error(error_name() ^
			   " must not have character contents"));
  ()
;;


class ['ext] markup_impl an_ext (* : ['ext] node *) =
  (* For elements, comments, pis, super root nodes *)
    object (self:'self)
      inherit ['ext] node_impl an_ext as super

      val mutable vr = no_validation
      val mutable flags = 0
          (* bit string of flags; see the values flag_* above *)
      val mutable ntype = T_none
      val mutable rev_nodes = ([] : 'c list)
      val mutable nodes = LA_not_available
      val mutable size = 0
      val mutable attributes = No_atts
      val mutable pinstr = (None : (string,proc_instruction) Hashtbl.t option)

      val mutable position = no_position

      method dump fmt =
	Format.pp_open_vbox fmt 2;
	( match ntype with
	      T_none -> 
		Format.pp_print_string fmt "* T_none";
	    | T_element n -> 
		Format.pp_print_string fmt "* T_element \"";
		Format.pp_print_string fmt n;
		Format.pp_print_string fmt "\"";
	    | T_comment ->
		Format.pp_print_string fmt "* T_comment";
	    | T_pinstr pi ->
		Format.pp_print_string fmt "* T_pinstr \"";
		Format.pp_print_string fmt pi;
		Format.pp_print_string fmt "\"";
	    | T_super_root ->
		Format.pp_print_string fmt "* T_super_root";
	    | _ ->
		assert false
	);
	att_iter
	  (fun n v ->
	     Format.pp_print_cut fmt ();
	     Format.pp_print_string fmt n;
	     Format.pp_print_string fmt "=";
	     format_att_value fmt v;
	  )
	  attributes;
	if att_have_nodes attributes then begin
	  List.iter
	    (fun n ->
	       Format.pp_print_cut fmt ();
	       n # dump fmt;
	    )
	    (att_return_nodes attributes);
	end;
	format_pinstr fmt pinstr;
	List.iter
	  (fun n ->
	     Format.pp_print_cut fmt ();
	     n # dump fmt;
	  )
	  (List.rev rev_nodes);
	Format.pp_close_box fmt (); 

      method private set_flag which value =
	flags <- (flags land (lnot which)) lor
	         (if value then which else 0)

      method comment =
	match ntype with
	    T_comment ->
	      (	match attributes with
		    No_atts -> None
		  | Att (_,Value c,No_atts) -> Some c
		  | _ -> assert false
	      )
	  | _ -> None

      method set_comment c =
	if ntype = T_comment then begin
	  match c with
	      None   -> attributes <- No_atts
	    | Some c -> attributes <- Att ("", Value c, No_atts)
	end
	else
	  method_na "set_comment"

      method attributes =
	att_map (fun an ax -> (an,ax)) attributes

      method position = position

      method private error_name =
	match ntype with
	    T_element n -> "Element `" ^ n ^ "'"
	  | T_super_root -> "Super root"
	  | T_pinstr n -> "Processing instruction node `" ^ n ^
	      "'"
	  | T_comment -> "Comment node"
	  | T_none -> "NO element"
	  | T_attribute _ -> assert false
	  | T_namespace _ -> assert false
	  | T_data -> assert false

      method append_node n =
	(* general DTD check: *)
	begin match dtd with
	    None -> ()
	  | Some d -> if n # dtd != d then
	      failwith "Pxp_document.markup_impl # append_node: the sub node has a different DTD";
	end;
	(* Add the node: *)
	n # internal_adopt (Some (self : 'ext #node :> 'ext node)) size;
	rev_nodes <- n :: rev_nodes;
	nodes <- LA_not_available;
	size <- size + 1


      method classify_data_node n =
	try
	  begin match n # node_type with
	      T_data ->
		begin match vr.content_model with
		    Any         -> CD_normal
		  | Unspecified -> CD_normal
		  | Empty       ->
		      if n # data <> "" then
			CD_error(Validation_error(self # error_name ^
						  " must be empty"))
		      else
			CD_empty
		  | Mixed _     -> CD_normal
		  | Regexp _    ->
		      (* May raise an exception: *)
		      only_whitespace (fun()->self # error_name)
			(n # data);
 	 	      (* TODO: following check faster *)
		      if n # dtd # standalone_declaration &&
		        n # data <> ""
		      then begin
			(* The standalone declaration is violated if the
			 * element declaration is contained in an external
			 * entity.
			 *)
			if flags land flag_ext_decl <> 0 then
			  raise
			    (Validation_error
			       (self # error_name ^
				" violates standalone declaration"  ^
				" because extra white space separates" ^
				" the sub elements"));
		      end;
		      CD_ignorable
		end
	    | _ ->
		(* n is not a data node: *)
		CD_other
	  end;
	with ex ->
	  CD_error ex

      method add_node ?(force = false) n =
	(* compatibility with PXP 1.0: *)
	match self # classify_data_node n with
	    CD_other
	  | CD_normal ->
	      self # append_node n
	  | CD_empty 
	  | CD_ignorable ->
	      ()
	  | CD_error ex ->
	      raise ex

      method insert_nodes ?(pos = size) new_nodes =
	if pos < 0 || pos > size then invalid_arg "insert_nodes: ~pos";
	List.iter
	  (fun n ->
	     try ignore(n # parent);
	         invalid_arg "insert_nodes: new node is not orphan"
	     with Not_found -> ()
	  )
	  new_nodes;
	let succ_nodes, rev_pred_nodes =
	  list_split (size-pos) rev_nodes in
	(* succ_nodes: the nodes at position pos
	 * rev_pred_nodes: the nodes before position pos in reverse order
	 *)
	rev_nodes <- List.rev_append 
	               succ_nodes 
	               (List.rev_append new_nodes rev_pred_nodes);
	let k = ref 0 in
	List.iter
	  (fun n -> 
	     n # internal_adopt 
	       (Some (self : 'ext #node :> 'ext node)) 
	       (pos + !k);
	     (* internal_adopt cannot fail because of the above orphan check *)
	     incr k
	  )
	  new_nodes;
	let number_new_nodes = !k in
	(* renumber succ_nodes: *)
	List.iter
	  (fun n -> 
	     n # internal_set_pos (pos + !k);
	     incr k
	  )
	  succ_nodes;
	nodes <- LA_not_available;
	size <- size + number_new_nodes;
	()

      method delete_nodes ?(pos = 0) ?(len = size) () =
	if pos < 0 || len < 0 || pos + len > size then 
	  invalid_arg "delete_node: ~pos or ~len";
	let succ_nodes, rev_pred_nodes =
	  list_split (size-pos) rev_nodes in
	(* succ_nodes: the nodes at position pos
	 * rev_pred_nodes: the nodes before position pos in reverse order
	 *                 (nodes to keep)
	 *)
	let rev_pred_nodes', succ_nodes' =
	  list_split len succ_nodes in
	(* succ_nodes': the nodes at position pos+len
	 *              (nodes to keep)
	 * rev_pred_nodes': the nodes from position pos to pos+len-1 (rev order)
	 *                  (nodes to delete)
	 *)
	List.iter
	  (fun n -> 
	     n # internal_adopt None (-1)
	     (* internal_adopt cannot fail because of an invariant *)
	  )
	  rev_pred_nodes';
	rev_nodes <- List.rev_append succ_nodes' rev_pred_nodes;
	nodes <- LA_not_available;
	size <- size - len;
	let k = ref pos in
	List.iter
	  (fun n ->
	     n # internal_set_pos !k;
	     incr k
	  )
	  succ_nodes';
	()

      method set_data _ = method_na "set_data"

      method add_pinstr pi =
	begin match dtd with
	    None -> ()
	  | Some d ->
	      if pi # encoding <> d # encoding then
		failwith "Pxp_document.markup_impl # add_pinstr: Inconsistent encodings";
	end;
	let name = pi # target in
	let l =
	  match pinstr with
	      None ->
		let l0 = Hashtbl.create 1 in
		pinstr <- Some l0;
		l0
	    | Some l' -> l' in
	Hashtbl.add l name pi

      method pinstr name =
	match pinstr with
	    None   -> []
	  | Some l -> Hashtbl.find_all l name

      method pinstr_names =
	match pinstr with
	    None   -> []
	  | Some l ->
	      let nl = ref [] in
	      Hashtbl.iter
		(fun n _ -> nl := n :: !nl)
		l;
	      !nl

      method sub_nodes =
	match nodes with
	    LA_not_available ->
	      if rev_nodes = [] then
		[]
	      else begin
		let cl = List.rev rev_nodes in
		nodes <- LA_list cl;
		cl
	      end
	  | LA_list cl ->
	      cl
	  | LA_array a ->
	      Array.to_list a   (* does this lead to performance problems ? *)

      method iter_nodes f =
	match nodes with
	    LA_not_available ->
	      if rev_nodes <> [] then begin
		let cl = List.rev rev_nodes in
		nodes <- LA_list cl;
		List.iter f cl
	      end
	  | LA_list cl ->
	      List.iter f cl
	  | LA_array a ->
	      Array.iter f a

      method iter_nodes_sibl f =
	let rec next last_node l =
	  match l with
	      [] -> ()
	    | [x] ->
		f last_node x None
	    | x :: y :: l' ->
		f last_node x (Some y);
		next (Some x) l'
	in
	match nodes with
	    LA_not_available ->
	      if rev_nodes <> [] then begin
		let cl = List.rev rev_nodes in
		nodes <- LA_list cl;
		next None cl
	      end
	  | LA_list cl ->
	      next None cl
	  | LA_array a ->
	      ( match a with
		    [| |] -> ()
		  | [| a0 |] -> f None a0 None
		  | _ ->
		      let previous = ref None in
		      for i = 0 to Array.length a - 2 do
			let nextnode = Some (a.(i+1)) in
			f !previous a.(i) nextnode;
			previous := Some a.(i);
		      done;
		      f !previous a.(Array.length a - 1) None
	      )

      method nth_node p =
	if p < 0 or p >= size then raise Not_found;
	match nodes with
	    LA_not_available ->
	      if rev_nodes = [] then
		invalid_arg "Array.get"
	      else begin
		let a = Array.of_list (List.rev rev_nodes) in
		nodes <- LA_array a;
		a.(p)
	      end
	  | LA_list l ->
	      let a = Array.of_list l in
	      nodes <- LA_array a;
	      a.(p)
	  | LA_array a ->
	      a.(p)

      method set_nodes nl =
	(* TODO *)
	let old_size = size in
	List.iter
	  (fun n -> n # internal_adopt None (-1))
	  rev_nodes;
	begin try
	  size <- 0;
	  List.iter
	    (fun n -> n # internal_adopt
		            (Some (self : 'ext #node :> 'ext node))
		            size;
	              size <- size + 1)
	    nl
	with
	    e ->
	      (* TODO: questionable impl! *)
	      (* revert action as much as possible *)
	      List.iter
		(fun n -> n # internal_adopt None (-1))
		rev_nodes;
	      size <- old_size;
	      let pos = ref (size-1) in
	      List.iter
		(fun n -> n # internal_adopt
		                (Some (self : 'ext #node :> 'ext node))
		                !pos;
		          decr pos
		)
		rev_nodes;
	      (* [TODO] Note: there may be bad members in nl *)
	      raise e
	end;
	rev_nodes <- List.rev nl;
	nodes <- LA_not_available;


      method orphaned_clone : 'self =
	let sub_clones =
	  List.map
	    (fun m ->
	       m # orphaned_clone)
	    rev_nodes
	in

	let x = extension # clone in
	let n =
	  {< parent = None;
	     node_position = -1;
	     extension = x;
	     rev_nodes = sub_clones;
	     nodes = LA_not_available;
	  >} in

	let pos = ref (size - 1) in
	List.iter
	  (fun m -> m # internal_adopt
	              (Some (n : 'ext #node :> 'ext node))
	              !pos;
	            decr pos
	  )
	  sub_clones;

	x # set_node (n : 'ext #node  :> 'ext node);
	n

      method orphaned_flat_clone : 'self =
	let x = extension # clone in
	let n =
	  {< parent = None;
	     node_position = -1;
	     extension = x;
	     rev_nodes = [];
	     nodes = LA_not_available;
	     size = 0;
	  >} in

	x # set_node (n : 'ext #node  :> 'ext node);
	n


      method internal_delete n =
	rev_nodes <- List.filter (fun n' -> n' != n) rev_nodes;
	size <- size - 1;
	let p = ref (size-1) in
	List.iter
	  (fun n' -> n' # internal_set_pos !p; decr p)
	  rev_nodes;
	nodes <- LA_not_available;
	n # internal_adopt None (-1);


      method data =
	match ntype with
	    T_element _ | T_super_root ->
	      let cl = self # sub_nodes in
	      String.concat
		""
		(List.map
		   (fun n ->
		      match n # node_type with
			  T_element _
			| T_super_root
			| T_data ->
			    n # data
		   	| _ ->
			    ""
		   )
		   cl
		)
	  | T_comment ->
	      ( match self # comment with
		    None   -> raise Not_found
		  | Some s -> s
	      )
	  | T_pinstr target ->
	      ( match self # pinstr target with
		    [ pi ] -> pi # value
		  | _      -> assert false
	      )
	  | _ ->
	      assert false

      method node_type = ntype


      method attribute n =
	att_assoc n attributes

      method attribute_names =
	att_map (fun an _ -> an) attributes

      method attribute_type n =
	match ntype with
	    T_element name ->
	      let d =
		match dtd with
		    None -> assert false
		  | Some d -> d in
	      let eltype = d # element name in
	      ( try
		  let atype, adefault = eltype # attribute n in
		  atype
		with
		    Undeclared ->
		      A_cdata
	      )
	  | _ ->
	      method_na "attribute_type"


      method required_string_attribute n =
	try
	  match att_assoc n attributes with
	      Value s -> s
	    | Valuelist l -> String.concat " " l
	    | Implied_value -> raise Not_found
	with
	    Not_found ->
	      failwith "Pxp_document, method required_string_attribute: not found"

      method optional_string_attribute n =
	try
	  match att_assoc n attributes with
	      Value s -> Some s
	    | Valuelist l -> Some (String.concat " " l)
	    | Implied_value -> None
	with
	    Not_found ->
	      None

      method required_list_attribute n =
	try
	  match att_assoc n attributes with
	      Value s -> [ s ]
	    | Valuelist l -> l
	    | Implied_value -> raise Not_found
	with
	    Not_found ->
	      failwith "Pxp_document, method required_list_attribute: not found"

      method optional_list_attribute n =
	try
	  match att_assoc n attributes with
	      Value s -> [ s ]
	    | Valuelist l -> l
	    | Implied_value -> []
	with
	    Not_found ->
	      []

      method id_attribute_name =
	match vr.id_att_name with
	    None -> raise Not_found
	  | Some name -> name

      method id_attribute_value =
	match vr.id_att_name with
	    None -> raise Not_found
	  | Some name ->
	      begin match att_assoc name attributes (* may raise Not_found *)
	      with
		  Value s -> s
		| _ -> raise Not_found
	      end


      method idref_attribute_names = vr.idref_att_names


      method quick_set_attributes atts =
	match ntype with
	    T_element _ ->
	      attributes <- att_make atts;
	  | _ ->
	      method_na "quick_set_attributes"

      method set_attributes atts =
	match ntype with
	    T_element _ ->
	      attributes <- att_make atts;
	  | _ ->
	      method_na "set_attributes"

      method private make_attribute_node element_name att_name value dtd =
	new attribute_impl 
	       ~element:element_name
	       ~name:att_name
	       value
	       dtd

      method attributes_as_nodes =
	match attributes with
	  | No_atts ->
	      []
	  | Att (_,_,_) ->
	      let dtd = self # dtd in
	      let element_name =
		match ntype with
		    T_element n -> n
		  | _ ->
		      assert false in
	      let atts' =
		att_add_nodes
		  (fun n v pos ->
		     let a =
		       self # make_attribute_node element_name n v dtd in
		     a # internal_adopt 
		           (Some (self : 'ext #node :> 'ext node)) pos;
		     a
		  )
		  attributes 
		  0
	      in
	      attributes <- atts';
	      att_return_nodes attributes
	  | _ ->
	      att_return_nodes attributes


      method create_element
	               ?name_pool_for_attribute_values
                       ?(position = no_position) 
		       ?(valcheck = true)
		       ?(att_values = [])
		       new_dtd new_type new_attlist =
	let x = extension # clone in
	let obj = ( {< parent = None;
		       extension = x;
		       pinstr = None;
		    >}
	    	    : 'ext #node :> 'ext node
		  ) in
	(* It is ok that obj does not reset rev_nodes because the init_*
	 * methods do that.
	 *)
	x # set_node obj;
	match new_type with
	    T_data ->
	      failwith "create_element: Cannot create T_data node"
	  | T_element name ->
	      obj # internal_init
                      position name_pool_for_attribute_values
		      valcheck
                      new_dtd name new_attlist att_values;
	      obj
	  | (T_comment | T_pinstr _ | T_super_root | T_none) ->
	      (* TODO: is it ok that attlist is silently ignored? *)
	      obj # internal_init_other position new_dtd new_type;
	      obj
	  | _ ->
	      failwith "create_element: Cannot create such node"


      method internal_init_other new_pos new_dtd new_ntype =
	(* resets the contents of the object *)
	parent <- None;
	rev_nodes <- [];
	nodes <- LA_not_available;
	ntype <- new_ntype;
	position <- new_pos;
	vr <- no_validation;
	attributes <- No_atts;
	dtd <- Some new_dtd;
	self # set_flag flag_ext_decl false;


      (* New attribute parsing algorithm:
       *
       * Get the attribute declaration of the element type. This is a
       * record containing:
       * - init_att_vals: an array of initial attribute values (default values
       *   or implied values)
       * - init_att_found: an array of booleans storing whether a default
       *   value has been replaced by an actual value. Initially, an array
       *   of 'false'.
       * - att_info: an array of attribute types and other per-attribute
       *   information. All arrays have the same size, and for the same
       *   attribute the same array position is used.
       * - att_lookup: a hashtable that allows quick lookup of the array
       *   position for a given attribute name
       * - att_required: a list of indexes of attributes that are required
       *
       * ROUND 1:
       *
       * In the first round the array init_att_vals is modified such that
       * the actual values replace the defaults. Furthermore, the undeclared
       * attributes are accumulated in a list undeclared_atts:
       *
       * (Round 1 can be left out if there are no declared attributes.)
       *
       * Input: The list of actual attributes new_attlist
       *
       * Initialization: init_att_vals is a copy of the array returned by
       * the DTD. Also, init_att_found must be initialized.
       *
       * For every element att of new_attlist:
       * (1) Look the index k of the attribute att up (by using att_lookup).
       *     If the attribute is undeclared, add it to undeclared_atts and
       *     continue with the next attribute. Otherwise continue with (2).
       * (2) If init_att_found.(k): failure (attribute has been defined twice)
       * (3) Get the type t of the attribute (by using att_info)
       * (4) Get the normalized value v of the attribute (by calling
       *     value_of_attribute with the lexical value and t)
       * (5) Try to overwrite the init_att_vals.(k). This may fail because the
       *     default value is #FIXED.
       * (6) If necessary: Check whether the stand-alone declaration is
       *     violated by the attribute
       * (7) Set init_att_found.(k) to true.
       *
       * After that loop, check for every member of att_required whether
       * init_att_found is set. If not, a required attribute is missing.
       *
       * If the DTD does not allow undeclared attributes, there is no
       * second round: undeclared_atts <> [] is a violation of the
       * validation constraint.
       *
       * ROUND 2:
       *
       * Input: Let n be the length of undeclared_atts.
       *
       * Variant A: Used if n is small
       *
       * Initialization: extra_att_vals is an array of n implied values.
       * k := 0
       *
       * For every attribute att and tail tl of undeclared_attributes:
       * (1) If the attribute name occurs in tl, the attribute is defined
       *     twice (failure)
       * (2) Put into extra_att_vals.(k) the normalized CDATA value of
       *     the attribute.
       * (3) Increment k
       *
       * Variant B: Used if n is big
       *
       * Initialization: extra_att_vals is an array of n implied values.
       * k := 0
       * att_names is a hashtable containing attribute names
       *
       * For every attribute att of undeclared_attributes:
       * (1) If the attribute name occurs in att_names, the attribute is
       *     defined twice (failure)
       * (2) Add the attribute name to att_names
       * (3) Put into extra_att_vals.(k) the normalized CDATA value of
       *     the attribute.
       * (4) Increment k
       *
       * MERGE RESULTS:
       *
       * Finally, init_att_vals and extra_att_vals are merged.
       *)

      method internal_init new_pos attval_name_pool 
                           valcheck_element_exists
                           new_dtd new_name
                           new_attlist new_attvalues =
	(* ONLY FOR T_Element NODES!!! *)
	(* resets the contents of the object *)
	parent <- None;
	rev_nodes <- [];
	nodes <- LA_not_available;
	ntype <- T_element new_name;
	position <- new_pos;
	dtd <- Some new_dtd;

	let lexerset = Pxp_lexers.get_lexer_set (new_dtd # encoding) in
	let sadecl = new_dtd # standalone_declaration in

	let mk_pool_value av0 =
	  match attval_name_pool with
	      None -> av0
	    | Some pool ->
		(match av0 with
		     Implied_value -> Implied_value
		   | Value s -> Value (pool_string pool s)
		   | Valuelist l ->
		       Valuelist (List.map (pool_string pool) l)
		)
	in

	let undeclared_atts = ref [] in
	let att_vals =
	  try
	    (* catch Undeclared in the following block: *)
	    let eltype = 
	      if valcheck_element_exists then
		new_dtd # element new_name (* may raise Undeclared *)
	      else
		raise Undeclared  (* raise Undeclared anyway *)
	    in
	    vr <- eltype # internal_vr;
	    self # set_flag flag_ext_decl (eltype # externally_declared);

	    let init_att_vals = Array.copy vr.init_att_vals in
	    let m = Array.length init_att_vals in

	    if m > 0 then begin
	      (* round 1 *)
	      let att_found = Array.create m false in
	      (* First iterate over new_attlist, then over new_attvalues: *)
	      (* new_attlist: *)
	      List.iter
		(fun (att_name, att_val) ->
		   let bad = ref false in
		   try
		     let k = Str_hashtbl.find vr.att_lookup att_name in
		             (* or raise Not_found *)
		     bad := true;
		     if att_found.(k) then
		       raise (WF_error("Attribute `" ^ att_name ^
				       "' occurs twice in element `" ^
				       new_name ^ "'"));
		     let att_type, att_fixed = vr.att_info.(k) in
		     let v0 = value_of_attribute
				lexerset new_dtd att_name att_type att_val in
		     let v = mk_pool_value v0 in
		     if att_fixed then begin
		       let _, v' = init_att_vals.(k) in
		       if v <> v' then
			 raise
			   (Validation_error
			      ("Attribute `" ^ att_name ^
			       "' is fixed, but has here a different value"));
		     end
		     else begin
		       init_att_vals.(k) <- att_name, v
		     end;

		     (* If necessary, check whether normalization violates
		      * the standalone declaration.
		      *)
		     if sadecl &&
                       eltype #
		       attribute_violates_standalone_declaration
		         att_name (Some att_val)
		     then
		       raise
			 (Validation_error
			    ("Attribute `" ^ att_name ^ "' of element type `" ^
			     new_name ^ "' violates standalone declaration"));

		     att_found.(k) <- true;

		   with
		       Not_found ->
			 assert(not !bad);
			 (* Raised by Hashtbl.find *)
			 let v = mk_pool_value (Value att_val) in
			 undeclared_atts := (att_name, v):: !undeclared_atts
		)
		new_attlist;

	      (* new_attvalues: Almost the same. *)
	      List.iter
		(fun (att_name, v) ->
		   let bad = ref false in
		   try
		     let k = Str_hashtbl.find vr.att_lookup att_name in
		             (* or raise Not_found *)
		     bad := true;
		     if att_found.(k) then
		       raise (WF_error("Attribute `" ^ att_name ^
				       "' occurs twice in element `" ^
				       new_name ^ "'"));
		     let att_type, att_fixed = vr.att_info.(k) in
		     check_value_of_attribute
		       lexerset new_dtd att_name att_type v;

		     (* Check: Only atts flagged as #IMPLIED
		      * can be set to Implied_value
		      *)

		     if v = Implied_value then begin
		       let _, d = init_att_vals.(k) in
		       if d = Implied_value then begin
			 if List.mem k vr.att_required then
			   raise
			     (Validation_error
				("Attribute `" ^ att_name ^ "' has Implied_value, but is declared as #REQUIRED"));
		       end
		       else
			 raise
			   (Validation_error
			      ("Attribute `" ^ att_name ^ "' has Implied_value, but is not declared as #IMPLIED"));
		     end;

		     if att_fixed then begin
		       let _, v' = init_att_vals.(k) in
		       if v <> v' then
			 raise
			   (Validation_error
			      ("Attribute `" ^ att_name ^
			       "' is fixed, but has here a different value"));
		     end
		     else begin
		       init_att_vals.(k) <- att_name, v
		     end;

		     att_found.(k) <- true;

		   with
		       Not_found ->
			 assert(not !bad);
			 (* Raised by Hashtbl.find *)
			 undeclared_atts := (att_name, v):: !undeclared_atts
		)
		new_attvalues;

              (* Check required attributes: *)
	      List.iter
		(fun k ->
		   if not att_found.(k) then begin
		     let n, _ = init_att_vals.(k) in
		     raise(Validation_error("Required attribute `" ^
					    n ^ "' is missing"))
		   end
		)
		vr.att_required;

	      (* Check standalone declaration of missing atts: *)
	      if sadecl then begin
		for k = 0 to m - 1 do
		  if not att_found.(k) then begin
		    let n, _ = init_att_vals.(k) in
		    if eltype #
		         attribute_violates_standalone_declaration
			   n None then
		      raise
			(Validation_error
			   ("Attribute `" ^ n ^ "' of element type `" ^
			    new_name ^ "' violates standalone declaration"));
		  end
		done
	      end;

	    end (* of round 1 *)
	    else
	      undeclared_atts := 
	        List.map (fun (n,s) -> n, mk_pool_value (Value s)) new_attlist @
	        new_attvalues;

	    init_att_vals
	  with
	      Undeclared ->
		(* raised by #element *)
		vr <- no_validation;
		undeclared_atts := 
		  List.map (fun (n,s) -> n, mk_pool_value (Value s)) new_attlist @
		  new_attvalues;
		[| |]
	in

	let att_vals' =
	  if !undeclared_atts <> [] then begin
	    if not vr.accept_undeclared_atts then begin
	      raise (Validation_error
		       ("The following attributes are not declared: " ^
			String.concat ", "
			  (List.map fst !undeclared_atts)))
	    end;

	    (* round 2 *)

	    let n = List.length !undeclared_atts in

	    if n < 5 then begin
	      (* variant A *)
	      simple_check_att !undeclared_atts
	    end
	    else begin
	      (* variant B *)
	      let att_names = Str_hashtbl.create n in
	      hashtbl_check_att att_names !undeclared_atts
	    end;
	    !undeclared_atts
	  end (* of round 2 *)
	  else []
	in

	let a = ref No_atts in
	att_add_array a att_vals;
	att_add_list  a att_vals';
	attributes <- !a


      method private update_vr =
	if vr == no_validation then begin
	  match ntype with
	      T_element name ->
		let dtd = self # dtd in
		( try
		    let eltype = 
		      dtd # element name (* may raise Undeclared *)
		    in
		    vr <- eltype # internal_vr;
		    self # set_flag flag_ext_decl (eltype # externally_declared);
		  with
		      Undeclared -> ()
		)
	    | _ -> ()
	end


      method validate_contents ?(use_dfa=false) ?(check_data_nodes=true) () =
	(* validates that the content of this element matches the model *)
	self # update_vr;
	if vr.content_model <> Any then begin
	  if check_data_nodes then begin
	    List.iter
	      (fun n ->
		 match n # node_type with
		     T_data ->
		       ( match self # classify_data_node n with
			     CD_normal
			   | CD_other
			   | CD_empty
			   | CD_ignorable -> ()
			   | CD_error e -> raise e
		       )
		   | _ ->
		       ()
	      )
	      rev_nodes;
	    ()
	  end;
	  let dfa = if use_dfa then Lazy.force vr.content_dfa else None in
	  if not (validate_content
		    ~use_dfa:dfa
		    vr.content_model
		    (self : 'ext #node :> 'ext node)) then
	    raise(Validation_error(self # error_name ^
				   " does not match its content model"))
	end

      method local_validate ?use_dfa ?check_data_nodes () =
	self # validate_contents ?use_dfa ?check_data_nodes ()

      method validate_attlist () =
	(* Only defined in element_impl below *)
	()

      method complement_attlist () =
	(* Only defined in element_impl below *)
	()

      method validate () =
	self # validate_attlist();
	self # validate_contents ~use_dfa:true ();  (* Use DFA if available *)

      method create_data _ _ = method_na "create_data"

      method private get_nsdecls prefixes =
	[]

      method write ?(prefixes = ([] : string list)) os enc =
	let encoding = self # encoding in
	let wms =
	  write_markup_string ~from_enc:encoding ~to_enc:enc os in

	let write_att p aname avalue =
	  match avalue with
	      Implied_value -> ()
	    | Value v ->
		wms ("\n" ^ p ^ aname ^ "=\"");
		write_data_string ~from_enc:encoding ~to_enc:enc os v;
		wms "\"";
	    | Valuelist l ->
		let v = String.concat " " l in
		wms ("\n" ^ p ^ aname ^ "=\"");
		write_data_string ~from_enc:encoding ~to_enc:enc os v;
		wms "\""
	in

	let nsdecls = self # get_nsdecls prefixes in

	begin match ntype with
	    T_element name ->
	      wms ("<" ^ name);
	      att_iter  (write_att "") attributes;
	      List.iter (fun (n,v) -> write_att "xmlns:" n v) nsdecls;
	      wms "\n>";
	  | T_comment ->
	      wms ("<!--");
	      ( match self # comment with
		    None   -> ()
		  | Some c -> wms c;
	      );
	      wms ("-->");
	  | _ ->
	      ()
	end;

	begin match pinstr with
	    None   -> ()
	  | Some l ->
	      Hashtbl.iter
		(fun n pi ->
		   pi # write os enc
		)
		l;
	end;

	let prefixes' = (List.map fst nsdecls) @ prefixes in

	List.iter
	  (fun n -> n # write ~prefixes:prefixes' os enc)
	  (self # sub_nodes);

	begin match ntype with
	    T_element name ->
	      wms ("</" ^ name ^ "\n>");
	  | _ ->
	      ()
	end

	(* TODO: How to write comments? The comment string may contain
	 * illegal characters or "--".
	 *)

    end
;;


class [ 'ext ] element_impl an_ext =
  object(self)
    inherit [ 'ext ] markup_impl an_ext as super


    method complement_attlist () =
      (* Iterate over init_att_vals of the validation record vr. Add every
       * missing attribute to the current set of attributes.
       *)
      self # update_vr;
      let old_atts = attributes in
      let new_atts = ref [] in
      Array.iter
	(fun (name,value) ->
	   try
	     ignore(att_assoc name old_atts)
	   with
	       Not_found ->
		 new_atts := (name,value) :: !new_atts
	)
	vr.init_att_vals;
      let atts = ref (att_remove_nodes attributes) in
      att_add_list atts !new_atts;
      attributes <- !atts


    method validate_attlist () =
      (* Only defined for elements:
       * Create a duplicate node and call internal_init for it. The 
       * resulting validated attlist must be equal to the current attlist.
       *)
      let atts = self # attributes in
      let dup =
	{< parent = None;
	   node_position = -1;
	   rev_nodes = [];
	   nodes = LA_not_available;
	   size = 0;
	>} in
      (* dup: flat duplicate without duplicated extension *)
      let name =
	match ntype with
	    T_element n -> n
	  | _ -> assert false
      in
      dup # internal_init no_position None true (self#dtd) name [] atts;
      (* It is possible that dup has now more attributes than atts,
       * because values in atts are missing for which the DTD defines a
       * default. (This is an error.)
       * The order of attributes is unspecified, so we must compare
       * attribute by attribute.
       *)
      let dup_atts = dup # attributes in
      let l_atts = List.length atts in
      let l_dup_atts = List.length dup_atts in
      if l_atts <> l_dup_atts then begin
	assert(l_atts < l_dup_atts);
	List.iter
	  (fun (n,_) ->
	     if not (List.mem_assoc n atts) then
	       raise(Validation_error("Attribute `" ^ n ^ 
				      "' is missing"))
	  )
	  dup_atts;
	assert false;
      end

    method internal_init_other new_pos new_dtd new_ntype =
      if new_ntype <> T_none then
	method_na "internal_init_other"
	  (* T_none is allowed because create_no_node (below) would not
	   * work otherwise
	   *)
      else
	super # internal_init_other new_pos new_dtd new_ntype

  end
;;


class [ 'ext ] comment_impl an_ext =
  object
    inherit [ 'ext ] markup_impl an_ext as super

    (* Comments are leaves, so the following methods are not applicable: *)

    method delete_nodes ?pos ?len () = method_na "delete_nodes"
    method append_node _             = method_na "append_node"
    method classify_data_node _      = method_na "classify_data_node"
    method add_node ?force _         = method_na "add_node"
    method insert_nodes ?pos _       = method_na "insert_nodes"
    method set_nodes _               = method_na "set_nodes"
    method add_pinstr _              = method_na "add_pinstr"
    method quick_set_attributes _    = method_na "quick_set_attributes"
    method set_attributes _          = method_na "set_attributes"

    method internal_init new_pos attval_name_pool new_dtd new_name
                         new_attlist =
      method_na "internal_init"

    method internal_init_other new_pos new_dtd new_ntype =
      if new_ntype <> T_comment then
	failwith "Pxp_document.comment_impl#internal_init_other: bad type";
      super # internal_init_other new_pos new_dtd new_ntype
  end
;;


class [ 'ext ] super_root_impl an_ext =
  object
    inherit [ 'ext ] markup_impl an_ext as super

    (* Super root nodes do not have attribute lists: *)

    method quick_set_attributes _    = method_na "quick_set_attributes"
    method set_attributes _          = method_na "set_attributes"

    (* TODO: Super root nodes are always roots *)

    method internal_init new_pos attval_name_pool new_dtd new_name
                         new_attlist =
      method_na "internal_init"

    method internal_init_other new_pos new_dtd new_ntype =
      if new_ntype <> T_super_root then
	failwith "Pxp_document.super_root_impl#internal_init_other: bad type";
      super # internal_init_other new_pos new_dtd new_ntype
  end
;;


class [ 'ext ] pinstr_impl an_ext =
  object
    inherit [ 'ext ] markup_impl an_ext as super

    (* PIs are leaves, so the following methods are not applicable: *)

    method delete_nodes ?pos ?len () = method_na "delete_nodes"
    method append_node _             = method_na "append_node"
    method classify_data_node _      = method_na "classify_data_node"
    method add_node ?force _         = method_na "add_node"
    method insert_nodes ?pos _       = method_na "insert_nodes"
    method set_nodes _               = method_na "set_nodes"
    method quick_set_attributes _    = method_na "quick_set_attributes"
    method set_attributes _          = method_na "set_attributes"

    method add_pinstr pi =
      (* fail if applied more than once *)
      if pinstr <> None then
	failwith "Pxp_document.pinstr_impl # add_pinstr: the node can only contain one processing instruction";
      super # add_pinstr pi

    method internal_init new_pos attval_name_pool new_dtd new_name
                         new_attlist =
      method_na "internal_init"

    method internal_init_other new_pos new_dtd new_ntype =
      match new_ntype with
	  T_pinstr _ ->
	    super # internal_init_other new_pos new_dtd new_ntype
	| _ ->
	    failwith "Pxp_document.pinstr_impl#internal_init_other: bad type";
  end
;;


let pinstr n =
  match n # node_type with
      T_pinstr pi ->
	( match n # pinstr pi with
	      [ pi_obj ] -> pi_obj
	    | _ -> assert false
	)
    | _ ->
	invalid_arg "Pxp_document.pinstr"
;;


class ['ext] namespace_attribute_impl ~element ~name value dtd =
   object (self)
    inherit [ 'ext ] attribute_impl ~element ~name value dtd as super

    val mutable normprefix = ""
    val mutable localname = ""

    initializer
      try
	let n = String.index name ':' in   (* may raise Not_found *)
	normprefix <- String.sub name 0 n;
	localname  <- String.sub name (n+1) (String.length name - n - 1)
	(* TODO: Check that localname is a name *)
	(* TODO: Use pools *)
      with
	  Not_found ->
	    (* No ':' *)
	    normprefix <- "";
	    localname  <- name

    method normprefix = normprefix
    method localname = localname

    method namespace_uri = 
      self # namespace_manager # get_primary_uri normprefix

    method namespace_info =
      self # parent # namespace_info

    method set_namespace_info x =
      method_na "set_namespace_info"

    method namespace_manager =
      self # dtd # namespace_manager

  end
;;


class [ 'ext ] namespace_element_impl an_ext =
  object (self)
    inherit [ 'ext ] element_impl an_ext as super

    val mutable normprefix = ""
    val mutable localname = ""
    val mutable nsinfo = None

    method normprefix = normprefix
    method localname = localname
    method namespace_uri = 
      self # namespace_manager # get_primary_uri normprefix

    method namespace_info =
      match nsinfo with
	  None -> raise Not_found
	| Some x -> x

    method set_namespace_info x =
      nsinfo <- x

    method namespace_manager =
      self # dtd # namespace_manager

    method internal_init new_pos attval_name_pool valcheck_element_exists
                         new_dtd new_name
                         new_attlist new_attvalues =

      super # internal_init
	new_pos attval_name_pool valcheck_element_exists new_dtd new_name 
	        new_attlist new_attvalues;

      try
	let n = String.index new_name ':' in   (* may raise Not_found *)
	normprefix <- String.sub new_name 0 n;
	localname  <- String.sub new_name (n+1) (String.length new_name - n - 1)
	(* TODO: Check that localname is a name *)
	(* TODO: Use pools *)
      with
	  Not_found ->
	    (* No ':' *)
	    normprefix <- "";
	    localname  <- new_name


    method private get_nsdecls prefixes =
      (* This method modifies the behaviour of 'write'. In 'prefixes' the
       * list of already printed namespace declarations is passed to this 
       * method. The task is to check whether additional declarations are
       * necessary and to pass them back as list of pairs (normprefix, uri).
       *)
      let scan_att name value =  (* return prefix of attribute *)
	try
	  let n = String.index name ':' in   (* may raise Not_found *)
	  String.sub name 0 n
	with
	    Not_found -> ""
      in

      let rec add_prefixes prefixes candidates =
	match candidates with
	    [] -> []
	  | "" :: candidates' ->
	      add_prefixes prefixes candidates'
	  | p :: candidates' ->
	      if List.mem p prefixes then
		add_prefixes prefixes candidates'
	      else
		p :: (add_prefixes (p :: prefixes) candidates')
      in

      let p_candidates =
	normprefix ::
	(att_map scan_att attributes)
      in
      let prefixes' = add_prefixes prefixes p_candidates in
      let mng = self # namespace_manager in

      List.map
	(fun p -> 
	   try
	     p, Value (mng # get_primary_uri p)
	   with
	       Not_found -> (* raised by get_primary_uri *)
		 failwith ("Pxp_document.namespace_element_impl#write: cannot map the prefix `" ^ p ^ "' to any URI")
	)

	prefixes'

      method private make_attribute_node element_name att_name value dtd =
	(* This method modifies the behaviour of attributes_as_nodes *)
	new namespace_attribute_impl 
	       ~element:element_name
	       ~name:att_name
	       value
	       dtd
  end
;;


class [ 'ext ] namespace_impl srcprefix normprefix dtd =
  (object (self)
     val mutable parent = (None : 'ext node option)
     val mutable node_position = -1
     val normprefix = normprefix
     val srcprefix = srcprefix

     method parent =
       match parent with
	   None -> raise Not_found
	 | Some p -> p

     method root =
       match parent with
	   None -> (self : 'ext #node :> 'ext node)
	 | Some p -> p # root

    method node_position =
      if node_position >= 0 then node_position else
	raise Not_found

    method node_path =
      let rec collect n path =
	try
	  let p = n # node_position in
	  collect (n # parent) (p :: path)
	with
	    Not_found ->
	      (* n is the root *)
	      path
      in
      try
	collect (self # parent) [ -2; self # node_position ]
      with
	  Not_found -> [ self # node_position ]

     method internal_adopt new_parent pos =
       parent <- new_parent;
       node_position <- pos;

     method orphaned_clone =
       {< parent = None >}

     method orphaned_flat_clone =
       {< parent = None >}

     method dtd = dtd

     method encoding = dtd # encoding

     method node_type = T_namespace srcprefix

     method data = 
       (self # namespace_manager) # get_primary_uri normprefix

     method normprefix = 
       (* This is a hack to ensure whenever there is no srcprefix we will
	* not have a normprefix, either.
	* However, there may be a namespace URI
	*)
       if srcprefix = "" then
	 ""
       else
	 normprefix

    method namespace_uri = 
      (* XPath requires this to be null: *)
      raise Not_found

    method namespace_manager =
      self # dtd # namespace_manager

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      Format.pp_print_string fmt "+ T_namespace";
      Format.pp_print_cut fmt ();
      Format.pp_print_string fmt "normprefix=";
      Format.pp_print_string fmt (self # normprefix);
      Format.pp_print_cut fmt ();
      Format.pp_print_string fmt "srcprefix=";
      Format.pp_print_string fmt srcprefix;
      Format.pp_print_cut fmt ();
      Format.pp_print_string fmt "uri=";
      ( try
	  Format.pp_print_string fmt (self # data);
	with
	    Not_found ->
	      Format.pp_print_string fmt "<Not found>"
      );
      Format.pp_close_box fmt ()

    (* Senseless methods: *)

     method attribute n = raise Not_found
     method attribute_names = []
     method attribute_type n = raise Not_found
     method attributes = []
     method required_string_attribute n = raise Not_found
     method required_list_attribute n = []
     method optional_string_attribute n = None
     method optional_list_attribute n = []
     method sub_nodes = []
     method pinstr _ = []
     method pinstr_names = []
     method iter_nodes _ = ()
     method iter_nodes_sibl _ = ()
     method nth_node _ = raise Not_found
     method position = no_position
     method comment = None
     method local_validate ?use_dfa ?check_data_nodes () = ()
     method validate_contents ?use_dfa ?check_data_nodes () = ()
     method validate_attlist () = ()
     method complement_attlist () = ()
     method validate () = ()

    (* Non-applicable methods: *)

     method extension =          method_na "extension"
     method delete =             method_na "delete"
     method delete_nodes ?pos ?len _ = method_na "delete_nodes"
     method previous_node =      method_na "previous_node"
     method next_node =          method_na "next_node"
     method internal_set_pos _ = method_na "internal_set_pos"
     method internal_delete _ =  method_na "internal_delete"
     method internal_init _ _ _ _ _ _ _ =   method_na "internal_init"
     method internal_init_other _ _ _ = method_na "internal_init_other"
     method classify_data_node _ = method_na "classify_data_node"
     method append_node _ =      method_na "append_node"
     method insert_nodes ?pos _ = method_na "insert_nodes"
     method add_node ?force _ =  method_na "add_node"
     method add_pinstr _ =       method_na "add_pinstr"
     method set_nodes _ =        method_na "set_nodes"
     method quick_set_attributes _ =    method_na "quick_set_attributes"
     method set_attributes _ =    method_na "set_attributes"
     method attributes_as_nodes =       method_na "attributes_as_nodes"
     method set_comment c =      method_na "set_comment"
     method set_data _ =         method_na "set_data"
     method create_element ?name_pool_for_attribute_values ?position 
                           ?valcheck ?att_values _ _ _ =
                                 method_na "create_element"
     method create_data _ _ =    method_na "create_data"
     method write ?prefixes _ _ = method_na "write"
     method id_attribute_name =  method_na "id_attribute_name"
     method id_attribute_value = method_na "id_attribute_value"
     method idref_attribute_names =     method_na "idref_attribute_names"
     method localname =          method_na "localname"
     method namespace_info =     method_na "namespace_info"
     method set_namespace_info info =
                                 method_na "set_namespace_info"
   end
     : ['ext] node)
;;

let namespace_normprefix n =
  match n # node_type with
      T_namespace _ -> n # normprefix
    | _ -> invalid_arg "Pxp_document.namespace_normprefix"
;;

let namespace_srcprefix n =
  match n # node_type with
      T_namespace s -> s
    | _ -> invalid_arg "Pxp_document.namespace_srcprefix"
;;

let namespace_uri n =
  match n # node_type with
      T_namespace _ -> n # data  (* sic! *)
    | _ -> invalid_arg "Pxp_document.namespace_uri"
;;


class [ 'ext ] namespace_info_impl 
                 (srcprefix:string) (element : 'ext node) mapping =
object
  val srcprefix = srcprefix
  val parent = element
  val mutable src_norm_mapping = mapping
  val mutable declaration = None

  method srcprefix = srcprefix

  method declaration =
    match declaration with
	Some d -> d
      | None ->
	  let d = ref [] in
	  let done_srcprefixes = ref [] in
	  let dtd = parent # dtd in
	  let pos = ref 0 in
	  (* Note: The first pair ("!", s) contains the current default
	   * namespace. if s = "" there is no such default; otherwise,
	   * s is the normprefix of the default.
	   *)
	  List.iter
	    (fun (srcprefix, normprefix) -> 
	       if not (List.mem srcprefix !done_srcprefixes) then begin
		 let srcprefix' = 
		   if srcprefix = "!" then "" else srcprefix in
		 if normprefix <> "" then begin
		   let nsnode = new namespace_impl srcprefix' normprefix dtd in
		   nsnode # internal_adopt (Some parent) !pos;
		   d := nsnode :: !d;
		 end;
		 done_srcprefixes := srcprefix :: !done_srcprefixes;
	       end
	    )
	    src_norm_mapping;
	  declaration <- Some !d;
	  src_norm_mapping <- [];     (* save memory *)
	  !d
end
;;

let spec_table_find_exemplar tab eltype =
  try
    Hashtbl.find tab.mapping eltype
  with
      Not_found -> tab.default_element
;;


let create_data_node spec dtd str =
  match spec with
      Spec_table tab ->
	let exemplar = tab.data_node in
	exemplar # create_data dtd str
;;


let get_data_exemplar spec =
  match spec with
      Spec_table tab ->
	tab.data_node
;;


let create_element_node ?name_pool_for_attribute_values ?position 
                        ?valcheck ?att_values 
                        spec dtd eltype atts =
   match spec with
      Spec_table tab ->
	let exemplar = spec_table_find_exemplar tab eltype in
	exemplar # create_element
	    ?name_pool_for_attribute_values
            ?position
	    ?valcheck
	    ?att_values
            dtd (T_element eltype) atts
;;


let get_element_exemplar spec eltype atts =
   match spec with
      Spec_table tab ->
	spec_table_find_exemplar tab eltype
;;


let create_super_root_node ?position spec dtd =
    match spec with
      Spec_table tab ->
	( match tab.super_root_node with
	      None ->
		failwith "Pxp_document.create_super_root_node: No exemplar"
	    | Some x ->
		x # create_element ?position:position dtd T_super_root []
	)
;;


let get_super_root_exemplar spec =
    match spec with
      Spec_table tab ->
	( match tab.super_root_node with
	      None ->
		raise Not_found
	    | Some x ->
		x
	)
;;


(* TODO: This function is broken, because an element will no longer
 * accept the type T_none
 *)
let create_no_node ?position spec dtd =
    match spec with
      Spec_table tab ->
	let x = tab.default_element in
	x # create_element ?position:position dtd T_none []
;;


let create_comment_node ?position spec dtd text =
  match spec with
      Spec_table tab ->
	( match tab.comment_node with
	      None ->
		failwith "Pxp_document.create_comment_node: No exemplar"
	    | Some x ->
		let e = x # create_element ?position:position dtd T_comment []
		in
		e # set_comment (Some text);
		e
	)
;;


let get_comment_exemplar spec =
  match spec with
      Spec_table tab ->
	( match tab.comment_node with
	      None ->
		raise Not_found
	    | Some x ->
		x
	)
;;


let create_pinstr_node ?position spec dtd pi =
  let target = pi # target in
  let exemplar =
    match spec with
	Spec_table tab ->
	  ( try
	      Hashtbl.find tab.pinstr_mapping target
	    with
		Not_found ->
		  ( match tab.default_pinstr_node with
			None ->
			  failwith
			    "Pxp_document.create_pinstr_node: No exemplar"
		      | Some x -> x
		  )
	  )
  in
  let el =
    exemplar # create_element ?position:position dtd (T_pinstr target) [] in
  el # add_pinstr pi;
  el
;;


let get_pinstr_exemplar spec pi =
  let target = pi # target in
  match spec with
      Spec_table tab ->
	( try
	    Hashtbl.find tab.pinstr_mapping target
	  with
	      Not_found ->
		( match tab.default_pinstr_node with
		      None ->
			raise Not_found
		    | Some x -> x
		)
	)
;;


(* TODO: try to avoid sub_nodes in the following; replace it with
 * iter_nodes.
 *)


let find ?(deeply=false) f base =
  let rec search_flat children =
    match children with
	[] -> raise Not_found
      | n :: children' ->
	  if f n then n else search_flat children'
  in
  let rec search_deep children =
    match children with
	[] -> raise Not_found
      | n :: children' ->
	  if f n then
	    n
	  else
	    try search_deep (n # sub_nodes)
	    with Not_found -> search_deep children'
  in
  (if deeply then search_deep else search_flat)
  (base # sub_nodes)
;;


let find_all ?(deeply=false) f base =
  let rec search_flat children =
    match children with
	[] -> []
      | n :: children' ->
	  if f n then n :: search_flat children' else search_flat children'
  in
  let rec search_deep children =
    match children with
	[] -> []
      | n :: children' ->
	  let rest =
	    search_deep (n # sub_nodes) @ search_deep children' in
	  if f n then
	    n :: rest
	  else
	    rest
  in
  (if deeply then search_deep else search_flat)
  (base # sub_nodes)
;;


let find_element ?deeply eltype base =
  find
    ?deeply:deeply
    (fun n ->
       match n # node_type with
	   T_element name -> name = eltype
	 | _              -> false)
    base
;;


let find_all_elements ?deeply eltype base =
  find_all
    ?deeply:deeply
    (fun n ->
       match n # node_type with
	   T_element name -> name = eltype
	 | _              -> false)
    base
;;


exception Skip;;

let map_tree ~pre ?(post=(fun x -> x)) base =
  let rec map_rec n =
    (try
      let n' = pre n in
      if n' # node_type <> T_data then begin
	let children = n # sub_nodes in
	let children' = map_children children in
	n' # set_nodes children';
      end;
      post n'
    with
	Skip -> raise Not_found
    )
  and map_children l =
    match l with
	[] -> []
      | child :: l' ->
	  (try
	     let child' = map_rec child in
	     child' :: map_children l'
	   with
	       Not_found ->
		 map_children l'
	  )
  in
  map_rec base
;;


let map_tree_sibl ~pre ?(post=(fun _ x _ -> x)) base =
  let rec map_rec l n r =
    (try
      let n' = pre l n r in
      if n' # node_type <> T_data then begin
	let children = n # sub_nodes in
	let children' = map_children None children in
	let children'' = postprocess_children None children' in
	n' # set_nodes children'';
      end;
      n'
    with
	Skip -> raise Not_found
    )
  and map_children predecessor l =
    (match l with
	 [] -> []
       | child :: l' ->
	   let successor =
	     match l' with
		 []    -> None
	      | x :: _ -> Some x in
	   (try
	      let child' = map_rec predecessor child successor in
	      child' :: map_children (Some child) l'
	    with
		Not_found ->
		  map_children (Some child) l'
	   )
    )
  and postprocess_children predecessor l =
    (match l with
	 [] -> []
       | child :: l' ->
	   let successor =
	     match l' with
		 []     -> None
	       | x :: _ -> Some x in
	   (try
	      let child' = post predecessor child successor in
	      child' :: postprocess_children (Some child) l'
	    with
		Skip ->
		  postprocess_children (Some child) l'
	   )
    )
  in
  let base' = map_rec None base None in
  try post None base' None with Skip -> raise Not_found
;;


let iter_tree ?(pre=(fun x -> ())) ?(post=(fun x -> ())) base =
  let rec iter_rec n =
    (try
      pre n;
      let children = n # sub_nodes in
      iter_children children;
      post n
    with
	Skip -> raise Not_found
    )
  and iter_children l =
    match l with
	[] -> []
      | child :: l' ->
	  (try
	     iter_rec child;
	     iter_children l'
	   with
	       Not_found ->
		 iter_children l'
	  )
  in
  iter_rec base
;;


let iter_tree_sibl ?(pre=(fun _ _ _ -> ())) ?(post=(fun _ _ _ -> ())) base =
  let rec iter_rec l n r =
    (try
      pre l n r;
      let children = n # sub_nodes in
      iter_children None children;
      post l n r
    with
	Skip -> raise Not_found
    )
  and iter_children predecessor l =
    (match l with
	 [] -> []
       | child :: l' ->
	   let successor =
	     match l' with
		 []    -> None
	      | x :: _ -> Some x in
	   (try
	      iter_rec predecessor child successor;
	      iter_children (Some child) l'
	    with
		Not_found ->
		  iter_children (Some child) l'
	   )
    )
  in
  iter_rec None base None
;;


let validate tree =
  iter_tree
    ~pre:(fun n -> n # validate())
    tree
;;


let compare a b =
  let rec cmp p1 p2 =
    match p1, p2 with
	[], []         -> 0
      | [], _          -> -1
      | _, []          -> 1
      | x::p1', y::p2' -> if x = y then cmp p1' p2' else x - y
  in

  let a_path = a # node_path in
  let b_path = b # node_path in

  cmp a_path b_path
;;


type 'ext ord_index = ('ext node, int) Hashtbl.t;;

let create_ord_index base =
  (* Note: Attribute and namespace nodes are not entered into the 
   * ordinal index; but some ordinal numbers are reserved for them.
   *)
  let n = ref 0 in
  iter_tree ~pre:(fun _ -> incr n) base;
  let idx = Hashtbl.create !n in
  let k = ref 0 in
  iter_tree 
    ~pre:(fun node -> 
	    match node # node_type with
		T_element _ ->
		  Hashtbl.add idx node (!k + 2); k := !k + 3
		    (* Reserve !k for namespace nodes, and !k+1 for 
		     * attribute nodes
		     *)
	      | _ ->
		  Hashtbl.add idx node !k; incr k
	 ) 
    base;
  idx
;;


let ord_number idx node =
  Hashtbl.find idx node
;;

let ord_compare idx a b =
  let get_index x =
    match x # node_type with
	T_attribute _ -> Hashtbl.find idx (x # parent) - 1, x # node_position
      | T_namespace _ -> Hashtbl.find idx (x # parent) - 2, x # node_position
      | _             -> Hashtbl.find idx x, 0
  in

  let ord_a, subord_a = get_index a in
  let ord_b, subord_b = get_index b in
  let d = ord_a - ord_b in
  if d = 0 then
    subord_a - subord_b
  else
    d
;;

type stripping_mode =
  [ `Strip_one_lf
  | `Strip_one
  | `Strip_seq
  | `Disabled
  ]

let strip_whitespace ?(force = false) ?(left = `Disabled) ?(right = `Disabled)
                     ?(delete_empty_nodes = true)
                     start =
  let strip_left =
    match left with
	`Disabled -> 
	  (fun s -> s)
      | `Strip_one_lf ->
	  (fun s -> 
	     if String.length s > 0 && s.[0] = '\n' then
	       String.sub s 1 (String.length s - 1)
	     else
	       s
	  )
      | `Strip_one ->
	  (fun s -> 
	     if String.length s > 0 then
	       let c = s.[0] in
	       if c = ' ' || c = '\n' || c = '\r' || c = '\t' then
		 String.sub s 1 (String.length s - 1)
	       else
		 s
	     else
	       s
	  )
      | `Strip_seq ->
	  (fun s ->
	     let k = ref 0 in
	     while !k < String.length s && 
	           (let c = s.[ !k ] in 
		    c = ' ' || c = '\n' || c = '\r' || c = '\t')
	     do
	       incr k
	     done;
	     if !k > 0 then
	       String.sub s !k (String.length s - !k)
	     else
	       s
	  )
  in
  let strip_right =
    match right with
	`Disabled -> 
	  (fun s -> s)
      | `Strip_one_lf ->
	  (fun s -> 
	     let l = String.length s in
	     if l > 0 && s.[l - 1] = '\n' then
	       String.sub s 0 (l - 1)
	     else
	       s
	  )
      | `Strip_one ->
	  (fun s -> 
	     let l = String.length s in
	     if l > 0 then
	       let c = s.[ l - 1] in
	       if c = ' ' || c = '\n' || c = '\r' || c = '\t' then
		 String.sub s 0 (l - 1)
	       else
		 s
	     else
	       s
	  )
      | `Strip_seq ->
	  (fun s ->
	     let l = String.length s in
	     let k = ref (l-1) in
	     while !k >= 0 && 
	           (let c = s.[ !k ] in 
		    c = ' ' || c = '\n' || c = '\r' || c = '\t')
	     do
	       decr k
	     done;
	     if !k < l-1 then
	       String.sub s 0 (!k + 1)
	     else
	       s
	  )
  in
  let rec strip_elements n preserve_space =
    let preserve_space' =
      not force &&
      try
	let space = n # attribute "xml:space" in
	let preserve = (space = Value "preserve") in
	let default = (space = Value "default") in
	if not (preserve || default) then raise Not_found;
	preserve
      with
	  Not_found -> preserve_space
    in
    if not preserve_space' then begin
      let left_side_done = ref false in
      let right_side = ref None in
      n # iter_nodes
	(fun sub ->
	   match sub # node_type with
	       T_data ->
		 if not !left_side_done then begin
		   let s = strip_left (sub # data) in
		   sub # set_data s;
		   if s = "" && delete_empty_nodes then sub # delete;
		   left_side_done := true
		 end;
		 right_side := Some sub;      (* candidate for right side *)
	     | T_element _ ->
		 left_side_done := true;
		 right_side := None;
		 strip_elements sub preserve_space'
	     | T_comment ->
		 ()
	     | _ ->
		 left_side_done := true;
		 right_side := None;
	);	       
      match !right_side with
	  None -> 
	    ()
	| Some sub ->
	    let s = strip_right (sub # data) in
	    sub # set_data s;
	    if s = "" && delete_empty_nodes then sub # delete;
    end
  in
  let rec preserve_mode n =
    try
      let space = n # attribute "xml:space" in
      let preserve = (space = Value "preserve") in
      let default = (space = Value "default") in
      if not (preserve || default) then raise Not_found;
      preserve
    with
	Not_found ->
	  let p = 
	    try Some (n # parent) with Not_found -> None in
	  match p with
	      Some parent -> preserve_mode parent
	    | None -> false
  in
  let preserve_space = not force && preserve_mode start in
  match start # node_type with
      T_data ->
	if not preserve_space then begin
	  let s = start # data in
	  let s' = strip_left (strip_right s) in
	  start # set_data s';
	  if s' = "" && delete_empty_nodes then start # delete;
	end
    | T_element _ 
    | T_super_root ->
	strip_elements start preserve_space
    | _ ->
	()
;;


let normalize tree =
  (* Concatenate consecutive data nodes: *)
  iter_tree_sibl
    ~pre:(fun l n r ->
	    match l with
		None ->
		  (* No left sibling: Nothing to do *)
		  ()
	      | Some ln ->
		  (* If ln and n are both data nodes, concatenate them *)
		  if n # node_type = T_data && ln # node_type = T_data then 
		    begin
		      n  # set_data (ln # data ^ n # data);
		      ln # set_data "";
		    end
	 )
    tree;
  (* Remove empty data nodes: *)
  iter_tree
    ~pre:(fun n ->
	    if n # node_type = T_data && n # data = "" then begin
	      n # delete;
	      raise Skip;
	    end
	 )
    tree
;;


class ['ext] document the_warner =
  object (self)
    val mutable xml_version = "1.0"
    val mutable dtd = (None : dtd option)
    val mutable root = (None : 'ext node option)

    val pinstr = lazy (Hashtbl.create 10 : (string,proc_instruction) Hashtbl.t)
    val warner = (the_warner : collect_warnings)

    method init_xml_version s =
      if s <> "1.0" then
	warner # warn ("XML version '" ^ s ^ "' not supported");
      xml_version <- s

    method init_root r =
      let dtd_r = r # dtd in
      match r # node_type with

	(**************** CASE: We have a super root element ***************)

	| T_super_root ->
	    if not (dtd_r # arbitrary_allowed) then begin
	      match dtd_r # root with
		  Some declared_root_element_name ->
		    let real_root_element =
		      try
			List.find
			  (fun r' ->
			     match r' # node_type with
			       | T_element _     -> true
			       | _               -> false)
			  (r # sub_nodes)
		      with
			  Not_found ->
			    failwith "Pxp_document.document#init_root: Super root does not contain root element"
			      (* TODO: Check also that there is at most one
			       * element in the super root node
			       *)

		    in
		    let real_root_element_name =
		      match real_root_element # node_type with
			  T_element name -> name
			| _              -> assert false
		    in
		    if real_root_element_name <> declared_root_element_name then
		      raise
			(Validation_error ("The root element is `" ^
					   real_root_element_name ^
					   "' but is declared as `" ^
					   declared_root_element_name))
		| None -> ()
	    end;
	    (* All is okay, so store dtd and root node: *)
	    dtd <- Some dtd_r;
	    root <- Some r

	(**************** CASE: No super root element **********************)

	| T_element root_element_name ->
	    if not (dtd_r # arbitrary_allowed) then begin
	      match dtd_r # root with
		  Some declared_root_element_name ->
		    if root_element_name <> declared_root_element_name then
		      raise
			(Validation_error ("The root element is `" ^
					   root_element_name ^
					   "' but is declared as `" ^
					   declared_root_element_name))
		| None ->
		    (* This may happen if you initialize your DTD yourself.
		     * The value 'None' means that the method 'set_root' was
		     * never called for the DTD; we interpret it here as:
		     * The root element does not matter.
		     *)
		    ()
	    end;
	    (* All is okay, so store dtd and root node: *)
	    dtd <- Some dtd_r;
	    root <- Some r

	| _ ->
	    failwith "Pxp_document.document#init_root: the root node must be an element or super-root"

    method xml_version = xml_version

    method xml_standalone =
      match dtd with
	  None -> false
	| Some d -> d # standalone_declaration

    method dtd =
      match dtd with
	  None -> failwith "Pxp_document.document#dtd: Document has no DTD"
	| Some d -> d

    method encoding =
      match dtd with
	  None -> failwith "Pxp_document.document#encoding: Document has no DTD"
	| Some d -> d # encoding

    method root =
      match root with
	  None -> failwith "Pxp_document.document#root: Document has no root element"
	| Some r -> r

    method add_pinstr pi =
      begin match dtd with
	  None -> ()
	| Some d ->
	    if pi # encoding <> d # encoding then
	      failwith "Pxp_document.document # add_pinstr: Inconsistent encodings";
      end;
      let name = pi # target in
      Hashtbl.add (Lazy.force pinstr) name pi

    method pinstr name =
      Hashtbl.find_all (Lazy.force pinstr) name

    method pinstr_names =
      let l = ref [] in
      Hashtbl.iter
	(fun n _ -> l := n :: !l)
	(Lazy.force pinstr);
      !l

    method write os enc =
      let encoding = self # encoding in
      let wms =
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      let r = self # root in
      wms ("<?xml version='1.0' encoding='" ^
	   Netconversion.string_of_encoding enc ^
	   "'?>\n");
      ( match self # dtd # root with
	    None ->
	      self # dtd # write os enc false
	  | Some _ ->
	      self # dtd # write os enc true
      );
      Hashtbl.iter
	(fun n pi ->
	   pi # write os enc
	)
	(Lazy.force pinstr);
      r # write os enc;
      wms "\n";

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      Format.pp_print_string fmt "* document";
      format_pinstr fmt (Some (Lazy.force pinstr));
      (match root with
	   None -> ()
	 | Some r ->
	     Format.pp_print_cut fmt ();
	     r # dump fmt
      );
      Format.pp_close_box fmt ();


  end
;;

let print_node (n : 'ext node) =
  n # dump (Format.std_formatter)
;;


let print_doc (n : 'ext document) =
  n # dump (Format.std_formatter)
;;


(* ======================================================================
 * History:
 *
 * $Log: pxp_document.ml,v $
 * Revision 1.23  2001/06/09 22:33:14  gerd
 * 	Added 'dump' methods to 'node' and 'document'. Also print_node,
 * print_doc.
 * 	Fixed namespace_info.
 *
 * Revision 1.22  2001/06/08 01:15:46  gerd
 * 	Moved namespace_manager from Pxp_document to Pxp_dtd. This
 * makes it possible that the DTD can recognize the processing instructions
 * <?pxp:dtd namespace prefix="..." uri="..."?>, and add the namespace
 * declaration to the manager.
 *
 * Revision 1.21  2001/06/08 00:12:56  gerd
 * 	Implemented rev. 1.16 of pxp_document.mli.
 *
 * Revision 1.20  2001/05/17 21:38:37  gerd
 * 	Continued the implementation of namespaces.
 *
 * Revision 1.19  2001/04/26 23:59:36  gerd
 * 	Experimental support for namespaces: classes namespace_impl,
 * namespace_element_impl, namespace_attribute_impl.
 * 	New classes comment_impl, pinstr_impl, super_root_impl. These
 * classes have been added for stricter (runtime) type checking.
 *
 * Revision 1.18  2000/10/01 19:46:28  gerd
 * 	Optimizations, especially in internal_init.
 *
 * Revision 1.17  2000/09/22 22:54:30  gerd
 * 	Optimized the attribute checker (internal_init of element
 * nodes). The validation_record has now more fields to support
 * internal_init.
 *
 * Revision 1.16  2000/09/21 21:29:41  gerd
 * 	New functions get_*_exemplar.
 *
 * Revision 1.15  2000/09/09 16:41:03  gerd
 * 	Effort to reduce the amount of allocated memory: The number of
 * instance variables in document nodes has been miminized; the class
 * default_ext no longer stores anything; string pools have been implemented.
 *
 * Revision 1.14  2000/08/30 15:47:52  gerd
 * 	Implementation of pxp_document.mli rev 1.10.
 *
 * Revision 1.13  2000/08/26 23:29:10  gerd
 * 	Implementations for the changed in rev 1.9 of pxp_document.mli.
 *
 * Revision 1.12  2000/08/18 20:14:00  gerd
 * 	New node_types: T_super_root, T_pinstr, T_comment, (T_attribute),
 * (T_none), (T_namespace).
 *
 * Revision 1.11  2000/08/14 22:24:55  gerd
 * 	Moved the module Pxp_encoding to the netstring package under
 * the new name Netconversion.
 *
 * Revision 1.10  2000/07/23 02:16:34  gerd
 * 	Support for DFAs.
 *
 * Revision 1.9  2000/07/16 19:37:09  gerd
 * 	Simplification.
 *
 * Revision 1.8  2000/07/16 17:50:01  gerd
 * 	Fixes in 'write'
 *
 * Revision 1.7  2000/07/16 16:34:41  gerd
 * 	New method 'write', the successor of 'write_compact_as_latin1'.
 *
 * Revision 1.6  2000/07/14 13:56:11  gerd
 * 	Added methods id_attribute_name, id_attribute_value,
 * idref_attribute_names.
 *
 * Revision 1.5  2000/07/09 17:51:14  gerd
 * 	Element nodes can store positions.
 *
 * Revision 1.4  2000/07/08 23:04:06  gerd
 * 	[Merging 0.2.10:] Bugfix: allow_undeclared_attribute
 *
 * Revision 1.3  2000/07/04 22:10:06  gerd
 * 	Implemented rev 1.3 of pxp_document.mli in a straight-
 * forward fashion.
 *
 * Revision 1.2  2000/06/14 22:19:06  gerd
 * 	Added checks such that it is impossible to mix encodings.
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
 * Old logs from markup_document.ml:
 *
 * Revision 1.19  2000/05/27 19:14:42  gerd
 * 	value_of_attribute: this function has been moved to
 * markup_aux.ml.
 *
 * 	Added the following checks whether there is a violation
 * against the standalone declaration:
 * 	- Externally declared elements with regexp content model
 * 	  must not contain extra white space
 * 	- The effect of normalization of externally declared attributes
 * 	  must not depend on the type of the attributes
 * 	- Declared default values of externally declared attributes
 * 	  must not have an effect on the value of the attributes.
 *
 * 	Removed the method init_xml_standalone. It is now stored in
 * the DTD whether there is a standalone declaration.
 *
 * Revision 1.18  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.17  2000/05/06 23:12:20  gerd
 * 	Allow undeclared attributes.
 *
 * Revision 1.16  2000/05/01 20:42:28  gerd
 * 	New method write_compact_as_latin1.
 *
 * Revision 1.15  2000/04/30 18:15:22  gerd
 * 	In function validate_content: Special handling of the pseudo
 * nodes "-pi" and "-vr".
 * 	Method init_root, class document: Recognizes whether the
 * root is virtual or real. The check on the root element name is different
 * in each case.
 * 	New method keep_always_whitespace_mode: Turns a special mode
 * on in which ignorable whitespace is included into the document.
 *
 * Revision 1.14  2000/03/11 22:58:15  gerd
 * 	Updated to support Markup_codewriter.
 *
 * Revision 1.13  2000/01/27 21:51:56  gerd
 * 	Added method 'attributes'.
 *
 * Revision 1.12  2000/01/27 21:19:34  gerd
 * 	Added methods.
 * 	Bugfix: 'orphaned_clone' performs now really a clone.
 *
 * Revision 1.11  2000/01/20 21:57:58  gerd
 * 	Bugfix: method set_nodes does no longer add the new subnodes
 * in the reverse order.
 *
 * Revision 1.10  1999/12/17 21:35:37  gerd
 * 	Bugfix: If the name of the root element is not specified in
 * the DTD, the document does not check whether the root element is a
 * specific element.
 *
 * Revision 1.9  1999/11/09 22:22:01  gerd
 * 	The "document" classes now checks that the root element is the
 * same as the declared root element. Thanks to Claudio Sacerdoti Coen
 * for his bug report.
 *
 * Revision 1.8  1999/09/01 22:51:40  gerd
 * 	Added methods to store processing instructions.
 *
 * Revision 1.7  1999/09/01 16:19:18  gerd
 * 	Added some warnings.
 * 	If an element type has the content model EMPTY, it is now strictly
 * checked that the element instance is really empty. Especially, white space
 * is NOT allowed in such instances.
 *
 * Revision 1.6  1999/08/19 21:58:59  gerd
 * 	Added method "reset_finder". This is not very convincing, but
 * currently the simplest way to update the ID hash table.
 *
 * Revision 1.5  1999/08/19 01:08:15  gerd
 * 	Added method "find" that searches node by ID in the whole
 * tree.
 * 	Bugfix: After the extension has been cloned, the "set_node" method
 * is invoked telling the clone to which node it is associated.
 *
 * Revision 1.4  1999/08/15 13:52:52  gerd
 * 	Bugfix: WF_error "Attribute x occurs twice in element [unnamed]"
 * no longer possible; instead of "[unnamed]" the actual name is printed.
 * 	Improved some of the error messages.
 *
 * Revision 1.3  1999/08/15 02:19:01  gerd
 * 	If the DTD allows arbitrary elements, unknown elements are not
 * rejected.
 *
 * Revision 1.2  1999/08/11 14:54:23  gerd
 * 	Optimizations: The hashtable for the 'pinstr' variable is only
 * created on demand. -- The 'only_whitespace' function uses a simple "for"
 * loop is the string is small and a lexer if the string is big.
 *
 * Revision 1.1  1999/08/10 00:35:50  gerd
 * 	Initial revision.
 *
 *
 *)
