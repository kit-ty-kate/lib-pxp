(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

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
    method remove : unit -> unit
    method remove_nodes : ?pos:int -> ?len:int -> unit -> unit
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
    method attributes : (string * att_value) list
    method required_string_attribute : string -> string
    method required_list_attribute : string -> string list
    method optional_string_attribute : string -> string option
    method optional_list_attribute : string -> string list
    method id_attribute_name : string
    method id_attribute_value : string
    method idref_attribute_names : string list
    method quick_set_attributes : (string * att_value) list -> unit
    method set_attributes : (string * att_value) list -> unit
    method set_attribute : ?force:bool -> string -> att_value -> unit
    method reset_attribute : string -> unit
    method attributes_as_nodes : 'ext node list
    method set_comment : string option -> unit
    method comment : string option
    method normprefix : string
    method display_prefix : string
    method localname : string
    method namespace_uri : string
    method namespace_scope : namespace_scope
    method set_namespace_scope : namespace_scope -> unit
    method namespaces_as_nodes : 'ext node list
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
    method create_other : ?position:(string * int * int) ->  
                          dtd -> node_type -> 'ext node
    method local_validate : ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
    method validate_contents : ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
    method complement_attlist : unit -> unit
    method validate_attlist : unit -> unit
    method validate : unit -> unit
    method write : ?prefixes:string list -> 
                   ?default:string ->
                   output_stream -> encoding -> unit
    method display : ?prefixes:(string StringMap.t) -> output_stream -> encoding -> unit
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


(**********************************************************************)
(*
 * CLASS HIERARCHY: 
 *
 * common_node_features
 *   |
 *   +- tree_features
 *   |   |
 *   |   +- leaf_features
 *   |   |    |
 *   |   |    +- data_impl                  pinstr_features
 *   |   |    +- comment_impl                 |
 *   |   |    +- pinstr_impl -----------------+
 *   |   |                                    |
 *   |   +- container_features                |
 *   |        |                               |
 *   |        +- element_impl ----------------+
 *   |        |   |                           |
 *   |        |   +- namespace_element_impl   |
 *   |        |                               |
 *   |        +- super_root_impl--------------+
 *   |
 *   +-- attribute_impl
 *   |     |
 *   |     +- namespace_attribute_impl
 *   |
 *   +-- namespace_impl
 *
 * The classes ending in _impl are real classes that can be
 * instantiated. These classes have all the type 'ext node.
 * The classes ending in _features are virtual only and define
 * some properties that are needed in several classes (mixin classes).
 * There are also (not shown) many classes called no_*_feature
 * that define methods such that a feature is turned off.
 *)

(**********************************************************************)
(* common_node_features                                               *)
(**********************************************************************)

(* These features are inherited by all implementations:
 *
 * - Every node has a DTD
 * - Every node may have a parent
 *)


class virtual ['ext] common_node_features =
object (self) 
  val mutable parent = (None : 'ext node option)
  val mutable node_position = -1
  val mutable dtd = (None : dtd option)

  method delete = self # remove()
  (* Not every class defines [remove]! *)

  (* --------------- DTD -------------------- *)

  method dtd =
    match dtd with
	None -> failwith "Pxp_document.common_node_features#dtd: No DTD available"
      | Some d -> d

  method encoding =
    match dtd with
	None -> failwith "Pxp_document.common_node_features#encoding: No DTD available"
      | Some d -> d # encoding
	  
  (* ---------- PARENT ------------- *)

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
	let n' = n # parent in      (* may raise Not_found *)
	let p = n' # node_position in
	collect n' (p :: path)
      with
	  Not_found ->
	    (* n is the root *)
	    path
    in
    collect (self : 'ext #node :> 'ext node) (self # local_node_path)

  method private local_node_path =
    (* to be overridden *)
    [self # node_position]

  method internal_adopt (new_parent : 'ext node option) pos =
    begin match parent with
	None -> ()
      | Some p ->
	  if new_parent <> None then
	    failwith "Pxp_document.common_node_features#internal_adopt: Tried to add a bound element"
    end;
    parent <- new_parent;
    node_position <- pos

  method internal_set_pos pos =
    node_position <- pos

end
;;


(**********************************************************************)
(* tree_features                                                      *)
(**********************************************************************)

(* All nodes that occur in the regular XML tree have these features:
 *
 * - There is an extension object
 * - The node can be removed
 * - There are siblings
 *)

class virtual ['ext] tree_features an_ext =
  object (self)
    constraint 'ext = 'ext node #extension

    inherit ['ext] common_node_features

    val mutable extension = an_ext

    initializer
      extension # set_node (self : 'ext #node  :> 'ext node)

    method extension = (extension : 'ext)

    method remove () =
      match parent with
	  None -> ()
	| Some p -> p # internal_delete (self : 'ext #node :> 'ext node)

    method previous_node =
      self # parent # nth_node (self # node_position - 1)

    method next_node =
      self # parent # nth_node (self # node_position + 1)

  end
;;

(**********************************************************************)
(* leaf_features                                                      *)
(**********************************************************************)

class virtual ['ext] leaf_features an_ext =
  object (self)
    constraint 'ext = 'ext node #extension

    inherit ['ext] tree_features an_ext

    (* Cloning leaves is very simple: *)

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
  end
;;

(**********************************************************************)
(* no_pinstr_feature                                                  *)
(**********************************************************************)

class virtual ['ext] no_pinstr_feature =
  object (self)
    (* Methods returning constant values: *)

    method pinstr (_:string) : proc_instruction list = []
    method pinstr_names : string list = []

    method add_pinstr (_:proc_instruction) : unit = 
      method_na "add_pinstr"
  end
;; 

(**********************************************************************)
(* no_subnodes_feature                                                *)
(**********************************************************************)

(* These Node types do not implement methods accessing or
 * modifying the subnodes.
 *)

class virtual ['ext] no_subnodes_feature =
  object (self)

    (* Methods returning constant values: *)

    method sub_nodes : 'ext node list = []
    method iter_nodes (_:'ext node -> unit) : unit = ()
    method iter_nodes_sibl 
           (_:'ext node option -> 'ext node -> 'ext node option -> unit) : unit
           = ()
    method nth_node (_:int) : 'ext node = raise Not_found

    (* Unavailable methods: *)

    method set_nodes (_:'ext node list) : unit = 
      method_na "set_nodes"
    method remove_nodes ?(pos:int option) ?(len:int option) () : unit = 
      method_na "remove_nodes"
    method append_node (_:'ext node) : unit = 
      method_na "append_node"
    method insert_nodes ?(pos:int option) (_:'ext node list) : unit = 
      method_na "insert_nodes"
    method add_node ?(force:bool option) (_:'ext node) : unit = 
      method_na "add_node"

    (* Impossible methods: *)

    method internal_delete (_:'ext node) : unit = assert false
  end
;;

(**********************************************************************)
(* no_attributes_feature                                              *)
(**********************************************************************)

(* Node types without attributes do not implement the attribute
 * methods.
 *)

class virtual ['ext] no_attributes_feature =
  object (self)

    (* Methods returning constant values: *)

    method attribute (_:string) : att_value = 
      raise Not_found
    method attribute_names : string list = 
      []
    method attribute_type (_:string) : att_type = 
      raise Not_found
    method attributes : (string * att_value) list = 
      [] 
    method required_string_attribute (_:string) : string =
      failwith "Pxp_document#required_string_attribute: not found"
    method required_list_attribute (_:string) : string list =
      failwith "Pxp_document#required_list_attribute: not found"
    method optional_string_attribute (_:string) : string option = 
      None
    method optional_list_attribute (_:string) : string list = 
      []
    method id_attribute_name : string = 
      raise Not_found
    method id_attribute_value : string = 
      raise Not_found
    method idref_attribute_names : string list = 
      []
    method attributes_as_nodes : 'ext node list = 
      []

    (* Unavailable methods: *)

    method quick_set_attributes (_ : (string * att_value) list) : unit = 
      method_na "quick_set_attributes"
    method set_attributes (_ : (string * att_value) list) : unit = 
      method_na "set_attributes"
    method set_attribute ?(force : bool option) (_:string) (_:att_value) : unit=
      method_na "set_attribute"
    method reset_attribute (_:string) : unit =
      method_na "reset_attribute"

  end
;;

(**********************************************************************)
(* no_validation_feature                                              *)
(**********************************************************************)

(* Implementations that are always valid *)

class virtual ['ext] no_validation_feature =
  object (self)
    method local_validate 
             ?(use_dfa:bool option) ?(check_data_nodes:bool option) () = ()
    method validate_contents 
             ?(use_dfa:bool option) ?(check_data_nodes:bool option) () = ()
    method complement_attlist () = ()
    method validate_attlist () = ()
    method validate () = ()

    method classify_data_node (_:'ext node) : data_node_classification = 
      method_na "classify_data_node"
  end
;;

(**********************************************************************)
(* no_namespace_feature                                               *)
(**********************************************************************)

class virtual ['ext] no_namespace_feature =
  object (self)
    method normprefix : string                    = nsmethod_na "normprefix"
    method display_prefix : string                = nsmethod_na "display_prefix"
    method localname : string                     = nsmethod_na "localname"
    method namespace_uri : string                 = nsmethod_na "namespace_uri"
    method namespace_scope : namespace_scope      = nsmethod_na "namespace_scope"
    method set_namespace_scope : namespace_scope -> unit
      = nsmethod_na "set_namespace_scope"

    method namespaces_as_nodes : 'ext node list 
      = nsmethod_na "namespaces_as_nodes"

    method namespace_manager : namespace_manager
      = nsmethod_na "namespace_manager"
  end
;;

(**********************************************************************)
(* no_comments_feature                                                *)
(**********************************************************************)

class virtual ['ext] no_comments_feature =
  object (self)
    method comment : string option                = method_na "comment"
    method set_comment (_ : string option) : unit = method_na "set_comment"
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


(**********************************************************************)
(* data_impl                                                          *)
(**********************************************************************)

class ['ext] data_impl an_ext : ['ext] node =
  object (self)
    (* Inherited features: *)
    inherit ['ext] leaf_features an_ext
    inherit ['ext] no_subnodes_feature
    inherit ['ext] no_pinstr_feature
    inherit ['ext] no_attributes_feature
    inherit ['ext] no_validation_feature
    inherit ['ext] no_namespace_feature
    inherit ['ext] no_comments_feature

    val mutable content = ("" : string)

    method position = no_position

    method data = content
    method node_type = T_data

    method create_element ?name_pool_for_attribute_values ?position 
                          ?valcheck ?att_values _ _ _ =
      method_na "create_element"

    method create_other ?position _ _ =
      method_na "create_other"

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

    method write ?(prefixes = ([]: string list)) ?default os enc =
      let encoding = self # encoding in
      write_data_string ~from_enc:encoding ~to_enc:enc os content

    method display ?prefixes os enc =
      let encoding = self # encoding in
      write_data_string ~from_enc:encoding ~to_enc:enc os content

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
(* attribute_impl                                                     *)
(**********************************************************************)

class ['ext] attribute_impl ~element ~name value init_dtd : ['ext] node =
  object (self)
    inherit ['ext] common_node_features
    inherit ['ext] no_subnodes_feature
    inherit ['ext] no_pinstr_feature
    inherit ['ext] no_validation_feature
    inherit ['ext] no_namespace_feature
    inherit ['ext] no_comments_feature

    val mutable element_name = element
    val mutable att_name = name
    val mutable att_value = value

    initializer
      dtd <- Some init_dtd

    method private local_node_path =
      (* overrides definition from common_node_features: *)
      [ -1; self # node_position ]

     method orphaned_clone =
       {< parent = None; node_position = -1 >}

     method orphaned_flat_clone =
       {< parent = None; node_position = -1 >}

     method node_type = T_attribute att_name

     method attribute n =
       if n = att_name then att_value else raise Not_found

     method attribute_names = [ att_name ]

     method attribute_type n =
       let eltype = self # dtd # element element_name in
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

    method position = no_position

    method previous_node =
      self # parent # nth_node (self # node_position - 1)

    method next_node =
      self # parent # nth_node (self # node_position + 1)

    (* Non-applicable attribute methods: *)

    method quick_set_attributes _ =    method_na "quick_set_attributes"
    method set_attributes _ =          method_na "set_attributes"
    method set_attribute ?force _ _ =  method_na "set_attribute"
    method reset_attribute _ =         method_na "reset_attribute"
    method attributes_as_nodes =       method_na "attributes_as_nodes"
    method id_attribute_name =         method_na "id_attribute_name"
    method id_attribute_value =        method_na "id_attribute_value"
    method idref_attribute_names =     method_na "idref_attribute_names"

    (* Non-applicable methods: *)

    method remove _ =           method_na "remove"
    method extension =          method_na "extension"
    method set_data _ =         method_na "set_data"
    method internal_init _ _ _ _ _ _ _ = assert false
    method internal_init_other _ _ _   = assert false

    method create_element ?name_pool_for_attribute_values ?position 
                          ?valcheck ?att_values _ _ _ =
                                method_na "create_element"
    method create_data _ _ =    method_na "create_data"
    method create_other ?position _ _ =   method_na "create_other"
    method write ?prefixes ?default _ _ = method_na "write"
    method display ?prefixes _ _        = method_na "display"
  end
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
    | _ -> invalid_arg "Pxp_document.attribute_string_value"
;;


(**********************************************************************)
(* comment_impl                                                       *)
(**********************************************************************)

class [ 'ext ] comment_impl an_ext : ['ext] node =
  object(self)
    (* Inherited features: *)
    inherit ['ext] leaf_features an_ext
    inherit ['ext] no_subnodes_feature
    inherit ['ext] no_pinstr_feature
    inherit ['ext] no_attributes_feature
    inherit ['ext] no_validation_feature
    inherit ['ext] no_namespace_feature

    val mutable position = no_position
    val mutable comment = None

    method node_type = T_comment

    method position = position

    method comment = comment

    method set_comment c = comment <- c

    method data =
      match comment with
	  None   -> raise Not_found
	| Some s -> s

    method write ?prefixes ?default os enc =
      let encoding = self # encoding in
      let wms =
	write_markup_string ~from_enc:encoding ~to_enc:enc os in
      wms ("<!--");
      ( match comment with
	    None   -> ()
	  | Some c -> wms c;
      );
      wms ("-->");

    method display ?prefixes os enc =
      self # write os enc

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      Format.pp_print_string fmt "* T_comment";
      Format.pp_close_box fmt (); 

    method create_element ?name_pool_for_attribute_values ?position 
                          ?valcheck ?att_values _ _ _ =
                                method_na "create_element"
    method create_data _ _ =    method_na "create_data"

    method create_other ?(position = no_position) new_dtd new_ntype =
      if new_ntype <> T_comment then
	failwith "Pxp_document.comment_impl#create_other: bad type";
      let x = extension # clone in
      let obj = ( {< parent = None;
		     node_position = -1;
		     extension = x;
		  >}
	    	    : 'ext #node :> 'ext node
		) in
      x # set_node obj;
      obj # internal_init_other position new_dtd new_ntype;
      obj

    method internal_init new_pos attval_name_pool new_dtd new_name
                         new_attlist =
      assert false

    method internal_init_other new_pos new_dtd new_ntype =
      (* resets the contents of the object *)
      parent <- None;
      node_position <- -1;
      position <- new_pos;
      dtd <- Some new_dtd;
      comment <- None;

    method set_data _ = method_na "set_data"
  end
;;

(**********************************************************************)
(* pinstr_features                                                    *)
(**********************************************************************)

class virtual [ 'ext ] pinstr_features =
  object (self)
    val mutable pinstr = (StringMap.empty : proc_instruction list StringMap.t)

    (* Maps have several advantages:
     * - The empty map does not allocate any memory
     * - The one-element map is very fast (better than hashtable)
     * - Maps are purely functional, and we do not care about them
     *   in clone operations.
     *)

    method add_pinstr pi =
      if pi # encoding <> self # encoding then
	failwith "Pxp_document.pinstr_features # add_pinstr: Inconsistent encodings";
      let name = pi # target in
      let old_list =
	try
	  StringMap.find name pinstr
	with
	    Not_found -> []
      in
      pinstr <- StringMap.add name (old_list @ [pi]) pinstr;
	
    method pinstr name =
      try
	StringMap.find name pinstr
      with
	  Not_found -> []
 	    
    method pinstr_names =
      let nl = ref [] in
      StringMap.iter
	(fun n _ -> nl := n :: !nl)
	pinstr;
      !nl

    (* To be included in subclasses: *)

    method private write_pinstr os enc =
      StringMap.iter
	(fun n pilist ->
	   List.iter (fun pi -> pi # write os enc) pilist
	)
	pinstr;

    method private dump_pinstr fmt =
      StringMap.iter
	(fun _ pilist ->
	   List.iter
	     (fun pi ->
		Format.pp_print_cut fmt ();
		Format.pp_print_string fmt "+ <?";
		Format.pp_print_string fmt (pi # target);
		Format.pp_print_string fmt " ";
		Format.pp_print_string fmt (pi # value);
		Format.pp_print_string fmt "?>"
	     )
	     pilist
	)
	pinstr
  end
;;

(**********************************************************************)
(* pinstr_impl                                                        *)
(**********************************************************************)

class [ 'ext ] pinstr_impl an_ext : ['ext] node =
  object(self)
    (* Inherited features: *)
    inherit ['ext] leaf_features an_ext
    inherit ['ext] pinstr_features as super
    inherit ['ext] no_subnodes_feature
    inherit ['ext] no_comments_feature
    inherit ['ext] no_attributes_feature
    inherit ['ext] no_validation_feature
    inherit ['ext] no_namespace_feature

    val mutable position = no_position
    val mutable pinstr_name = ""

    method node_type = T_pinstr pinstr_name

    method position = position

    method data =
       match self # pinstr pinstr_name with
	   [ pi ] -> pi # value
	 | _      -> assert false

    method write ?prefixes ?default os enc =
      self # write_pinstr os enc

    method display ?prefixes os enc =
      self # write_pinstr os enc

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      Format.pp_print_string fmt "* T_pinstr \"";
      Format.pp_print_string fmt pinstr_name;
      Format.pp_print_string fmt "\"";
      self # dump_pinstr fmt;
      Format.pp_close_box fmt (); 

    method add_pinstr pi =
      (* Overrides the definition in pinstr_features:
       * fail if applied more than once 
       *)
      if pinstr <> StringMap.empty then
	failwith "Pxp_document.pinstr_impl # add_pinstr: the node can only contain one processing instruction";
      super # add_pinstr pi

    method create_element ?name_pool_for_attribute_values ?position 
                          ?valcheck ?att_values _ _ _ =
                                method_na "create_element"
    method create_data _ _ =    method_na "create_data"

    method create_other ?(position = no_position) new_dtd new_ntype =
      ( match new_ntype with
	    T_pinstr _ -> ()
	  | _ ->
	      failwith "Pxp_document.pinstr_impl#create_other: bad type";
      );
      let x = extension # clone in
      let obj = ( {< parent = None;
		     node_position = -1;
		     extension = x;
		  >}
	    	    : 'ext #node :> 'ext node
		) in
      x # set_node obj;
      obj # internal_init_other position new_dtd new_ntype;
      obj

    method internal_init new_pos attval_name_pool new_dtd new_name
                         new_attlist =
      assert false

    method internal_init_other new_pos new_dtd new_ntype =
      (* resets the contents of the object *)
      parent <- None;
      node_position <- -1;
      position <- new_pos;
      dtd <- Some new_dtd;
      pinstr <- StringMap.empty;
      (match new_ntype with
	   T_pinstr n -> pinstr_name <- n
	 | _          -> assert false
      )

    method set_data _ = method_na "set_data"
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



(**********************************************************************)

let flag_ext_decl = 1;;
    (* Whether the element is externally declared *)

(* REMOVED in PXP 1.1: *)
(* let flag_keep_always_whitespace = 2;; *)
    (* Whether the "keep whitespace mode" is on *)


type 'a list_or_array =
    LA_not_available
  | LA_list of 'a list
  | LA_array of 'a array
;;
(* Perhaps we need also the hybrid LA_list_array storing both representations.
 *)


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


(**********************************************************************)
(* container_features                                                 *)
(**********************************************************************)

class virtual ['ext] container_features an_ext =
  (* For elements and super root nodes *)
    object (self:'self)
      inherit ['ext] tree_features an_ext as super

      val mutable rev_nodes = ([] : 'c list)
      val mutable nodes = LA_not_available
      val mutable size = 0
      val mutable position = no_position

      method position = position

      method append_node n =
	(* general DTD check: *)
	begin match dtd with
	    None -> ()
	  | Some d -> if n # dtd != d then
	      failwith "Pxp_document.container_features # append_node: the sub node has a different DTD";
	end;
	(* Add the node: *)
	n # internal_adopt (Some (self : 'ext #node :> 'ext node)) size;
	rev_nodes <- n :: rev_nodes;
	nodes <- LA_not_available;
	size <- size + 1

      method add_node ?(force = false) n =
	(* compatibility with PXP 1.0: *)
	if force then
	  self # append_node n
	else
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

      method remove_nodes ?(pos = 0) ?(len = size) () =
	if pos < 0 || len < 0 || pos + len > size then 
	  invalid_arg "remove_nodes: ~pos or ~len";
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
	(* For every node in nl must hold:
	 * (a) the node is already a child of this node, or
	 * (b) the node is a root node
	 *)
	List.iter
	  (fun n ->
	     try
	       let parent = n # parent in
	       if parent != (self : 'ext #node :> 'ext node) then
		 failwith "Pxp_document.container_features # set_nodes: node is already a member of another tree";
	     with
		 Not_found -> 
		   ()
	  )
	  nl;
	(* All ok. We set now the parent of the old subnodes to None,
	 * then the parent of the new subnodes to self.
	 *)
	List.iter
	  (fun n -> n # internal_adopt None (-1))
	  rev_nodes;
	let k = ref 0 in
	List.iter
	  (fun n -> n # internal_adopt
	                  (Some (self : 'ext #node :> 'ext node))
	                  !k;
	            incr k
	  )
	  nl;
	size <- !k;
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
    end
;;

(**********************************************************************)
(* element_impl                                                       *)
(**********************************************************************)

(*------------------------- attributes -------------------------------*)

type 'ext attlist =
  | No_atts
  | Atts of (att_value array *                     (* declared attribs *)
	     att_value StringMap.t)                (* undeclared attribs *)
  | Atts_with_nodes of ((att_value * 'ext node) array * 
			(att_value * 'ext node) StringMap.t)
;;

(* - Declared attributes:
 *   They are simply in an array; the index of the array element is the position
 *   vr.att_lookup contains for the attribute. 
 *   As an exception of the rule, the array may be empty indicating that
 *   currently all attributes are treated as undeclared attributes.
 * - Undeclared attributes:
 *   They are contained in a StringMap.t mapping from attribute names to
 *   attribute values.
 *)


let att_assoc vr n l =
  let assoc a m =
    try
      if a = [| |] then raise Not_found;
      let k = Str_hashtbl.find vr.att_lookup n in
      a.(k)
    with
	Not_found ->
	  StringMap.find n m
  in
  match l with
      No_atts               -> raise Not_found
    | Atts (a,m)            -> assoc a m
    | Atts_with_nodes (a,m) -> fst(assoc a m)
;;


let set_att ?(force=false) vr l n v =
  (* Does not work with Atts_with_nodes *)
  match l with
      No_atts -> 
	Atts ( [| |], StringMap.add n v StringMap.empty )
    | Atts_with_nodes(_,_) -> assert false
    | Atts(a,m) ->
	try
	  if a = [| |] then raise Not_found;
	  let k = Str_hashtbl.find vr.att_lookup n in
	  a.(k) <- v;
	  l
	with
	    Not_found ->
	      if not force then
		( if not(StringMap.mem n m) then
		    failwith "Pxp_document # set_attribute: no such attribute";
		);
	      Atts(a, StringMap.add n v m)
	  
;;


let reset_att vr l n =
  (* Does not work with Atts_with_nodes *)
  match l with
      No_atts -> 
	failwith "Pxp_document # reset_attribute: no such attribute";
    | Atts_with_nodes(_,_) -> assert false
    | Atts(a,m) ->
	try
	  if a = [| |] then raise Not_found;
	  let k = Str_hashtbl.find vr.att_lookup n in
	  let (_,v) = vr.init_att_vals.(k) in
	  a.(k) <- v;
	  l
	with
	    Not_found ->
	      ( if not(StringMap.mem n m) then
		  failwith "Pxp_document # reset_attribute: no such attribute";
	      );
	      Atts(a, StringMap.remove n m)
;;



let stringmap_to_list f m =
  let l = ref [] in
  StringMap.iter
    (fun n v -> l := f n v :: !l)
    m;
  !l
;;


let array_to_list vr f a =
  let l = ref [] in
  for k = Array.length a - 1 downto 0 do
    let n = fst vr.init_att_vals.(k) in
    let v = a.(k) in
    l := f n v :: !l
  done;
  !l
;;


let attlist_to_list vr f l =
  match l with
      No_atts -> []
    | Atts (a,m) ->
	array_to_list vr f a @ stringmap_to_list f m
    | Atts_with_nodes (a,m) ->
	let f' n (v,_) = f n v in
	array_to_list vr f' a @ stringmap_to_list f' m
;;


let attlist_iter vr f l =
  let iter f a m =
    Array.iteri
      (fun k v ->
	 let n = fst vr.init_att_vals.(k) in
	 f n v
      )
      a;
    StringMap.iter f m
  in
  match l with
      No_atts               -> ()
    | Atts (a,m)            -> iter f a m
    | Atts_with_nodes (a,m) -> iter (fun n (v,_) -> f n v) a m
;;


let attlist_of_list l =
  let m = ref StringMap.empty in
  List.iter
    (fun (n,v) ->
       m := StringMap.add n v !m
    )
    l;
  Atts( [| |], !m)
;;


let attlist_with_nodes vr f l =
  match l with
      No_atts -> No_atts
    | Atts(a,m) ->
	let a' = Array.mapi
		   (fun k v ->
		      let n = fst vr.init_att_vals.(k) in
		      let node = f n v k in
		      (v,node)
		   )
		   a in
	let k = ref (Array.length a) in
	let m' = StringMap.mapi
		   (fun n v ->
		      let node = f n v !k in
		      incr k;
		      (v,node)
		   )
		   m in
	Atts_with_nodes(a',m')
    | Atts_with_nodes(_,_) -> assert false
;;


let nodes_of_attlist vr l =
  match l with
      No_atts      -> []
    | Atts (_,_)   -> assert false
    | Atts_with_nodes (a,m) -> 
	let f n (v,node) = node in
	array_to_list vr f a @ stringmap_to_list f m
;;


let attlist_without_nodes l =
  match l with
      No_atts      -> No_atts
    | Atts (_,_)   -> l
    | Atts_with_nodes (a,m) -> 
	let a' = Array.map fst a in
	let m' = StringMap.map fst m in
	Atts(a',m')
;;


let attlist_has_nodes l =
  match l with
      Atts_with_nodes (_,_) -> true
    | _ -> false
;;

(*------------------------- validation -------------------------------*)

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

let only_whitespace error_name s =
  (* Checks that the string "s" contains only whitespace. On failure,
   * Validation_error is raised.
   *)
  if not (Pxp_lib.only_whitespace s) then
    raise(Validation_error(error_name() ^
			   " must not have character contents"));
  ()
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

(*--------------------------------------------------------------------*)

(* Make that some slots are visible in namespace_element_impl: *)

class type [ 'ext ] element_node =
  object ('self)
    inherit ['ext] node

    val mutable attributes : 'ext attlist
    val mutable vr : validation_record

    method private get_nsdecls : string list -> (string * att_value) list
    method private get_nsname : string -> string -> string
    method private make_attribute_node : 
                     string -> string -> att_value -> dtd -> 'ext node

  end
;;


class [ 'ext ] element_impl an_ext (* : ['ext] element_node *) =
  object(self)
    inherit ['ext] container_features an_ext
    inherit ['ext] pinstr_features
    inherit ['ext] no_comments_feature
    inherit ['ext] no_namespace_feature

    val mutable vr = no_validation
    val mutable flags = 0
        (* bit string of flags; see the values flag_* above *)
    val mutable ntype = T_none
    val mutable attributes = No_atts

    method node_type = ntype

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      ( match ntype with
	    T_none -> 
	      Format.pp_print_string fmt "* T_none";
	  | T_element n -> 
	      Format.pp_print_string fmt "* T_element \"";
	      Format.pp_print_string fmt n;
	      Format.pp_print_string fmt "\"";
	  | _ ->
	      assert false
	);
	attlist_iter
	  vr
	  (fun n v ->
	     Format.pp_print_cut fmt ();
	     Format.pp_print_string fmt n;
	     Format.pp_print_string fmt "=";
	     format_att_value fmt v;
	  )
	  attributes;
	if attlist_has_nodes attributes then begin
	  List.iter
	    (fun n ->
	       Format.pp_print_cut fmt ();
	       n # dump fmt;
	    )
	    (nodes_of_attlist vr attributes);
	end;
	self # dump_pinstr fmt;
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

      method private error_name =
	match ntype with
	    T_element n -> "Element `" ^ n ^ "'"
	  | _ -> assert false

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

      method attributes =
	attlist_to_list vr (fun an ax -> (an,ax)) attributes

      method attribute n =
	att_assoc vr n attributes

      method attribute_names =
	attlist_to_list vr (fun an _ -> an) attributes

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
	  match att_assoc vr n attributes with
	      Value s -> s
	    | Valuelist l -> String.concat " " l
	    | Implied_value -> raise Not_found
	with
	    Not_found ->
	      failwith "Pxp_document, method required_string_attribute: not found"

      method optional_string_attribute n =
	try
	  match att_assoc vr n attributes with
	      Value s -> Some s
	    | Valuelist l -> Some (String.concat " " l)
	    | Implied_value -> None
	with
	    Not_found ->
	      None

      method required_list_attribute n =
	try
	  match att_assoc vr n attributes with
	      Value s -> [ s ]
	    | Valuelist l -> l
	    | Implied_value -> raise Not_found
	with
	    Not_found ->
	      failwith "Pxp_document, method required_list_attribute: not found"

      method optional_list_attribute n =
	try
	  match att_assoc vr n attributes with
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
	      begin match att_assoc vr name attributes (* may raise Not_found *)
	      with
		  Value s -> s
		| _ -> raise Not_found
	      end


      method idref_attribute_names = vr.idref_att_names


      method quick_set_attributes atts =
	attributes <- attlist_of_list atts;

      method set_attributes atts =
	attributes <- attlist_of_list atts;

      method set_attribute ?force n v =
	attributes <- set_att  ?force vr (attlist_without_nodes attributes) n v

      method reset_attribute n =
        attributes <- reset_att vr (attlist_without_nodes attributes) n

      method private make_attribute_node element_name att_name value dtd =
	(* to be overridden *)
	new attribute_impl 
	       ~element:element_name
	       ~name:att_name
	       value
	       dtd

      method attributes_as_nodes =
	match attributes with
	  | No_atts ->
	      []
	  | Atts (_,_) ->
	      let dtd = self # dtd in
	      let element_name =
		match ntype with
		    T_element n -> n
		  | _ ->
		      assert false in
	      let atts' =
		attlist_with_nodes
		  vr
		  (fun n v pos ->
		     let a =
		       self # make_attribute_node element_name n v dtd in
		     a # internal_adopt 
		           (Some (self : 'ext #node :> 'ext node)) pos;
		     a
		  )
		  attributes 
	      in
	      attributes <- atts';
	      nodes_of_attlist vr atts'
	  | _ ->
	      nodes_of_attlist vr attributes


      method create_element
	               ?name_pool_for_attribute_values
                       ?(position = no_position) 
		       ?(valcheck = true)
		       ?(att_values = [])
		       new_dtd new_type new_attlist =
	let x = extension # clone in
	let obj = ( {< parent = None;
		       extension = x;
		       pinstr = StringMap.empty;
		    >}
	    	    : 'ext #node :> 'ext node
		  ) in
	(* It is ok that obj does not reset rev_nodes because the init_*
	 * methods do that.
	 *)
	x # set_node obj;
	match new_type with
	  | T_element name ->
	      obj # internal_init
                position name_pool_for_attribute_values
		valcheck
                new_dtd name new_attlist att_values;
	      obj
	  | T_none ->
	      (* a special case to make create_no_node work *)
	      obj # internal_init
                position None false new_dtd "_xxx_" [] [];
	      obj
	  | _ ->
	      failwith "create_element: Cannot create such node"

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
       * DECLARED ATTRIBUTES:
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
       * UNDECLARED ATTRIBUTES:
       *
       * These are simply collected in a StringMap which is also useful
       * for detecting duplicates.
       *
       * MERGE RESULTS:
       *
       * Finally, init_att_vals and extra_att_vals are merged.
       *)

      method internal_init new_pos attval_name_pool 
                           valcheck_element_exists
                           new_dtd new_name
                           new_attlist new_attvalues =
	(* resets the contents of the object *)
	parent <- None;
	node_position <- -1;
	rev_nodes <- [];
	nodes <- LA_not_available;
	size <- 0;
	ntype <- T_element new_name;
	position <- new_pos;
	dtd <- Some new_dtd;
	pinstr <- StringMap.empty;
	
	let lfactory = new_dtd # lexer_factory in
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

	let undeclared_atts = ref StringMap.empty in
	let add_undeclared_att n v =
	  if StringMap.mem n !undeclared_atts then
	    raise (WF_error("Attribute `" ^ n ^ "' occurs twice"))
	  else
	    undeclared_atts := StringMap.add n v !undeclared_atts
	in
	let add_undeclared_att_string n s =
	  add_undeclared_att n (mk_pool_value(Value s)) in
	let add_undeclared_att_pair (n,v) =
	  add_undeclared_att n v in
	let add_undeclared_att_string_pair (n,s) =
	  add_undeclared_att n (mk_pool_value(Value s)) in

	let att_vals =
	  (* att_vals: the array of declared attributes or [| |] *)
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

	    (* init_att_vals, cur_att_vals: about declared attributes only *)

	    let init_att_vals = vr.init_att_vals in
	        (* contains pairs (att_name, att_default_value) *)

	    let cur_att_vals  = Array.map snd init_att_vals in
	        (* contains current value *)

	    let m = Array.length init_att_vals in

	    if m > 0 then begin
	      (* round 1 *)
	      let att_found = Array.create m false in
	          (* whether the declared attribute has been found *)
	      (* First iterate over new_attlist, then over new_attvalues: *)
	      (* new_attlist: *)
	      List.iter
		(fun (att_name, att_val) ->
		   let bad = ref false in
		   (* if !bad = true, Not_found must not happen *)
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
				lfactory new_dtd att_name att_type att_val in
		     let v = mk_pool_value v0 in
		     if att_fixed then begin
		       let v' = cur_att_vals.(k) in
		       if v <> v' then
			 raise
			   (Validation_error
			      ("Attribute `" ^ att_name ^
			       "' is fixed, but has here a different value"));
		     end
		     else begin
		       cur_att_vals.(k) <- v
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
			 add_undeclared_att_string att_name att_val;
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
		       lfactory new_dtd att_name att_type v;

		     (* Check: Only atts flagged as #IMPLIED
		      * can be set to Implied_value
		      *)

		     if v = Implied_value then begin
		       let d = cur_att_vals.(k) in
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
		       let v' = cur_att_vals.(k) in
		       if v <> v' then
			 raise
			   (Validation_error
			      ("Attribute `" ^ att_name ^
			       "' is fixed, but has here a different value"));
		     end
		     else begin
		       cur_att_vals.(k) <- v
		     end;

		     att_found.(k) <- true;

		   with
		       Not_found ->
			 assert(not !bad);
			 (* Raised by Hashtbl.find *)
			 add_undeclared_att att_name v;
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
	    else begin
	      (* m = 0 *)
	      List.iter add_undeclared_att_string_pair new_attlist;
	      List.iter add_undeclared_att_pair        new_attvalues;
	    end;

	    cur_att_vals    (* result *)
	  with
	      Undeclared ->
		(* raised by #element *)
		vr <- no_validation;
		List.iter add_undeclared_att_string_pair new_attlist;
		List.iter add_undeclared_att_pair        new_attvalues;
		[| |]
	in

	if !undeclared_atts <> StringMap.empty &&
	   not vr.accept_undeclared_atts 
	then begin
	  raise (Validation_error
		   ("The following attributes are not declared: " ^
		    String.concat ", "
		      (stringmap_to_list (fun n v -> n) !undeclared_atts)))
	end;

	attributes <- Atts(att_vals, !undeclared_atts)


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


    method complement_attlist () =
      (* Iterate over init_att_vals of the validation record vr. Add every
       * missing attribute to the current set of attributes.
       *)
      self # update_vr;
      let old_atts = attlist_without_nodes attributes in
      let new_array, new_map = 
	match old_atts with
	    No_atts   -> [| |], ref StringMap.empty
	  | Atts(a,m) -> a, ref m
	  | _         -> assert false
      in
      Array.iter
	(fun (name,value) ->
	   try
	     ignore(att_assoc vr name old_atts)
	   with
	       Not_found ->
		 new_map := StringMap.add name value !new_map;
	)
	vr.init_att_vals;
      attributes <- Atts(new_array, !new_map)


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

    method validate () =
      self # validate_attlist();
      self # validate_contents ~use_dfa:true ();  (* Use DFA if available *)
      
    method private get_nsdecls prefixes =
      (* to be overridden *)
      []

    method private get_nsname name default =
      (* to be overridden *)
      name

    method write ?(prefixes = ([] : string list)) ?default os enc =
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
      let nsdefault = 
	match default with
	    None -> []
	  | Some d ->
	      if List.mem "" prefixes then
		[]
	      else
		let d_uri = self # namespace_manager # get_primary_uri d in
		[Value d_uri]
      in

      let name' =
	match ntype with
	    T_element name ->
	      (	match default with
		    Some d -> self # get_nsname name d
		  | None   -> name 
	      )
	  | _ -> assert false
      in
      wms ("<" ^ name');
      attlist_iter vr (write_att "") attributes;
      List.iter   (fun (n,v) -> write_att "xmlns:" n v) nsdecls;
      List.iter   (fun (  v) -> write_att "" "xmlns" v) nsdefault;
      wms "\n>";

      self # write_pinstr os enc;

      let prefixes' = (List.map fst nsdecls) @ prefixes in
      let prefixes'' =
	if nsdefault <> [] then "" :: prefixes' else prefixes' in

      List.iter
	(fun n -> n # write ?prefixes:(Some prefixes'') ?default os enc)
	(self # sub_nodes);

      wms ("</" ^ name' ^ "\n>");

    method display ?prefixes os enc =
      (* Overriden in namespace_element_impl, so this is only for the
       * non-namespace case:
       *)
      self # write os enc

    method internal_init_other new_pos new_dtd new_ntype =
      method_na "internal_init_other"

    method set_data _ = method_na "set_data"
    method create_other ?position _ _ =   method_na "create_other"
    method create_data _ _ = method_na "create_data"
  end
;;

(**********************************************************************)
(* super_root_impl                                                    *)
(**********************************************************************)

class [ 'ext ] super_root_impl an_ext : ['ext] node =
  object(self)
    inherit ['ext] container_features an_ext
    inherit ['ext] pinstr_features
    inherit ['ext] no_comments_feature
    inherit ['ext] no_namespace_feature
    inherit ['ext] no_attributes_feature
    inherit ['ext] no_validation_feature

    method node_type = T_super_root

    method create_other
                       ?(position = no_position) 
		       new_dtd new_ntype =
      let x = extension # clone in
      let obj = ( {< parent = None;
		     extension = x;
		  >}
	    	    : 'ext #node :> 'ext node
		) in
      (* It is ok that obj does not reset rev_nodes because the init_*
       * methods do that.
       *)
      x # set_node obj;
      match new_ntype with
	| (T_super_root) ->
	    obj # internal_init_other position new_dtd new_ntype;
	    obj
	| _ ->
	    failwith "create_other: Cannot create such node"


    method internal_init_other new_pos new_dtd new_ntype =
      (* resets the contents of the object *)
      parent <- None;
      node_position <- -1;
      rev_nodes <- [];
      nodes <- LA_not_available;
      size <- 0;
      position <- new_pos;
      dtd <- Some new_dtd;
      pinstr <- StringMap.empty

    (* TODO: Super root nodes are always roots *)

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      Format.pp_print_string fmt "* T_super_root";
      self # dump_pinstr fmt;
      List.iter
	(fun n ->
	   Format.pp_print_cut fmt ();
	   n # dump fmt;
	)
	(List.rev rev_nodes);
      Format.pp_close_box fmt (); 

    method write ?prefixes ?default os enc =
      let encoding = self # encoding in
      let wms =
	write_markup_string ~from_enc:encoding ~to_enc:enc os in
      self # write_pinstr os enc;
      List.iter
	(fun n -> n # write ?prefixes ?default os enc)
	(self # sub_nodes);

    method display ?prefixes os enc =
      let encoding = self # encoding in
      let wms =
	write_markup_string ~from_enc:encoding ~to_enc:enc os in
      self # write_pinstr os enc;
      List.iter
	(fun n -> n # display ?prefixes os enc)
	(self # sub_nodes);

    method create_element ?name_pool_for_attribute_values ?position 
                          ?valcheck ?att_values _ _ _ =
                                method_na "create_element"
    method create_data _ _ = method_na "create_data"

    method internal_init new_pos attval_name_pool new_dtd new_name
                         new_attlist =
      method_na "internal_init"

    method set_data _ =         method_na "set_data"
  end
;;

(**********************************************************************)

class ['ext] namespace_attribute_impl ~element ~name value dtd =
   object (self)
    inherit [ 'ext ] attribute_impl ~element ~name value dtd as super
      (* Note: Inheriting from an *_impl class can be problematic
       * as not all methods and/or values may be visible
       *)

    val mutable normprefix = ""
    val mutable localname = ""

    initializer
      let (p,l) = namespace_split name in
      normprefix <- p;
      localname  <- l;

    method normprefix = normprefix
    method localname = localname

    method display_prefix =
      self # parent # namespace_scope # display_prefix_of_normprefix normprefix
      
    method namespace_uri = 
      self # namespace_manager # get_primary_uri normprefix

    method namespace_scope =
      self # parent # namespace_scope

    method set_namespace_scope x =
      method_na "set_namespace_scope"

    method namespaces_as_nodes = 
      []

    method namespace_manager =
      self # dtd # namespace_manager

  end
;;

(**********************************************************************)
(* namespace_impl                                                     *)
(**********************************************************************)

class [ 'ext ] namespace_impl srcprefix normprefix init_dtd : ['ext] node =
  object (self)
    inherit ['ext] common_node_features
    inherit ['ext] no_attributes_feature
    inherit ['ext] no_subnodes_feature
    inherit ['ext] no_pinstr_feature
    inherit ['ext] no_comments_feature
    inherit ['ext] no_validation_feature 

    val normprefix = normprefix
    val srcprefix = srcprefix
		       
    initializer
      dtd <- Some init_dtd

    method private local_node_path =
      (* Overrides definition in common_node_features: *)
      [ -2; self # node_position ]
      
    method orphaned_clone =
      {< parent = None; node_position = -1 >}
      
    method orphaned_flat_clone =
      {< parent = None; node_position = -1 >}
      
    method node_type = T_namespace srcprefix
			 
    method data = 
      (self # namespace_manager) # get_primary_uri normprefix
      
    method normprefix = 
      normprefix
      (* CHECK in the light of new namespace impl *)
      (* This is a hack to ensure whenever there is no srcprefix we will
       * not have a normprefix, either.
       * However, there may be a namespace URI
       *)
      (*
      if srcprefix = "" then
	""
      else
	normprefix
      *)
	  
    method display_prefix =
      srcprefix

    method namespace_uri = 
      (* XPath requires this to be null: *)
      raise Not_found
	
    method namespace_manager =
      self # dtd # namespace_manager

    method namespace_scope =
      self # parent # namespace_scope

    method namespaces_as_nodes = 
      []

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      Format.pp_print_string fmt "+ T_namespace";
      Format.pp_print_cut fmt ();
      Format.pp_print_string fmt "normprefix=";
      Format.pp_print_string fmt (self # normprefix);
      Format.pp_print_cut fmt ();
      Format.pp_print_string fmt "display prefix=";
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

     method position = no_position

    (* Non-applicable methods: *)

     method extension =          method_na "extension"
     method internal_init _ _ _ _ _ _ _ =   method_na "internal_init"
     method internal_init_other _ _ _ = method_na "internal_init_other"
     method set_data _ =         method_na "set_data"
     method set_namespace_scope _ = method_na "set_namespace_scope"
     method create_element ?name_pool_for_attribute_values ?position 
                           ?valcheck ?att_values _ _ _ =
                                 method_na "create_element"
     method create_data _ _ =    method_na "create_data"
     method create_other ?position _ _ = method_na "create_other"
     method write ?prefixes ?default _ _ = method_na "write"
     method display ?prefixes _ _        = method_na "display"
     method localname =          method_na "localname"
     method previous_node =      method_na "previous_node"
     method next_node =          method_na "next_node"
     method remove _ =           method_na "remove"
  end
;;

let namespace_normprefix n =
  match n # node_type with
      T_namespace _ -> n # normprefix
    | _ -> invalid_arg "Pxp_document.namespace_normprefix"
;;

let namespace_display_prefix n =
  match n # node_type with
      T_namespace _ -> n # display_prefix
    | _ -> invalid_arg "Pxp_document.namespace_display_prefix"
;;

let namespace_uri n =
  match n # node_type with
      T_namespace _ -> n # data  (* sic! *)
    | _ -> invalid_arg "Pxp_document.namespace_uri"
;;


(**********************************************************************)
(* namespace_element_impl                                             *)
(**********************************************************************)

class [ 'ext ] namespace_element_impl an_ext =
  object (self)
    inherit [ 'ext ] element_impl an_ext as super
      (* Note: Inheriting from an *_impl class can be problematic
       * as not all methods and/or values may be visible
       *)

    val mutable normprefix = ""
    val mutable localname = ""
    val mutable scope = None
    val mutable nsnodes = None

    method normprefix = normprefix
    method localname = localname
    method namespace_uri = 
      (* IDEA: Map Not_found to Namespace_not_declared *)
      self # namespace_manager # get_primary_uri normprefix

    method display_prefix =
      self # namespace_scope # display_prefix_of_normprefix normprefix

    method namespace_scope =
      match scope with
	  None   -> 
	    ( let empty_scope =
		new namespace_scope_impl self#namespace_manager None [] in
	      scope <- Some empty_scope;
	      empty_scope
	    )
	| Some x -> x

    method set_namespace_scope x =
      let m = self#namespace_manager in
      if m <> x # namespace_manager then
	failwith "set_namespace_scope: Invalid namespace manager";
      scope <- Some x;
      nsnodes <- None;  (* force recomputation *)

    method namespace_manager =
      self # dtd # namespace_manager

    method namespaces_as_nodes =
      match nsnodes with
	  None ->
	    let dtd = self#dtd in
	    let m = self#namespace_manager in
	    let s = self#namespace_scope in
	    let l1 = s#effective_declaration in  (* pairs (dsp_prefix, uri) *)
	    let l2 = 
	      List.map
		(fun (dsp_prefix, uri) ->
		   let norm_prefix = 
		     try m#get_normprefix uri 
		     with Not_found -> "<unknown>" (* CHECK *) in
		   new namespace_impl dsp_prefix norm_prefix dtd
		)
		l1 in
	    nsnodes <- Some l2;
	    l2
	| Some l ->
	    l

    method internal_init new_pos attval_name_pool valcheck_element_exists
                         new_dtd new_name
                         new_attlist new_attvalues =

      super # internal_init
	new_pos attval_name_pool valcheck_element_exists new_dtd new_name 
	        new_attlist new_attvalues;

      let (p,l) = namespace_split new_name in
      normprefix <- p;
      localname  <- l;
      (* TODO: Use pools *)
      (* CHECK: It is possible to create elements with non-existing 
       * normprefixes. This may cause errors later (namespace URI not
       * found). Maybe it is better to catch this case here.
       *)

    method private get_nsname name default =
      (* Overrides the definition in element_impl *)
      (* If the prefix of [name] is [default], strip the prefix: *)
      let prefix, localname = namespace_split name in
      if prefix = default then
	localname
      else
	name

    method private get_nsdecls prefixes =
      (* Overrides the definition in element_impl *)
      (* This method modifies the behaviour of 'write'. In 'prefixes' the
       * list of already printed namespace declarations is passed to this 
       * method. The task is to check whether additional declarations are
       * necessary and to pass them back as list of pairs (normprefix, uri).
       *)
      let scan_att name value =  (* return prefix of attribute *)
	extract_prefix name
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
	(attlist_to_list vr scan_att attributes)
      in
      let prefixes' = add_prefixes prefixes p_candidates in
      let mng = self # namespace_manager in

      List.map
	(fun p ->  p, Value (mng # get_primary_uri p) )
	prefixes'

    method private make_attribute_node element_name att_name value dtd =
      (* This method modifies the behaviour of attributes_as_nodes *)
      new namespace_attribute_impl 
	~element:element_name
	~name:att_name
	value
	dtd

    method display ?(prefixes = StringMap.empty) os enc =
      let encoding = self # encoding in
      let wms =
	write_markup_string ~from_enc:encoding ~to_enc:enc os in
      
      (* Get the required declarations: *)
      let mng = self # namespace_manager in
      let scope = self # namespace_scope in
      let eff_decl = scope # effective_declaration in
      let eff_decl_to_add =
	(* The prefixes in [eff_decl] that are not in [prefixes] *)
	List.filter
	  (fun (dp, uri) ->
	     try
	       StringMap.find dp prefixes <> uri
	     with
		 Not_found -> true
	  )
	  eff_decl in
      let eff_decl_to_add' = ref [] in    (* further prefixes *)
      
      let prefixes' =
	ref (List.fold_left 
	       (fun acc (dp, uri) ->
		  StringMap.add dp uri acc)
	       prefixes
	       eff_decl_to_add) in
      
      let search_prefix uri =
	(* Slow! *)
	let p =
	  StringMap.fold
	    (fun _p _uri y -> if _uri = uri then _p else y)
	    !prefixes'
	    "" in
	if p = "" then raise Not_found;
	p
      in

      let invent_new_prefix uri =
	let n = ref 0 in
	while StringMap.mem ("ns" ^ string_of_int !n) !prefixes' do
	  incr n
	done;
	let p = "ns" ^ string_of_int !n in
	prefixes' := StringMap.add p uri !prefixes';
	eff_decl_to_add' := (p, uri) :: !eff_decl_to_add';
	p
      in
      
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
	
      let write_att_remap aname avalue =
	let (p,local) = namespace_split aname in
	if p = "" then
	  write_att "" aname avalue
	else
	  let d = 
	    try scope # display_prefix_of_normprefix p 
	    with Namespace_not_in_scope _ -> 
	      (* Display prefix is missing. This is an error, but we
	       * can search or invent a new prefix on the fly.
	       *)
	      ( let uri = mng # get_primary_uri p in
		try 
		  search_prefix uri
		with
		    Not_found ->
		      invent_new_prefix uri )
	  in
	  write_att (d ^ ":") local avalue
      in
	
      let this_display_prefix = 
	try self # display_prefix
	with Namespace_not_in_scope _ -> 
	  (* Display prefix is missing. This is an error, but we
	   * can search or invent a new prefix on the fly.
	   *)
	  ( let uri = mng # get_primary_uri self#normprefix in
	    try 
	      search_prefix uri
	    with
		Not_found ->
		  invent_new_prefix uri )
      in
      let this_localname = self # localname in
      
      let name =
	if this_display_prefix = "" then
	  this_localname  (* within default namespace *)
	else
	  this_display_prefix ^ ":" ^ this_localname in
      
      wms ("<" ^ name);
      attlist_iter vr write_att_remap attributes;
      List.iter   
	(fun (n,v) -> 
	   if n = "" then
	     write_att "" "xmlns" (Value v)
	   else
	     write_att "xmlns:" n (Value v))
	(eff_decl_to_add @ !eff_decl_to_add');
      wms "\n>";
      
      super # write_pinstr os enc;
      
      List.iter
	(fun n -> n # display ?prefixes:(Some !prefixes') os enc)
	(self # sub_nodes);
      
      wms ("</" ^ name ^ "\n>");
      
  end
;;

(**********************************************************************)

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
		x # create_other ?position:position dtd T_super_root
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
		let e = x # create_other ?position:position dtd T_comment
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
    exemplar # create_other ?position:position dtd (T_pinstr target) in
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

(**********************************************************************)

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
    let n' = pre n in
    ( match n' # node_type with
	  T_element _ 
	| T_super_root ->
	    let children = n # sub_nodes in
	    let children' = map_children children in
	    n' # set_nodes children';
	| _ -> ()
    );
    post n'
  and map_children l =
    match l with
	[] -> []
      | child :: l' ->
	  (try
	     let child' = map_rec child in
	     child' :: map_children l'
	   with
	       Skip ->
		 map_children l'
	  )
  in
  try map_rec base with Skip -> raise Not_found
;;


let map_tree_sibl ~pre ?(post=(fun _ x _ -> x)) base =
  let rec map_rec l n r =
    let n' = pre l n r in
    ( match n' # node_type with
	  T_element _ 
	| T_super_root ->
	    let children = n # sub_nodes in
	    let children' = map_children None children in
	    let children'' = postprocess_children None children' in
	    n' # set_nodes children'';
	| _ -> ()
    );
    n'
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
		Skip ->
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
  try 
    let base' = map_rec None base None in
    post None base' None
  with Skip -> raise Not_found
;;


let iter_tree ?(pre=(fun x -> ())) ?(post=(fun x -> ())) base =
  let rec iter_rec n =
    pre n;
    let children = n # sub_nodes in
    iter_children children;
    post n
  and iter_children l =
    match l with
	[] -> []
      | child :: l' ->
	  (try
	     iter_rec child;
	     iter_children l'
	   with
	       Skip ->
		 iter_children l'
	  )
  in
  try
    iter_rec base
  with
      Skip -> ()
;;


let iter_tree_sibl ?(pre=(fun _ _ _ -> ())) ?(post=(fun _ _ _ -> ())) base =
  let rec iter_rec l n r =
    pre l n r;
    let children = n # sub_nodes in
    iter_children None children;
    post l n r
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
		Skip ->
		  iter_children (Some child) l'
	   )
    )
  in
  try
    iter_rec None base None
  with
      Skip -> ()
;;

(**********************************************************************)

let validate tree =
  iter_tree
    ~pre:(fun n -> n # validate())
    tree
;;

(**********************************************************************)

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

(**********************************************************************)

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
    else begin
      (* It is possible that there is a sub node that says again
       * xml:space = "default".
       *)
      n # iter_nodes
	(fun sub ->
	   match sub # node_type with
	       T_element _ ->
		 strip_elements sub preserve_space'
	     | _ ->
		 ()
	)
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

(**********************************************************************)

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

(**********************************************************************)
(* document                                                           *)
(**********************************************************************)

class ['ext] document ?swarner the_warner enc =
  object (self)
    inherit ['ext] pinstr_features

    val mutable xml_version = "1.0"
    val mutable dtd = (None : dtd option)
    val mutable root = (None : 'ext node option)
    val mutable raw_root_name = ""
    val encoding = (enc : rep_encoding)

    val warner = (the_warner : collect_warnings)
    val swarner = (swarner : symbolic_warnings option)

    method init_xml_version s =
      if s <> "1.0" then
	warn swarner warner (`W_XML_version_not_supported s);
      xml_version <- s

    method init_root r real_root_element_name =
      let dtd_r = r # dtd in
      if dtd_r # encoding <> encoding then
	failwith "Pxp_document.document#init_root: encoding mismatch";

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
		    if real_root_element_name <> declared_root_element_name then
		      raise
			(Validation_error ("The root element is `" ^
					   real_root_element_name ^
					   "' but is declared as `" ^
					   declared_root_element_name ^ "'"));
		| None -> ()
	    end;
	    (* All is okay, so store dtd and root node: *)
	    dtd <- Some dtd_r;
	    root <- Some r;
	    raw_root_name <- real_root_element_name;

	(**************** CASE: No super root element **********************)

	| T_element root_element_name ->
	    if not (dtd_r # arbitrary_allowed) then begin
	      match dtd_r # root with
		  Some declared_root_element_name ->
		    if real_root_element_name <> declared_root_element_name then
		      raise
			(Validation_error ("The root element is `" ^
					   real_root_element_name ^
					   "' but is declared as `" ^
					   declared_root_element_name ^ "'"))
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
	    root <- Some r;
	    raw_root_name <- real_root_element_name;

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

    method encoding = encoding

    method root =
      match root with
	  None -> failwith "Pxp_document.document#root: Document has no root element"
	| Some r -> r

    method raw_root_name = 
      match root with
	  None -> failwith "Pxp_document.document#raw_root_name: Document has no root element"
	| Some _ -> raw_root_name

    method write ?default ?(prefer_dtd_reference = false) os enc =
      let encoding = self # encoding in
      let wms =
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      let r = self # root in
      wms ("<?xml version='1.0' encoding='" ^
	   Netconversion.string_of_encoding enc ^
	   "'?>\n");

      begin match dtd with
	  None -> ()
	| Some d ->
	    if prefer_dtd_reference &&
	      ( match d # id with
		    Some (External _) -> true
		  | _ -> false
	      )
	    then
	      d # write_ref os enc
	    else
	      d # write os enc true;
      end;

      self # write_pinstr os enc;
      r # write ?default os enc;
      wms "\n";

    method display ?(prefer_dtd_reference = false) os enc =
      let encoding = self # encoding in
      let wms =
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      let r = self # root in
      wms ("<?xml version='1.0' encoding='" ^
	   Netconversion.string_of_encoding enc ^
	   "'?>\n");

      begin match dtd with
	  None -> ()
	| Some d ->
	    if prefer_dtd_reference &&
	      ( match d # id with
		    Some (External _) -> true
		  | _ -> false
	      )
	    then
	      d # write_ref os enc
	    else
	      d # write os enc true;
      end;

      self # write_pinstr os enc;
      r # display os enc;
      wms "\n";

    method dump fmt =
      Format.pp_open_vbox fmt 2;
      Format.pp_print_string fmt "* document";
      self # dump_pinstr fmt;
      (match root with
	   None -> ()
	 | Some r ->
	     Format.pp_print_cut fmt ();
	     r # dump fmt
      );
      Format.pp_close_box fmt ();


  end
;;

(**********************************************************************)

let print_node (n : 'ext node) =
  n # dump (Format.std_formatter)
;;


let print_doc (n : 'ext document) =
  n # dump (Format.std_formatter)
;;


(**********************************************************************)

exception Error_event of exn


type state =
    Null
  | Start_seen   (* Start tag seen *)
  | End_seen     (* End tag seen *)
  | NA           (* Not applicable: Start/end tag cannot occur any longer *)
;;


type 'ext solid_xml =
    [ `Node of 'ext node
    | `Document of 'ext document
    ]
;;


let solidify ?dtd cfg spec next_ev : 'ext solid_xml =
  let eff_dtd =
    ref (match dtd with 
	     Some d ->  
	       if d#encoding <> cfg.encoding then
		 failwith "Pxp_document.solidify: Encoding mismatch";
	       d
	   | None   -> 
	       let d = new dtd ?swarner:cfg.swarner cfg.warner cfg.encoding in
	       d # allow_arbitrary;
	       ( match cfg.enable_namespace_processing with
		     Some m -> d # set_namespace_manager m;
		   | None -> ()
	       );
	       d) in
  
  let return = ref None in
  let return_doc = ref None in
  let stack = Stack.create() in
  let depth = ref 0 in           (* Stack depth, without super root node *)
  let doc_state = ref Null in
  let super_state = ref Null in
  let root_state = ref Null in
  let pos = ref None in
  let eof = ref false in

  let unexpected txt =
    failwith ("Pxp_document.solidify: Unexpected " ^ txt ^ " event")
  in

  while not !eof do
    let ev = next_ev() in
    match ev with
	Some (E_start_doc(xml_version,found_dtd)) ->
	  if !doc_state <> Null then unexpected "E_start_doc";
	  doc_state := Start_seen;
	  pos := None;
	  let doc = 
	    new document ?swarner:cfg.swarner cfg.warner cfg.encoding in
	  doc # init_xml_version xml_version;
	  if dtd = None then (
	    if found_dtd#encoding <> cfg.encoding then
	      failwith "Pxp_document.solidify: Encoding mismatch";
	    eff_dtd := found_dtd;
	  );
	  return_doc := Some doc;

      | Some (E_end_doc lit_root) ->
	  if ((!doc_state <> Start_seen) ||
	      (!super_state = Start_seen) ||
	      (!root_state <> End_seen)) then unexpected "T_end_doc";
	  doc_state := End_seen;
	  pos := None;
	  assert(Stack.is_empty stack);
	  (match (!return, !return_doc) with
	     | (Some r, Some r_doc) -> 
		 r_doc # init_root r lit_root;
	     | _ ->
		 assert false
	  );

      | Some E_start_super ->
	  if !super_state <> Null then unexpected "E_start_super";
	  if !doc_state = Null then doc_state := NA;
	  super_state := Start_seen;
	  if cfg.enable_super_root_node then (
	    let n = create_super_root_node ?position:!pos spec !eff_dtd in
	    Stack.push n stack;
	  )

      | Some E_end_super ->
	  if (!super_state <> Start_seen || !root_state <> End_seen) then
	    unexpected "E_end_super";
	  super_state := End_seen;
	  if  cfg.enable_super_root_node then (
	    let n = Stack.pop stack in
	    assert(Stack.is_empty stack);
	    assert(n#node_type = T_super_root);
	    return := Some n;
	  )

      | Some (E_start_tag(name,atts,scope_opt,eid)) ->
	  let is_root = (!depth = 0) in
	  if is_root then (
	    if !root_state <> Null then unexpected "E_start_tag";
	    root_state := Start_seen;
	    if !doc_state = Null then doc_state := NA;
	    if !super_state = Null then super_state := NA;
	  );
	  let n = create_element_node 
		    ?name_pool_for_attribute_values:
		    (if cfg.enable_name_pool_for_attribute_values
		     then Some cfg.name_pool
		     else None)
		    ?position:!pos
		    spec !eff_dtd name atts in
	  ( match scope_opt with
		None -> ()
	      | Some scope -> n # set_namespace_scope scope
	  );
	  ( try
	      let parent = Stack.top stack in
	      parent # append_node n
	    with
		Stack.Empty ->
		  return := Some n
	  );
	  Stack.push n stack;
	  incr depth;
	  pos := None

      | Some (E_end_tag(name,eid)) ->
	  decr depth;
	  let is_root = (!depth = 0) in
	  if is_root then (
	    if !root_state <> Start_seen then unexpected "E_end_tag";
	    root_state := End_seen;
	  );
	  if Stack.is_empty stack then unexpected "E_end_tag";
	  let top = Stack.pop stack in
	  ( match top # node_type with
		T_element _ -> ()
	      | _ -> unexpected "E_end_tag"
	  );
	  if not cfg.disable_content_validation then
	    top # validate_contents 
	      ~use_dfa:cfg.validate_by_dfa ~check_data_nodes:true ();
	  pos := None

      | Some (E_char_data data) ->
	  if !root_state <> Start_seen then unexpected "E_char_data";
	  ( try
	      let n = create_data_node spec !eff_dtd data in
	      (Stack.top stack) # append_node n
	    with
		Stack.Empty -> assert false
	  );
	  pos := None

      | Some (E_pinstr(target,value)) ->
	  (* A PI may occur everywhere between start_doc and end_doc.  *)
	  if !doc_state = End_seen then unexpected "E_pinstr";
	  if !doc_state = Null then doc_state := NA;
	  let pi = new proc_instruction target value !eff_dtd#encoding in
	  if !depth = 0 && (!super_state <> Start_seen || 
			    not cfg.enable_super_root_node) then (
	    (* Cannot process E_pinstr here. Should be E_pinstr_member. *)
	    ()
	  )
	  else 
	    if cfg.enable_pinstr_nodes then (
	      let n = create_pinstr_node ?position:!pos spec !eff_dtd pi in
	      (Stack.top stack) # append_node n
	    )
	    else (
	      (* Cannot process E_pinstr here. Should be E_pinstr_member. *)
	      ()
	    );
	  pos := None

      | Some (E_pinstr_member(target,value)) ->
	  if !doc_state = End_seen then unexpected "E_pinstr";
	  if !doc_state = Null then doc_state := NA;
	  let pi = new proc_instruction target value !eff_dtd#encoding in
	  if !depth = 0 && (!super_state <> Start_seen || 
			    not cfg.enable_super_root_node) then (
	    (* Add processing instruction to document, if any *)
	    match !return_doc with
		Some doc -> doc # add_pinstr pi
	      | None -> ()  (* PI is lost *)
	  )
	  else 
	    if not cfg.enable_pinstr_nodes then (
	      (Stack.top stack) # add_pinstr pi
	    );
	  pos := None
	  
      | Some (E_comment data) ->
	  (* A comment may occur everywhere between start_doc and end_doc. 
	   * Only below the super root or the simple root node it is 
	   * accepted, however.
	   *)
	  if !doc_state = End_seen then unexpected "E_comment";
	  if !doc_state = Null then doc_state := NA;
	  if (cfg.enable_super_root_node && !super_state = Start_seen) ||
	     (!root_state = Start_seen) then ( 
	    try
	      if cfg.enable_comment_nodes then begin
		let n = create_comment_node ?position:!pos spec !eff_dtd data in
		(Stack.top stack) # append_node n
	      end
	    with
		Stack.Empty -> ()
	  );
	  pos := None

      | Some (E_position(ent,line,colpos)) ->
	  pos := Some (ent,line,colpos)

      | Some (E_error err) ->
	  raise(Error_event err)

      | Some E_end_of_stream
      | None ->
	  if (!doc_state = Start_seen) ||
	     (!super_state = Start_seen) ||
	     (!root_state <> End_seen) then
	       unexpected "E_end_of_stream/actual end"
	  pos := None;
	  eof := (ev = None)
  done;
  ( match !return, !return_doc with
	_, Some doc -> `Document doc
      | Some n, None -> `Node n
      | _ -> assert false
  )
;;


type 'ext flux_state = 
    [ `Node_start of 'ext node 
    | `Node_end of 'ext node 
    | `Output of (event list * 'ext flux_state)
    | `EOS 
    | `None 
    ]


class dummy = object end ;;


let liquefy_node ?(omit_end = false) ?(omit_positions = false) 
                 (init_fstate : 'ext flux_state) =
  let fstate = ref init_fstate in
  let eid = new dummy in
  let rec generate arg =
    match !fstate with
	`Node_start n ->
	  (* Find next node: *)
	  let fstate' =
	    ( match n#sub_nodes with
		  [] ->
		    `Node_end n
		| n' :: _ ->
		    `Node_start n'
	    ) in
	  fstate := fstate';
	  (* Do action for n: *)
	  ( match n # node_type with 
		T_element name ->
		  let atts =
		    List.flatten
		      (List.map
			 (fun (n,v) ->
			    match v with
				Value s -> [n, s]
			      | Valuelist l -> [n, String.concat " " l]
			      | Implied_value -> []
			 )
			 n # attributes) in
		  let scope_opt =
		    try
		      Some(n # namespace_scope)
		    with
			Namespace_method_not_applicable _ -> None in
		  let (entity,line,colpos) = n # position in
		  let pos = E_position(entity,line,colpos) in
		  let tag = E_start_tag(name, atts, scope_opt, eid) in
		  let out = 
		    if omit_positions then [ tag ] else [ pos; tag ] in
		  let out_pinstr =
		    List.flatten
		      (List.map
			 (fun target ->
			    List.map
			      (fun pi ->
				  E_pinstr_member(target,pi#value)
			      )
			      (n # pinstr target)
			 )
			 n # pinstr_names
		      )
		  in
		  fstate := `Output(out @ out_pinstr, !fstate);
		  generate arg
	      | T_data ->
		  Some(E_char_data(n # data))
	      | T_super_root ->
		  Some E_start_super
	      | T_pinstr target -> 
		  let (entity,line,colpos) = n # position in
		  let pos = E_position(entity,line,colpos) in
		  let value = (List.hd (n # pinstr target)) # value in
		  let ev = E_pinstr(target,value) in
		  let out =
		    if omit_positions then [ ev ] else [ pos; ev ] in
		  fstate := `Output(out, !fstate);
		  generate arg
	      | T_comment ->
		  let (entity,line,colpos) = n # position in
		  let pos = E_position(entity,line,colpos) in
		  let s = try n # data with Not_found -> "" in
		  let ev = E_comment s in
		  let out =
		    if omit_positions then [ ev ] else [ pos; ev ] in
		  fstate := `Output(out, !fstate);
		  generate arg
	      | _ ->
		  failwith "Pxp_document.liquefy_node: Unexpected node type"
	  );

      | `Node_end n ->
	  let fstate' =
	    ( try
		`Node_start(n # next_node)
	      with
		  Not_found -> 
		    ( try 
			`Node_end(n # parent)
		      with
			  Not_found ->
			    if omit_end then `None else `EOS
		    )
	    ) in
	  fstate := fstate';
	  (* Do action for n: *)
	  ( match n # node_type with 
		T_element name ->
		  Some(E_end_tag(name,eid))
	      | T_super_root ->
		  Some E_end_super
	      | _ ->
		  generate arg
	  );

      | `Output(events, fstate') ->
	  ( match events with
		ev :: events' ->
		  fstate := `Output(events', fstate');
		  Some ev
	      | [] ->
		  fstate := fstate';
		  generate arg
	  )

      | `EOS ->
	  fstate := `None;
	  Some E_end_of_stream

      | `None ->
	  None

  in
  generate
;;


let liquefy_doc ?(omit_end = false) ?(omit_positions = false) 
                (doc : 'ext document) =
  let fstate = ref `Start in
  let rec generate arg =
    match !fstate with
	`Start ->
	  let out_pinstr =
	    List.flatten
	      (List.map
		 (fun target ->
		    List.map
		    (fun pi ->
		       E_pinstr_member(target,pi#value)
		    )
		    (doc # pinstr target)
		 )
		 doc # pinstr_names
	      )
	  in
	  let node_fstate =
	    `Output(out_pinstr, `Node_start(doc#root)) in
	  fstate := `Nodes 
	               (liquefy_node ~omit_end:true ~omit_positions node_fstate);
	  Some(E_start_doc(doc#xml_version,doc#dtd))
      | `Nodes g ->
	  let e = g arg in
	  if e = None then (
	    fstate := `End;
	    generate arg
	  )
	  else e
      | `End ->
	  fstate := if omit_end then `None else `EOS;
	  Some(E_end_doc doc#raw_root_name)
      | `EOS ->
	  fstate := `None;
	  Some E_end_of_stream
      | `None ->
	  None
  in
  generate
;;


let liquefy ?omit_end ?omit_positions solid =
  match solid with
      `Node n     -> liquefy_node ?omit_end ?omit_positions (`Node_start n)
    | `Document d -> liquefy_doc ?omit_end ?omit_positions d
;;
