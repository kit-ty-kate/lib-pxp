(* $Id: pxp_document.ml,v 1.1 2000/05/29 23:48:38 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_lexer_types
open Pxp_dtd
open Pxp_aux


exception Skip

type node_type =
    T_element of string
  | T_data
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
    method parent : 'ext node
    method root : 'ext node
    method orphaned_clone : 'self
    method orphaned_flat_clone : 'self
    method add_node : 'ext node -> unit
    method add_pinstr : proc_instruction -> unit
    method pinstr : string -> proc_instruction list
    method pinstr_names : string list
    method sub_nodes : 'ext node list
    method iter_nodes : ('ext node -> unit) -> unit
    method iter_nodes_sibl :
      ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
    method set_nodes : 'ext node list -> unit
    method data : string
    method node_type : node_type
    method attribute : string -> att_value
    method attribute_names : string list
    method attribute_type : string -> att_type
    method attributes : (string * Pxp_types.att_value) list
    method required_string_attribute : string -> string
    method required_list_attribute : string -> string list
    method optional_string_attribute : string -> string option
    method optional_list_attribute : string -> string list
    method quick_set_attributes : (string * Pxp_types.att_value) list -> unit
    method find : string -> 'ext node
    method reset_finder : unit
    method dtd : dtd
    method create_element :
      dtd -> node_type -> (string * string) list -> 'ext node
    method create_data : dtd -> string -> 'ext node
    method local_validate : unit
    method keep_always_whitespace_mode : unit
    method write_compact_as_latin1 : output_stream -> unit
    method internal_adopt : 'ext node option -> unit
    method internal_delete : 'ext node -> unit
    method internal_init : dtd -> string -> (string * string) list -> unit
  end
;;

(**********************************************************************)

exception Found;;

let validate_content model (el : 'a node) =
  (* checks that the nodes of 'el' matches the DTD. Returns 'true'
   * on success and 'false' on failure.
   *)

  (* Handling of "-pi": These elements are just ignored everywhere, as if
   * they did not exist.
   *)

  let rec is_empty cl =
    (* Whether the node list counts as empty or not. *)
    match cl with
	[] -> true
      | n :: cl' ->
	  ( match n # node_type with
		T_element "-pi" -> is_empty cl'    (* ignore such an element *)
	      | T_data          -> is_empty cl'    (* ignore data nodes *)
	      | _               -> false
	  )
  in

  let rec run_regexp cl ml =
    (* cl:   the list of children that will have to be matched
     * ml:   the list of regexps that will have to match (to be read as
     *       sequence)
     * returns () meaning that no match has been found, or raises Found.
     *)
    match ml with
	[] ->
	  if cl = [] then raise Found;      (* Frequent case *)
	  if is_empty cl then raise Found;  (* General condition *)
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
		  | T_element "-pi" ->              (* Ignore this element *)
		      run_regexp cl' ml
		  | T_element nt ->
		      if nt = chld then run_regexp cl' ml'
		end
  in

  match model with
      Unspecified -> true
    | Any -> true
    | Empty ->
	let cl = el # sub_nodes in
	is_empty cl 
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
		 T_data -> ()
	       | T_element name ->
		   if not (List.mem name mix') then begin
		     match name with
			 "-pi" -> ()                 (* Ignore this element *)
		       | _     -> raise Not_found
		   end
	    );
	  true
	with
	    Not_found ->
	      false
	end
    | Regexp re ->
	let cl = el # sub_nodes in
	begin try
	  run_regexp cl [re];
	  false
	with
	    Found -> true
	end
    | _ -> assert false
;;

(**********************************************************************)


class virtual ['ext] node_impl an_ext =
  object (self)
    constraint 'ext = 'ext node #extension

    val mutable parent = (None : 'ext node option)
    val mutable dtd = (None : dtd option)
    val mutable extension = an_ext

    initializer
      extension # set_node (self : 'ext #node  :> 'ext node)


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

    method orphaned_clone =
      let x = extension # clone in
      let n =
	{< parent = None;
	   extension = x;
	>} in
      x # set_node (n : 'ext #node  :> 'ext node);
      n

    method orphaned_flat_clone =
      let x = extension # clone in
      let n =
	{< parent = None;
	   extension = x;
	>} in
      x # set_node (n : 'ext #node  :> 'ext node);
      n

    method dtd =
      match dtd with
	  None -> raise Not_found
	| Some d -> d

    method internal_adopt (new_parent : 'ext node option) =
      begin match parent with
	  None -> ()
	| Some p ->
	    if new_parent <> None then
	      failwith "Pxp_document.node_impl#internal_adopt: Tried to add a bound element"
      end;
      parent <- new_parent


    method virtual add_node : 'ext node -> unit
    method virtual add_pinstr : proc_instruction -> unit
    method virtual sub_nodes : 'ext node list
    method virtual pinstr : string -> proc_instruction list
    method virtual pinstr_names : string list
    method virtual iter_nodes : ('ext node -> unit) -> unit
    method virtual iter_nodes_sibl : ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
    method virtual set_nodes : 'ext node list -> unit
    method virtual data : string
    method virtual node_type : node_type
    method virtual attribute : string -> att_value
    method virtual attribute_names : string list
    method virtual attribute_type : string -> att_type
    method virtual attributes : (string * Pxp_types.att_value) list
    method virtual required_string_attribute : string -> string
    method virtual required_list_attribute : string -> string list
    method virtual optional_string_attribute : string -> string option
    method virtual optional_list_attribute : string -> string list
    method virtual quick_set_attributes : (string * Pxp_types.att_value) list -> unit
    method virtual find : string -> 'ext node
    method virtual reset_finder : unit
    method virtual create_element : dtd -> node_type -> (string * string) list -> 'ext node
    method virtual create_data : dtd -> string -> 'ext node
    method virtual keep_always_whitespace_mode : unit
    method virtual write_compact_as_latin1 : output_stream -> unit
    method virtual local_validate : unit
    method virtual internal_delete : 'ext node -> unit
    method virtual internal_init : dtd -> string -> (string * string) list -> unit
  end
;;


(**********************************************************************)


class ['ext] data_impl an_ext str : ['ext] node =
  object (self)
    inherit ['ext] node_impl an_ext
    val mutable content = (str : string)

    method find id =
      if parent = None then
	raise Not_found
      else
	self # root # find id

    method reset_finder =
      self # root # reset_finder
    method add_node _ =
      failwith "method 'add_node' not applicable to data node"
    method add_pinstr _ =
      failwith "method 'add_pinstr' not applicable to data node"
    method pinstr _ = []
    method pinstr_names = []
    method sub_nodes = []
    method iter_nodes _ = ()
    method iter_nodes_sibl _ = ()
    method set_nodes _ =
      failwith "method 'set_nodes' not applicable to data node"
    method data = content
    method node_type = T_data
    method attribute _ = raise Not_found
    method attribute_names = []
    method attribute_type _ = raise Not_found
    method attributes = []
    method required_string_attribute _ =
      failwith "Markup.document, method required_string_attribute: not found"
    method required_list_attribute _ =
      failwith "Markup.document, method required_list_attribute: not found"
    method optional_string_attribute _ = None
    method optional_list_attribute _ = []
    method quick_set_attributes _ =
      failwith "method 'quick_set_attributes' not applicable to data node"
    method create_element _ _ _ =
      failwith "method 'create_element' not applicable to data node"
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
    method local_validate = ()
    method keep_always_whitespace_mode = ()


    method write_compact_as_latin1 os =
      write_data_string os content

	
    method internal_delete _ =
      assert false
    method internal_init _ _ _ =
      assert false
  end
;;


(**********************************************************************)


class ['ext] element_impl an_ext : ['ext] node =
    object (self:'self)
      inherit ['ext] node_impl an_ext as super

      val mutable content_model = Any
      val mutable ext_decl = false
      val mutable name = "[unnamed]"
      val mutable rev_nodes = ([] : 'c list)
      val mutable nodes = (None : 'c list option)
      val mutable attributes = []
      val pinstr = lazy (Hashtbl.create 10 : (string,proc_instruction) Hashtbl.t)
      val mutable id_table = None
      val mutable keep_always_whitespace = false

      method attributes = attributes

      method internal_adopt new_parent =
	super # internal_adopt new_parent;
	id_table <- None

      method reset_finder =
	if parent = None then
	  id_table <- None
	else
	  self # root # reset_finder

      method find id =
	if parent = None then begin (* this is the root element *)
	  let t =
	    begin match id_table with
		None   ->
		  (* create the ID hash table *)
		  let new_t = Hashtbl.create 100 in
		  let rec enter n =
		    let anames = n # attribute_names in
		    List.iter
		      (fun aname ->
			 let atype = n # attribute_type aname in
			 if atype = A_id then begin
			   let aval  = n # attribute aname in
			   match aval with
			       Value aval_str ->
				 if Hashtbl.mem new_t aval_str then
				   raise
				     (Validation_error("Element ID `" ^ aval_str ^ "' is not unique"));
				 Hashtbl.add new_t aval_str n
			     | Implied_value -> ()
			     | _ -> ()
			 end)
		      anames;
		    List.iter enter (n # sub_nodes);
		  in
		  enter (self : 'ext #node :> 'ext node);
		  id_table <- Some new_t;
		  new_t
	      | Some t -> t
	    end
	  in
	  Hashtbl.find t id
	end
	else self # root # find id

      method add_node n =
	let only_whitespace s =
	  (* Checks that the string "s" contains only whitespace. On failure,
	   * Validation_error is raised.
	   *)
	  let l = String.length s in
	  if l < 100 then begin
	    for i=0 to l - 1 do  (* for loop is faster for small 'l' *)
	      match s.[i] with
		  ('\009'|'\010'|'\013'|'\032') -> ()
		| _ ->
		    raise(Validation_error("Element `" ^ name ^ "' must not have character contents"));
	    done
	  end
	  else begin
	    let lexbuf = Lexing.from_string s in
	    let lexerset = Pxp_lexers.get_lexer_set (self # dtd # encoding) in
	    let t = lexerset.scan_name_string lexbuf in
	    if t <> Ignore or
	      (lexerset.scan_name_string lexbuf <> Eof)
	    then
	      raise(Validation_error("Element `" ^ name ^ "' must not have character contents"));
	    ()
	  end
	in
	(* general DTD check: *)
	begin match dtd with
	    None -> ()
	  | Some d -> if n # dtd != d then
	      failwith "Pxp_document.element_impl # add_node: the sub node has a different DTD";
	end;
	(* specific checks: *)
	try
	  begin match n # node_type with
	      T_data ->
		begin match content_model with
		    Any         -> ()
		  | Unspecified -> ()
		  | Empty       -> 
		      if n # data <> "" then
			raise(Validation_error("Element `" ^ name ^ "' must be empty"));
		      raise Skip
		  | Mixed _     -> ()
		  | Regexp _    -> 
		      only_whitespace (n # data);
		      (* TODO: following check faster *)
		      if n # dtd # standalone_declaration &&
		         n # data <> ""
		      then begin
			(* The standalone declaration is violated if the
			 * element declaration is contained in an external
			 * entity.
			 *)
			if ext_decl then
			  raise
			    (Validation_error
			       ("Element `" ^ name ^ 
				"' violates standalone declaration"  ^
				" because extra white space separates" ^ 
				" the sub elements"));
		      end;
		      if not keep_always_whitespace then raise Skip
		end
	    | T_element nt ->
		()
	  end;
	  (* all OK, so add this node: *)
	  n # internal_adopt (Some (self : 'ext #node :> 'ext node));
	  rev_nodes <- n :: rev_nodes;
	  nodes <- None
	with Skip ->
	  ()

      method add_pinstr pi =
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

      method sub_nodes =
	match nodes with
	    None ->
	      let cl = List.rev rev_nodes in
	      nodes <- Some cl;
	      cl
	  | Some cl ->
	      cl

      method iter_nodes f =
	let cl = self # sub_nodes in
	List.iter f cl

      method iter_nodes_sibl f =
	let cl = self # sub_nodes in
	let rec next last_node l =
	  match l with
	      [] -> ()
	    | [x] ->
		f last_node x None
	    | x :: y :: l' ->
		f last_node x (Some y);
		next (Some x) l'
	in
	next None cl

      method set_nodes nl =
	List.iter
	  (fun n -> n # internal_adopt None)
	  rev_nodes;
	begin try
	  List.iter
	    (fun n -> n # internal_adopt (Some (self : 'ext #node :> 'ext node)))
	    nl
	with
	    e ->
	      (* revert action as much as possible *)
	      List.iter
		(fun n -> n # internal_adopt None)
		rev_nodes;
	      List.iter
		(fun n -> n # internal_adopt (Some (self : 'ext #node :> 'ext node)))
		rev_nodes;
	      (* [TODO] Note: there may be bad members in nl *)
	      raise e
	end;
	rev_nodes <- List.rev nl;
	nodes <- None


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
	     extension = x;
	     rev_nodes = sub_clones;
	     nodes = None;
	  >} in	

	List.iter
	  (fun m -> m # internal_adopt (Some (n : 'ext #node :> 'ext node)))
	  sub_clones;

	x # set_node (n : 'ext #node  :> 'ext node);
	n

      method orphaned_flat_clone : 'self =
	let x = extension # clone in
	let n =
	  {< parent = None;
	     extension = x;
	     rev_nodes = [];
	     nodes = None;
	  >} in	

	x # set_node (n : 'ext #node  :> 'ext node);
	n


      method internal_delete n =
	rev_nodes <- List.filter (fun n' -> n' != n) rev_nodes;
	nodes <- None;
	n # internal_adopt None

      method data =
	let cl = self # sub_nodes in
	String.concat "" (List.map (fun n -> n # data) cl)

      method node_type = T_element name

      method attribute n =
	List.assoc n attributes

      method attribute_names =
	List.map fst attributes


      method attribute_type n =
	let d =
	  match dtd with
	      None -> failwith "attribute_type not available without DTD"
	    | Some d -> d
	in
	let eltype = d # element name in
	try
	  let atype, adefault = eltype # attribute n in
	  atype
	with
	    Undeclared ->
	      A_cdata


      method required_string_attribute n =
	try
	  match List.assoc n attributes with
	      Value s -> s
	    | Valuelist l -> String.concat " " l
	    | Implied_value -> raise Not_found
	with
	    Not_found ->
	      failwith "Markup.document, method required_string_attribute: not found"

      method optional_string_attribute n =
	try
	  match List.assoc n attributes with
	      Value s -> Some s
	    | Valuelist l -> Some (String.concat " " l)
	    | Implied_value -> None
	with
	    Not_found ->
	      None

      method required_list_attribute n =
	try
	  match List.assoc n attributes with
	      Value s -> [ s ]
	    | Valuelist l -> l
	    | Implied_value -> raise Not_found
	with
	    Not_found ->
	      failwith "Markup.document, method required_list_attribute: not found"

      method optional_list_attribute n =
	try
	  match List.assoc n attributes with
	      Value s -> [ s ]
	    | Valuelist l -> l
	    | Implied_value -> []
	with
	    Not_found ->
	      []

      method quick_set_attributes atts =
	attributes <- atts


      method create_element new_dtd new_type new_attlist =
	let x = extension # clone in
	let obj = ( {< parent = None;
		       extension = x;
		       pinstr = lazy (Hashtbl.create 10)
		    >}
	    	    : 'ext #node :> 'ext node
		  ) in
	x # set_node obj;
	match new_type with
	    T_data ->
	      failwith "Cannot create T_data node"
	  | T_element name ->
	      obj # internal_init new_dtd name new_attlist;
	      obj

      method internal_init new_dtd new_name new_attlist =
	(* resets the contents of the object *)
	parent <- None;
	rev_nodes <- [];
	nodes <- None;
	name <- new_name;
	(* Hashtbl.clear pinstr; *)

	let lexerset = Pxp_lexers.get_lexer_set (new_dtd # encoding) in
	let sadecl = new_dtd # standalone_declaration in

	(* First validate the element name and the attributes: *)
	(* Well-Formedness Constraint: Unique Att Spec *)
	let rec check_uniqueness al =
	  match al with
	      [] -> ()
	    | (n, av) :: al' ->
		if List.mem_assoc n al' then
		  raise (WF_error("Attribute `" ^ n ^ "' occurs twice in element `" ^ name ^ "'"));
		check_uniqueness al'
	in
	check_uniqueness new_attlist;
	(* Validity Constraint: Element Valid [element has been declared] *)
	try
	  let eltype = new_dtd # element new_name in
	  content_model <- eltype # content_model;
	  ext_decl <- eltype # externally_declared;
	  (* Validity Constraint: Attribute Value Type *)
	  (* Validity Constraint: Fixed Attribute Default *)
	  (* Validity Constraint: Standalone Document Declaration (partly) *)
	  let new_attlist' =
	    List.map
	      (fun (n,v) ->
		 try
		   (* Get type, default, and the normalized attribute
		    * value 'av':
		    *)
		   let atype, adefault = eltype # attribute n in
		   let av = value_of_attribute lexerset new_dtd n atype v in
		   (* If necessary, check whether normalization violates
		    * the standalone declaration.
		    *)
		   if sadecl &&
                      eltype # 
		        attribute_violates_standalone_declaration n (Some v)
		   then
		     raise
		       (Validation_error
			  ("Attribute `" ^ n ^ "' of element type `" ^
			   name ^ "' violates standalone declaration"));
		   (* If the default is "fixed", check that. *)
		   begin match adefault with
		       (D_required | D_implied) -> ()
		     | D_default _ -> ()
		     | D_fixed u ->
			 let uv = value_of_attribute 
                                         lexerset new_dtd "[default]" atype u in
			 if av <> uv then
			   raise
			     (Validation_error
				("Attribute `" ^ n ^ 
				 "' is fixed, but has here a different value"));
		   end;
		   n,av
		 with
		     Undeclared ->
		       (* raised by method "# attribute" *)
		       n, value_of_attribute lexerset new_dtd n A_cdata v
	      )
	      new_attlist in
	  (* Validity Constraint: Required Attribute *)
	  (* Validity Constraint: Standalone Document Declaration (partly) *)
	  (* Add attributes with default values *)
	  let new_attlist'' =
	    List.map
	      (fun n ->
		 try
		   n, List.assoc n new_attlist'
		 with
		     Not_found ->
		       (* Check standalone declaration: *)
		       if sadecl &&
			    eltype # 
			    attribute_violates_standalone_declaration
			    n None then
			 raise
			   (Validation_error
			      ("Attribute `" ^ n ^ "' of element type `" ^
			       name ^ "' violates standalone declaration"));
		       (* add default value or Implied *)
		       let atype, adefault = eltype # attribute n in
		       match adefault with
			   D_required ->
			     raise(Validation_error("Required attribute `" ^ n ^ "' is missing"))
			 | D_implied ->
			     n, Implied_value
			 | D_default v ->
			     n, value_of_attribute lexerset new_dtd n atype v
			 | D_fixed v ->
			     n, value_of_attribute lexerset new_dtd n atype v
	      )
	      (eltype # attribute_names)
	  in
	  dtd <- Some new_dtd;
	  attributes <- new_attlist'';
	with
	    Undeclared ->
	      (* The DTD allows arbitrary attributes/contents for this
	       * element
	       *)
	      dtd <- Some new_dtd;
	      attributes <- List.map (fun (n,v) -> n, Value v) new_attlist;
	      content_model <- Any;

      method local_validate =
	(* validates that the content of this element matches the model *)
	if not (validate_content content_model (self : 'ext #node :> 'ext node)) then
	  raise(Validation_error("Element `" ^ name ^ "' does not match its content model"))


      method create_data _ _ =
	failwith "method 'create_data' not applicable to element node"

      method keep_always_whitespace_mode =
	keep_always_whitespace <- true

      method write_compact_as_latin1 os =
	let invisible = (name = "-vr" || name = "-pi") in
	if not invisible then begin
	  write os "<" 0 1;
	  write os name 0 (String.length name);
	  List.iter
	    (fun (aname, avalue) ->
	       match avalue with
		   Implied_value -> ()
		 | Value v ->
		     write os "\n" 0 1;
		     write os aname 0 (String.length aname);
		     write os "=\"" 0 2;
		     write_data_string os v;
		     write os "\"" 0 1;
		 | Valuelist l ->
		     let v = String.concat " " l in
		     write os "\n" 0 1;
		     write os aname 0 (String.length aname);
		     write os "=\"" 0 2;
		     write_data_string os v;
		     write os "\"" 0 1;
	    )
	    attributes;
	  write os "\n>" 0 2;
	end;
	Hashtbl.iter
	  (fun n pi ->
	     pi # write_compact_as_latin1 os
	  )
	  (Lazy.force pinstr);
	List.iter 
	  (fun n -> n # write_compact_as_latin1 os)
	  (self # sub_nodes);
	if not invisible then begin
	  write os "</" 0 2;
	  write os name 0 (String.length name);
	  write os "\n>" 0 2;
	end

    end
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
	  T_element root_element_name ->
	    if not (dtd_r # arbitrary_allowed) then begin
	      match dtd_r # root with
		  Some declared_root_element_name ->
		    let real_root_element_name =
		      (* -------------------------------------------------*)
		      (* We must handle the special case that 'r' is the
		       * virtual root and not the real root.
		       *)
		      if root_element_name = "-vr" then begin
			(* The real root is the first son of 'r' that is
			 * not "-pi".
			 *)
			let real_r =
			  try
			    List.find
			      (fun r' -> 
				 match r' # node_type with
				     T_element "-pi" -> false
				   | T_element _     -> true
				   | _               -> assert false)
			      (r # sub_nodes)
			  with
			      Not_found ->      (* should not happen *)
				assert false
			in
			match real_r # node_type with 
			    T_element name -> name
			  | _              -> assert false
		      end
		      (* -------------------------------------------------*)
		      (* NORMAL CASE: *)
		      else root_element_name
		    in
		    if real_root_element_name <> declared_root_element_name then
		      raise
			(Validation_error ("The root element is `" ^ 
					   real_root_element_name ^ 
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

	| T_data ->
	    failwith "Pxp_document.document#init_root: the root node must be an element"

    method xml_version = xml_version

    method xml_standalone = 
      match dtd with
	  None -> false
	| Some d -> d # standalone_declaration

    method dtd =
      match dtd with
	  None -> failwith "Document has no dtd"
	| Some d -> d

    method root =
      match root with
	  None -> failwith "Document has no root element"
	| Some r -> r

    method add_pinstr pi =
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

    method write_compact_as_latin1 os =
      let r = self # root in
      write os "<?xml version='1.0' encoding='ISO-8859-1'?>\n" 0 44;
      ( match self # dtd # root with
	    None ->
	      self # dtd # write_compact_as_latin1 os false
	  | Some _ ->
	      self # dtd # write_compact_as_latin1 os true
      );
      Hashtbl.iter
	(fun n pi ->
	   pi # write_compact_as_latin1 os
	)
	(Lazy.force pinstr);
      r # write_compact_as_latin1 os;
	    

  end
;;


(* ======================================================================
 * History:
 *
 * $Log: pxp_document.ml,v $
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
