(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_core_types
open Pxp_lexer_types
open Pxp_lexers
open Pxp_entity
open Pxp_aux
open Pxp_dfa

(**********************************************************************)

type validation_record =
    { content_model   : content_model_type;
      content_dfa     : dfa_definition option Lazy.t;
      id_att_name     : string option;
      idref_att_names : string list;
      att_lookup      : int Str_hashtbl.t;
      init_att_vals   : (string * att_value) array;
      att_info        : (att_type * bool) array;
      att_required    : int list;
      accept_undeclared_atts : bool;
    }
;;


(* class type? *)

class namespace_manager =
object (self)
    val uri_of_prefix = Hashtbl.create 10  (* not unique *)
    val prefix_of_uri = Hashtbl.create 10  (* unique *)
    val primary_uri_of_prefix = Hashtbl.create 10  (* unique *)

    initializer
      ignore(self # add_namespace "xml" "http://www.w3.org/XML/1998/namespace")

    method add_uri (np:string) (uri:string) =
      if not (Hashtbl.mem uri_of_prefix np) then 
	raise(Namespace_prefix_not_managed np);
      try
	let np' = Hashtbl.find prefix_of_uri uri in
	if np <> np' then
	  raise(Namespace_error "add_uri: the URI is already managed")
      with
	  Not_found ->
	    Hashtbl.add uri_of_prefix np uri;
	    Hashtbl.add prefix_of_uri uri np;
	    ()

    method add_namespace np uri =
      let l = Hashtbl.find_all uri_of_prefix np in
      if l = [] then begin
	if Hashtbl.mem prefix_of_uri uri then
	  raise(Namespace_error "add_namespace: the URI is already managed");
	Hashtbl.add uri_of_prefix np uri;
	Hashtbl.add primary_uri_of_prefix np uri;
	Hashtbl.add prefix_of_uri uri np;
      end
      else 
	if l <> [ uri ] then
	  raise(Namespace_error "add_namespace: the namespace does already exist")

    method lookup_or_add_namespace prefix (uri:string) =
      let rec add_loop n =
	let p = prefix ^ (if n=0 then "" else string_of_int n) in
	if Hashtbl.mem uri_of_prefix p then begin
	  add_loop (n+1)
	end
	else begin
	  Hashtbl.add uri_of_prefix p uri;
	  Hashtbl.add primary_uri_of_prefix p uri;
	  Hashtbl.add prefix_of_uri uri p;
	  p
	end
      in
      try
	Hashtbl.find prefix_of_uri uri
      with
	  Not_found ->
	    add_loop (if prefix = "" then 1 else 0)
	      (* prefix = "": make sure that such a prefix is never added *)

    method get_primary_uri normprefix =
      try
	Hashtbl.find primary_uri_of_prefix normprefix
      with
	  Not_found -> 
	    raise(Namespace_prefix_not_managed normprefix)

    method get_uri_list normprefix =
      Hashtbl.find_all uri_of_prefix normprefix

    method get_normprefix uri =
      try
	Hashtbl.find prefix_of_uri uri
      with
	  Not_found ->
	    raise(Namespace_not_managed uri)

    method iter_namespaces f =
      Hashtbl.iter 
	(fun p uri -> f p)
	primary_uri_of_prefix

    method as_declaration =
      let l = ref [] in
      Hashtbl.iter 
	(fun p uri -> l := (p, uri) :: !l)
	primary_uri_of_prefix;
      !l
      
  end
;;


let create_namespace_manager () = new namespace_manager;;


class type namespace_scope =
object
  method namespace_manager : namespace_manager
  method parent_scope : namespace_scope option
  method declaration : (string * string) list
  method effective_declaration : (string * string) list
  method display_prefix_of_uri : string -> string
  method display_prefix_of_normprefix : string -> string
  method uri_of_display_prefix : string -> string
  method normprefix_of_display_prefix : string -> string
end
;;



module StrSet = Set.Make(String);;

class namespace_scope_impl mng parent_opt decl : namespace_scope =
object(self)
  method namespace_manager = mng
  method parent_scope = parent_opt
  method declaration = decl

  method effective_declaration =
    let rec collect visible d s =
      match d with
	| ("", "") :: d' ->
	    if StrSet.mem "" visible then
	      collect visible d' s  (* no effect *)
	    else
	      collect (StrSet.add "" visible) d' s  (* hide inner default *)
	| (dp, uri) :: d' ->
	    if StrSet.mem dp visible then
	      collect visible d' s
	    else
	      (dp, uri) :: collect (StrSet.add dp visible) d' s
	| [] ->
	    ( match s # parent_scope with
		  Some s' ->
		    collect visible s'#declaration s'
		| None ->
		    []
	    )
    in
    collect StrSet.empty self#declaration (self : #namespace_scope :> namespace_scope)

  method display_prefix_of_uri uri =
    try
      fst(List.find (fun (p,u) -> u = uri) decl)
    with
	Not_found ->
	  ( match parent_opt with
		Some pa -> pa # display_prefix_of_uri uri
	      | None    -> raise(Namespace_not_in_scope uri)
	  )

  method display_prefix_of_normprefix np =
    let uris = mng # get_uri_list np in
    if uris = [] then raise(Namespace_prefix_not_managed np);
    try
      fst(List.find (fun (p,u) -> List.mem u uris) decl)
    with
	Not_found ->
	  ( match parent_opt with
		Some pa -> pa # display_prefix_of_normprefix np
	      | None    -> raise(Namespace_not_in_scope
				   (List.hd(List.rev uris)))
	  )

  method uri_of_display_prefix dp =
    try
      List.assoc dp decl
    with
	Not_found -> 
	  ( match parent_opt with
		Some pa -> pa # uri_of_display_prefix dp
	      | None    -> raise Not_found
	  )

  method normprefix_of_display_prefix dp =
    let uri = self # uri_of_display_prefix dp in
    mng # get_normprefix uri

end
;;


let create_namespace_scope ?parent ?(decl = []) mng =
  new namespace_scope_impl mng parent decl ;;


class dtd  ?swarner the_warner init_encoding =
  object (self)
    val mutable root = (None : string option)
    val mutable id =   (None : dtd_id option)
    val mutable mng =  (None : namespace_manager option)

    val warner       = (the_warner : collect_warnings)
    val swarner      = (swarner : symbolic_warnings option)
    val encoding     = init_encoding
    val lfactory     = Pxp_lexers.get_lexer_factory init_encoding

    val elements     = (Str_hashtbl.create 100 : dtd_element Str_hashtbl.t)
    val gen_entities = (Str_hashtbl.create 100 : (entity * bool) Str_hashtbl.t)
    val par_entities = (Str_hashtbl.create 100 : entity Str_hashtbl.t)
    val notations    = (Str_hashtbl.create 100 : dtd_notation Str_hashtbl.t)
    val pinstr       = (Str_hashtbl.create 100 : proc_instruction Str_hashtbl.t)
    val mutable element_names = []
    val mutable gen_entity_names = []
    val mutable par_entity_names = []
    val mutable notation_names = []
    val mutable pinstr_names = []

    val mutable allow_arbitrary = false
    val mutable standalone_declaration = false

    val mutable validated = false

    initializer
    let w = new drop_warnings in
    self # add_gen_entity 
      (new internal_entity self "lt"   None w "&#38;#60;" false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "gt"   None w "&#62;"     false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "amp"  None w "&#38;#38;" false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "apos" None w "&#39;"     false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "quot" None w "&#34;"     false false encoding)
      false;


    method encoding = encoding

    method lexer_factory = lfactory

    method warner = warner

    method swarner = swarner

    method set_root r =
      if root = None then
	root <- Some r
      else
	assert false


    method set_id j =
      if id = None then
	id <- Some j
      else
	assert false


    method standalone_declaration = standalone_declaration

    method set_standalone_declaration b =
      standalone_declaration <- b

    method allow_arbitrary =
      allow_arbitrary <- true

    method disallow_arbitrary =
      allow_arbitrary <- false

    method arbitrary_allowed = allow_arbitrary

    method root = root
    method id = id

    method namespace_manager =
      match mng with
	  None -> raise(Namespace_method_not_applicable "namespace_manager")
	| Some m -> m

    method set_namespace_manager m =
      mng <- Some m

    method add_element el =
      (* raises Not_found if 'el' has already been added *)
      (* Note: 'el' is encoded in the same way as 'self'! *)
      let name = el # name in
      check_name ?swarner warner name;
      if Str_hashtbl.mem elements name then
	raise Not_found;
      Str_hashtbl.add elements name el;
      element_names <- name :: element_names;
      validated <- false


    method add_gen_entity en extdecl =
      (* The following is commented out; perhaps there should be an option
       * to reactivate it on demand
       *)
      (* raises Validation_error if the predefines entities 'lt', 'gt', 'amp',
       * 'quot', and 'apos' are redeclared with an improper value.
       *)
      if en # encoding <> encoding then
	failwith "Pxp_dtd.dtd # add_gen_entity: Inconsistent encodings";
      let name = en # name in
      check_name ?swarner warner name;
      if Str_hashtbl.mem gen_entities name then begin
	if List.mem name [ "lt"; "gt"; "amp"; "quot"; "apos" ] then begin
	  (* These are allowed to be declared several times *)
	  let (rt,_) = en # replacement_text in
	  let toks = tokens_of_content_string lfactory rt in
	  try
	    begin match toks with
	      [CRef 60]       -> if name <> "lt"   then raise Not_found
	    | [CharData ">"]  -> if name <> "gt"   then raise Not_found
	    | [CRef 62]       -> if name <> "gt"   then raise Not_found
	    | [CRef 38]       -> if name <> "amp"  then raise Not_found
	    | [CharData "'"]  -> if name <> "apos" then raise Not_found
	    | [CRef 39]       -> if name <> "apos" then raise Not_found
	    | [CharData "\""] -> if name <> "quot" then raise Not_found
	    | [CRef 34]       -> if name <> "quot" then raise Not_found
	    | _               -> raise Not_found
	    end
	  with
	      Not_found ->
		raise (Validation_error("Predefined entity `" ^ name ^
					"' redeclared"))
	end
	else
	  warn swarner warner (`W_entity_declared_twice name)
      end
      else begin
	Str_hashtbl.add gen_entities name (en, extdecl);
	gen_entity_names <- name :: gen_entity_names
      end


    method add_par_entity en =
      if en # encoding <> encoding then
	failwith "Pxp_dtd.dtd # add_par_entity: Inconsistent encodings";
      let name = en # name in
      check_name ?swarner warner name;
      if not (Str_hashtbl.mem par_entities name) then begin
	Str_hashtbl.add par_entities name en;
	par_entity_names <- name :: par_entity_names
      end
      else
	warn swarner warner (`W_entity_declared_twice name)


    method add_notation no =
      (* raises Validation_error if 'no' already added *)
      if no # encoding <> encoding then
	failwith "Pxp_dtd.dtd # add_notation: Inconsistent encodings";
      let name = no # name in
      check_name ?swarner warner name;
      if Str_hashtbl.mem notations name then
	raise (Validation_error("Notation `" ^ name ^ "' declared twice"));
      Str_hashtbl.add notations name no;
      notation_names <- name :: notation_names


    method add_pinstr pi =
      if pi # encoding <> encoding then
	failwith "Pxp_dtd.dtd # add_pinstr: Inconsistent encodings";
      let name = pi # target in
      check_name ?swarner warner name;

      if String.length name >= 4 && String.sub name 0 4 = "pxp:" then begin
	match name with
	    "pxp:dtd" -> 
	      let _, optname, atts = pi # parse_pxp_option in
	      begin match optname with
		  "optional-element-and-notation-declarations" ->
		    self # allow_arbitrary
		| "optional-attribute-declarations" ->
		    let el_string = 
		      try List.assoc "elements" atts
		      with Not_found ->
			raise(Error("Missing `elements' attribute for pxp:dtd"))
		    in
		    let el = split_attribute_value lfactory el_string in
		    List.iter
		      (fun e_name ->
			 let e =
			   try Str_hashtbl.find elements e_name
			   with
			       Not_found ->
				 raise(Error("Reference to unknown element `" ^
					     e_name ^ "'"))
			 in
			 e # allow_arbitrary
		      )
		      el
		| "namespace" ->
		    let prefix = 
		      try List.assoc "prefix" atts
		      with Not_found ->
			raise(Error("Missing `prefix' attribute for pxp:dtd"))
		    in
		    let uri =
		      try List.assoc "uri" atts
		      with Not_found ->
			raise(Error("Missing `uri' attribute for pxp:dtd"))
		    in
		    ( match mng with
			  None ->
			    raise(Error("Cannot do pxp:dtd instruction: namespaces not enabled"))
			| Some m ->
			    ( try m # add_uri prefix uri
			      with Namespace_prefix_not_managed _ ->
				m # add_namespace prefix uri
			    )
		    )
		| _ ->
		    raise(Error("Unknown PXP option `" ^ 
				optname ^ "'"))
	      end
	  | _ ->
	      raise(Error("The processing instruction target `" ^ 
			  name ^ "' is not defined by this PXP version"))
      end;
      Str_hashtbl.add pinstr name pi;
      if not (List.mem name pinstr_names) then
	pinstr_names <- pinstr_names @ [name];


    method element name =
      (* returns the element 'name' or raises Validation_error if not found *)
      try
	Str_hashtbl.find elements name
      with
	  Not_found ->
	    if allow_arbitrary then
	      raise Undeclared
	    else
	      raise(Validation_error("Reference to undeclared element `" ^ name ^ "'"))

    method element_names =
      (* returns the list of all names of element declarations *)
      element_names


    method gen_entity name =
      (* returns the entity 'name' or raises WF_error if not found *)
      try
	Str_hashtbl.find gen_entities name
      with
	  Not_found ->
	    raise(WF_error("Reference to undeclared general entity `" ^ name ^ "'"))


    method gen_entity_names = gen_entity_names


    method par_entity name =
      (* returns the entity 'name' or raises WF_error if not found *)
      try
	Str_hashtbl.find par_entities name
      with
	  Not_found ->
	    raise(WF_error("Reference to undeclared parameter entity `" ^ name ^ "'"))


    method par_entity_names = par_entity_names


    method notation name =
      (* returns the notation 'name' or raises Validation_error if not found *)
      try
	Str_hashtbl.find notations name
      with
	  Not_found ->
	    if allow_arbitrary then
	      raise Undeclared
	    else
	      raise(Validation_error("Reference to undeclared notation `" ^ name ^ "'"))


    method notation_names = notation_names


    method pinstr name =
      (* returns the list of all processing instructions contained in the DTD
       * with target 'name'
       *)
      Str_hashtbl.find_all pinstr name


    method pinstr_names = pinstr_names

    method write_ref os enc =
      let write_sysid s =
	write_markup_string 
	  ~from_enc:`Enc_utf8 ~to_enc:enc os
	  ( if String.contains s '"' then
	      "'" ^ s ^ "'"
	    else
	      "\"" ^ s ^ "\""
	  )
      in
      let wms = 
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      wms "<!DOCTYPE ";
      ( match root with
	    None -> failwith "#write: DTD without root";
	  | Some r -> wms r
      );
      begin match id with
	  None ->
	    failwith "#write_ref: DTD does not have an ID"
	| Some (External (Public (p,s))) ->
	    wms " PUBLIC ";
	    write_sysid p;
	    wms " ";
	    write_sysid s
	| Some (External (System s)) ->
	    wms " SYSTEM ";
	    write_sysid s
	| Some (External _) ->
	    failwith "#write_ref: External ID cannot be represented"
	| Some Internal ->
	    failwith "#write_ref: Cannot write internal ID"
	| Some (Derived _) ->
	    failwith "#write_ref: Cannot write derived ID"
      end;
      wms ">\n";


    method write os enc doctype = 
      let wms = 
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      let write_sysid s =
	write_markup_string 
	  ~from_enc:`Enc_utf8 ~to_enc:enc os
	  ( if String.contains s '"' then
	      "'" ^ s ^ "'"
	    else
	      "\"" ^ s ^ "\""
	  )
      in

      if doctype then begin
	wms "<!DOCTYPE ";
	( match root with
	    None -> failwith "#write: DTD without root";
	  | Some r -> wms r
	);
	wms " [\n";
      end;

      (* Notations: *)
      List.iter
	(fun name ->
	   let notation = 
	     try Str_hashtbl.find notations name with Not_found -> assert false in
	   notation # write os enc)
	(List.sort compare notation_names);

      (* Unparsed entities: *)
      List.iter
	(fun name ->
	   let ent,_ = 
	     try Str_hashtbl.find gen_entities name with Not_found -> assert false 
	   in
	   if ent # is_ndata then begin
	     let xid = ent # ext_id in
	     let notation = ent # notation in
	     wms ("<!ENTITY " ^ name ^ " " );
	     ( match xid with
		   System s ->
		     wms "SYSTEM ";
		     write_sysid s;
		 | Public (p,s) ->
		     wms "PUBLIC ";
		     write_sysid p;
		     if (s <> "") then begin
		       wms " ";
		       write_sysid s;
		     end;
		 | Anonymous ->
		     failwith "#write: External ID `Anonymous' cannot be represented"
		 | Private _ ->
		     failwith "#write: External ID `Private' cannot be represented"
	     );
	     wms (" NDATA " ^ notation ^ ">\n");
	   end
	)
	(List.sort compare gen_entity_names);

      (* Elements: *)
      List.iter
	(fun name ->
	   let element = 
	     try Str_hashtbl.find elements name with Not_found -> assert false in
	   element # write os enc)
	(List.sort compare element_names);

      (* Processing instructions: *)
      List.iter
	(fun name ->
	   List.iter
	     (fun pi ->
		pi # write os enc)
	     (Str_hashtbl.find_all pinstr name)
	)
	(List.sort compare pinstr_names);

      if doctype then 
	wms "]>\n";


    (************************************************************)
    (*                    VALIDATION                            *)
    (************************************************************)

    method only_deterministic_models =
      Str_hashtbl.iter
	(fun n el ->
	   let cm = el # content_model in
	   match cm with
	       Regexp _ ->
		 if el # content_dfa = None then
		   raise(Validation_error("The content model of element `" ^
					  n ^ "' is not deterministic"))
	     | _ ->
		 ()
	)
	elements;
      

    method validate =
      if validated or allow_arbitrary then
	()
      else begin
	(* Validity constraint: Notations in NDATA entity declarations must
	 * be declared
	 *)
	List.iter
	  (fun name ->
	     let ent,_ = 
	       try Str_hashtbl.find gen_entities name with Not_found -> assert false 
	     in
	     if ent # is_ndata then begin
	       let xid = ent # ext_id in
	       let notation = ent # notation in
	       try
		 ignore(self # notation notation)
		   (* Raises Validation_error if the constraint is violated *)
	       with
		   Undeclared -> ()
	     end
	  )
	  gen_entity_names;

	(* Validate the elements: *)
	Str_hashtbl.iter
	  (fun n el ->
	     el # validate)
	  elements;

	(* Check the root element: *)
	(* TODO: Check if this piece of code is executed at all! *)
	begin match root with
	    None -> ()
	  | Some r ->
	      begin try
		let _ = Str_hashtbl.find elements r in ()
	      with
		  Not_found ->
		    raise(Validation_error("The root element is not declared"))
	      end
	end;
	validated <- true;
      end

    method invalidate =
      validated <- false

    (************************************************************)

  end


(**********************************************************************)

and dtd_element the_dtd the_name =
  object (self)
    val dtd = (the_dtd : dtd)
    val name = the_name
    val lfactory = the_dtd # lexer_factory
    val mutable content_model = Unspecified
    val mutable content_model_validated = false
    val mutable content_dfa = lazy None

    val mutable externally_declared = false

    val mutable attributes = 
	    ([] : (string * ((att_type * att_default) * bool)) list)
    val mutable attributes_validated = false

    val mutable id_att_name = None
    val mutable idref_att_names = []

    val mutable allow_arbitrary = false

    val mutable vr = (None : validation_record option)

    method name = name

    method set_cm_and_extdecl m extdecl =
      if content_model = Unspecified then begin
	content_model <- m;
	content_model_validated <- false;
	content_dfa <- lazy (self # compute_content_dfa);
	externally_declared <- extdecl;
	self # update_vr;
	dtd # invalidate
      end
      else
	raise(Validation_error("Element `" ^ name ^ "' has already a content model"))

    method content_model = content_model

    method content_dfa = Lazy.force content_dfa
      
    method private compute_content_dfa =
      match content_model with
	  Regexp re ->
	    ( try Some (dfa_of_regexp_content_model re)
	      with Not_found -> None
	    )
	| _ ->
	    None

    method externally_declared = externally_declared

    method encoding = dtd # encoding

    method allow_arbitrary =
      allow_arbitrary <- true;
      self # update_vr;

    method disallow_arbitrary =
      allow_arbitrary <- false;
      self # update_vr;

    method arbitrary_allowed = allow_arbitrary

    method add_attribute aname t d extdecl =
      let swarner = dtd#swarner 
      and warner = dtd#warner in
      if aname <> "xml:lang" & aname <> "xml:space" then
	check_name ?swarner warner aname;
      if List.mem_assoc aname attributes then
	warn swarner warner (`W_multiple_attribute_declarations(name,aname))
      else begin
	begin match aname with
	    "xml:space" ->
	      begin match t with
		  A_enum l ->
		    let ok =
		      List.for_all
			(fun tok -> List.mem tok ["default";"preserve"])
			l
		    in
		    if not ok then
		      raise(Validation_error("Declaration of attribute `xml:space' does not conform to XML specification"))
		| _ ->
		    raise(Validation_error("Declaration of attribute `xml:space' does not conform to XML specification"))
	      end
	  | _ -> ()
	end; 
	begin match t with
	    A_id ->
	      id_att_name <- Some aname;
	  | (A_idref | A_idrefs) ->
	      idref_att_names <- aname :: idref_att_names;
	  | _ ->
	      ()
	end;
	attributes <- (aname, ((t,d),extdecl)) :: attributes;
	attributes_validated <- false;
	dtd # invalidate;
	self # update_vr;
      end

    method attribute attname =
      try
	fst (List.assoc attname attributes)
      with
	  Not_found ->
	    if allow_arbitrary then
	      raise Undeclared
	    else
	      raise(Validation_error("Attribute `" ^ attname ^ "' of element `"
				     ^ name ^ "' not declared"))

    method attribute_violates_standalone_declaration attname v =
      try
	let (atype, adefault), extdecl = List.assoc attname attributes in
	extdecl &&
	( match v with
	      None -> 
		adefault <> D_required && adefault <> D_implied
		(* i.e. adefault matches D_default or D_fixed *)
	    | Some s ->
		atype <> A_cdata &&
		normalization_changes_value lfactory atype s
	)
      with
	  Not_found ->
	    if allow_arbitrary then
	      raise Undeclared
	    else
	      raise(Validation_error("Attribute `" ^ attname ^ "' of element `"
				     ^ name ^ "' not declared"))


    method attribute_names =
      List.map fst attributes

    method names_of_required_attributes =
      List.flatten
	(List.map
	   (fun (n,((t,d),_)) ->
	      if d = D_required then
		[n]
	      else
		[])
	   attributes)

    method id_attribute_name = id_att_name

    method idref_attribute_names = idref_att_names


    method private update_vr =
      vr <- None

    method internal_vr =
      (	match vr with
	    None ->
	      let n = List.length attributes in
	      let init_att_vals = Array.create n ("", Implied_value) in
	      let att_lookup = Str_hashtbl.create n in
	      let att_info = Array.create n (A_cdata, false) in
	      let att_required = ref [] in
	      let k = ref 0 in
	      List.iter
		(fun (n, ((t,d), ext)) ->

		   Str_hashtbl.add att_lookup n !k;

		   let init_val = 
		     match d with
			 (D_required | D_implied) -> Implied_value
		       | D_default v ->
			   value_of_attribute lfactory dtd n t v
		       | D_fixed v ->
			   value_of_attribute lfactory dtd n t v
		   in

		   init_att_vals.( !k ) <- (n, init_val);
		   att_info.( !k ) <- (t, match d with D_fixed _ -> true 
				                     | _         -> false);
		   if d = D_required then
		     att_required := !k :: !att_required;
		   incr k;
		)
		attributes;
		
	      vr <- Some { content_model = content_model;
			   content_dfa =  content_dfa;
			   id_att_name = id_att_name;
			   idref_att_names = idref_att_names;
			   init_att_vals = init_att_vals;
			   att_lookup = att_lookup;
			   att_info = att_info;
			   att_required = !att_required;
			   accept_undeclared_atts = allow_arbitrary;
			 }
	  | _ -> ()
      );
      ( match vr with
	    None -> assert false
	  | Some vr' -> vr'
      )

    method write os enc = 
      let encoding = self # encoding in
      let wms = 
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      let rec write_contentspec cs =
	match cs with
	    Unspecified ->
	      failwith "#write: Unspecified content model found"
	  | Empty ->
	      wms "EMPTY"
	  | Any ->
	      wms "ANY"
	  | Mixed ml ->
	      wms "(";
	      write_mixedspec_list ml;
	      wms ")*";
	  | Regexp re ->
	      write_children re false

      and write_mixedspec_list ml =
	match ml with
	    MPCDATA :: ml' ->
	      wms "#PCDATA";
	      if ml' <> [] then wms "|";
	      write_mixedspec_list ml';
	  | MChild s :: ml' ->
	      wms s;
	      if ml' <> [] then wms "|";
	      write_mixedspec_list ml';
	  | [] ->
	      ()

      and write_children re cp =
	match re with
	    Optional re' ->
	      let p = needs_parens re' in
	      if p then wms "(";
	      write_children re' cp;
	      if p then wms ")";
	      wms "?";
	  | Repeated re' ->
	      let p = needs_parens re' in
	      if p then wms "(";
	      write_children re' cp;
	      if p then wms ")";
	      wms "*";
	  | Repeated1 re' ->
	      let p = needs_parens re' in
	      if p then wms "(";
	      write_children re' cp;
	      if p then wms ")";
	      wms "+";
	  | Alt re' ->
	      wms "(";
	      ( match re' with
		    re1' :: rer' ->
		      write_children re1' true;
		      List.iter
			(fun ren' ->
			   wms "|";
			   write_children ren' true;
			)
			rer';
		  | [] ->
		      failwith "#write: Illegal content model"
	      );
	      wms ")";
	  | Seq re' ->
	      wms "(";
	      ( match re' with
		    re1' :: rer' ->
		      write_children re1' true;
		      List.iter
			(fun ren' ->
			   wms ",";
			   write_children ren' true;
			)
			rer';
		  | [] ->
		      failwith "#write: Illegal content model"
	      );
	      wms ")";
	  | Child ch ->
	      if not cp then wms "(";
	      wms ch;
	      if not cp then wms ")";

      and needs_parens re =
	match re with
	    (Optional _ | Repeated _ | Repeated1 _ ) -> true
	  | _ -> false
      in

      wms ("<!ELEMENT " ^ name ^ " ");
      write_contentspec content_model;
      wms ">\n";

      wms ("<!ATTLIST " ^ name);
      List.iter
	(fun (n,((t,d),_)) ->
	   wms ("\n  " ^ n);
	   ( match t with
		 A_cdata       -> wms " CDATA";
	       | A_id          -> wms " ID";
	       | A_idref       -> wms " IDREF";
	       | A_idrefs      -> wms " IDREFS";
	       | A_entity      -> wms " ENTITY";
	       | A_entities    -> wms " ENTITIES";
	       | A_nmtoken     -> wms " NMTOKEN";
	       | A_nmtokens    -> wms " NMTOKENS";
	       | A_notation nl -> 
		   wms " NOTATION (";
		   ( match nl with
			 nl1:: nl' ->
			   wms nl1;
			   List.iter
			     (fun n ->
				wms ("|" ^ n);
			     )
			     nl'
		       | [] ->
			   failwith "#write: Illegal content model";
		   );
		   wms ")";
	       | A_enum el     ->
		   wms " (";
		   ( match el with
			 el1:: el' ->
			   wms el1;
			   List.iter
			     (fun e ->
				wms ("|" ^ e);
			     )
			     el'
		       | [] ->
			   failwith "#write: Illegal content model";
		   );
		   wms ")";
	   );
	   ( match d with
		 D_required -> wms " #REQUIRED"
	       | D_implied  -> wms " #IMPLIED"
	       | D_default s ->
		   wms " \"";
		   write_data_string ~from_enc:encoding ~to_enc:enc os s;
		   wms "\"";
	       | D_fixed s ->
		   wms " FIXED \"";
		   write_data_string ~from_enc:encoding ~to_enc:enc os s;
		   wms "\"";
	   );
	)
	(List.sort (fun (n1,x1) (n2,x2) -> compare n1 n2) attributes);

      wms ">\n";

    (************************************************************)
    (*                    VALIDATION                            *)
    (************************************************************)

    method validate =
      self # validate_attributes();
      self # validate_content_model()

    method private validate_attributes() =
      if attributes_validated then
	()
      else begin
	(* Validity Constraint: One ID per Element Type *)
	let n = count (fun (n,((t,d),_)) -> t = A_id) attributes in
	if n > 1 then
	  raise(Validation_error("More than one ID attribute for element `" ^ name ^ "'"));
	(* Validity Constraint: ID Attribute Default *)
	if List.exists
	     (fun (n,((t,d),_)) ->
		t = A_id & (d <> D_required & d <> D_implied))
	     attributes
	then
	  raise(Validation_error("ID attribute must be #IMPLIED or #REQUIRED; element `" ^ name ^ "'"));
	(* Validity Constraint: One Notation per Element Type *)
	let n = count (fun (n,((t,d),_)) ->
			 match t with A_notation _ -> true | _ -> false)
		      attributes in
	if n > 1 then
	  raise(Validation_error("More than one NOTATION attribute for element `" ^ name ^ "'"));
	(* Validity Constraint: Notation Attributes [second part] *)
	List.iter
	  (fun (n,((t,d),_)) ->
	     match t with
		 A_notation l ->
		   List.iter
		     (fun nname ->
			let _ = dtd # notation nname in ())
		     l
	       | _ -> ())
	  attributes;
	(* Validity Constraint: Attribute Default Legal *)
	List.iter
	  (fun (n,((t,d),_)) ->

	     let check v =
	       let lexical_error() =
		 lazy (raise(Validation_error("Default value for attribute `" ^ n ^ "' is lexically malformed"))) in
	       check_attribute_value_lexically lfactory (lexical_error()) t v;
	       begin match t with
		   (A_entity|A_entities) ->
		     List.iter
		       (fun nd ->
			  let en, extdecl = dtd # gen_entity nd in
			  if not (en # is_ndata) then
			    raise(Validation_error("Attribute default value must be the name of an NDATA entity; attribute `" ^ n ^ "' in declaration for element `" ^ name ^ "'"));
(*			  if dtd # standalone_declaration && extdecl then
			    raise(Validation_error("Attribute default value violates the standalone declaration; attribute `" ^ n ^ "' in declaration for element `" ^ name ^ "'")); 
-- This is checked anyway when the attribute value is normalized
*)
		       )
		       (split_attribute_value lfactory v)
		 | A_notation nl ->
		     if not (List.mem v nl) then
		       raise(Validation_error("Illegal default value for attribute `" ^ n ^ "' in declaration for element `" ^ name ^ "'"));
		 | A_enum nl ->
		     if not (List.mem v nl) then
		       raise(Validation_error("Illegal default value for attribute `" ^ n ^ "' in declaration for element `" ^ name ^ "'"));
		 | _          -> ()
	       end
	     in

	     match d with
		 D_required -> ()
	       | D_implied -> ()
	       | D_default v -> check v
	       | D_fixed v   -> check v
	  )
	  attributes;

	(* Ok: This element declaration is valid *)
	attributes_validated <- true;

      end

    method private validate_content_model () =
      (* checks:
       * - Validity Constraint: No Duplicate Types
       * It is not an error if there is a child in the declaration for which
       * no element declaration is provided.
       *)
      match content_model with
	  Unspecified ->
	    warn (dtd#swarner) (dtd#warner)
 	         (`W_element_mentioned_but_not_declared name);
	    ()
	| Empty -> ()
	| Any -> ()
	| Mixed (pcdata :: l) ->
	    (* MPCDATA is always the first element by construction *)
	    assert (pcdata = MPCDATA);
	    if check_dups l then
	      raise (Validation_error("Double children in declaration for element `" ^ name ^ "'"))
	| Regexp _ -> ()
	| _ -> assert false



    (************************************************************)

  end

and dtd_notation the_name the_xid init_encoding =
object (self)
    val name = the_name
    val xid = (the_xid : ext_id)
    val encoding = (init_encoding : Pxp_core_types.rep_encoding)
    method name = name
    method ext_id = xid
    method encoding = encoding

    method write os enc = 
      let wms = 
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      let write_sysid s =
	if String.contains s '"' then
	  wms ("'" ^ s ^ "'")
	else
	  wms ("\"" ^ s ^ "\"");
      in

      wms ("<!NOTATION " ^ name ^ " ");
      ( match xid with
	    System s ->
	      wms "SYSTEM ";
	      write_sysid s;
	  | Public (p,s) ->
	      wms "PUBLIC ";
	      write_sysid p;
	      if (s <> "") then begin
		wms " ";
		write_sysid s;
	      end;
	  | Anonymous ->
	      failwith "#write: External ID `Anonymous' cannot be represented"
	  | Private _ ->
	      failwith "#write: External ID `Private' cannot be represented"
      );
      wms ">\n";

  end

and proc_instruction the_target the_value init_encoding =
object (self)
    val target = the_target
    val value = (the_value : string)
    val encoding = (init_encoding : Pxp_core_types.rep_encoding)

    initializer
      match target with
	  ("xml"|"xmL"|"xMl"|"xML"|"Xml"|"XmL"|"XMl"|"XML") ->
	    (* This is an error, not a warning, because I do not have a
	     * "warner" object by hand.
	     *)
	    raise(WF_error("Reserved processing instruction"))
	| _ -> ()

    method target = target
    method value = value
    method encoding = encoding

    method write os enc = 
      let wms = 
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      wms "<?";
      wms target;
      wms " ";
      wms value;
      wms "?>";

    method parse_pxp_option =
      let lfactory = get_lexer_factory encoding in
      try
	let toks = tokens_of_xml_pi lfactory value in (* may raise WF_error *)
	begin match toks with
	    (Pro_name option_name) :: toks' ->
	      let atts = decode_xml_pi toks' in       (* may raise WF_error *)
	      (target, option_name, atts)
	  | _ ->
	      raise(Error("Bad PXP processing instruction"))
	end
      with
	  WF_error _ ->
	    raise(Error("Bad PXP processing instruction"))

  end
;;

let create_dtd ?swarner ?(warner = new drop_warnings) enc =
  new dtd ?swarner warner enc ;;


type source =
    Entity of ((dtd -> Pxp_entity.entity) * Pxp_reader.resolver)
  | ExtID of (ext_id * Pxp_reader.resolver)
  | XExtID of (ext_id * string option * Pxp_reader.resolver)
;;

module Entity = struct
  let get_name ent = ent # name
  let get_full_name ent = ent # full_name
  let get_encoding ent = ent # encoding
  let get_type ent =
    if ent # is_ndata then `NDATA else
      try ignore(ent # ext_id); `External with Not_found -> `Internal
  let replacement_text ent = fst(ent # replacement_text)
  let get_xid ent =
    try Some(ent # ext_id) with Not_found -> None
  let get_resolver_id ent =
    try Some(ent # resolver_id) with Not_found -> None
  let get_notation ent =
    if ent # is_ndata then Some (ent # notation) else None
  let create_internal_entity ~name ~value dtd =
    new internal_entity dtd name (dtd # swarner) (dtd # warner) value 
        false false (dtd # encoding)
  let create_ndata_entity ~name ~xid ~notation dtd =
    new ndata_entity name xid notation dtd#encoding
  let create_external_entity ?(doc_entity = false) ?system_base 
                             ~name ~xid ~resolver dtd =
    if doc_entity then
      new document_entity resolver dtd name dtd#swarner dtd#warner xid
	                  system_base dtd#encoding
    else
      new external_entity resolver dtd name dtd#swarner dtd#warner xid
	                  system_base false dtd#encoding
  let from_external_source ?doc_entity ~name dtd src =
    match src with
	ExtID(xid,resolver) -> 
	  create_external_entity ?doc_entity ~name ~xid ~resolver dtd
      | XExtID(xid,system_base,resolver) -> 
	  create_external_entity ?doc_entity ?system_base 
                                 ~name ~xid ~resolver dtd
      | Entity(make,resolver) ->
	  make dtd  (* resolver ignored *)

  let entity_id ent = (ent :> < >)

  class fake = object end

  let create_entity_id () = new fake

end


