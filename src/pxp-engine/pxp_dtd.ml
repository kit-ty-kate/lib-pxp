(* $Id: pxp_dtd.ml,v 1.7 2000/07/16 17:50:01 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_lexer_types
open Pxp_entity
open Pxp_aux

(**********************************************************************)

class dtd  the_warner init_encoding =
  object (self)
    val mutable root = (None : string option)
    val mutable id =   (None : dtd_id option)

    val warner       = (the_warner : collect_warnings)
    val encoding     = init_encoding
    val lexerset     = Pxp_lexers.get_lexer_set init_encoding

    val elements     = (Hashtbl.create 100 : (string,dtd_element) Hashtbl.t)
    val gen_entities = (Hashtbl.create 100 : (string,entity * bool) Hashtbl.t)
    val par_entities = (Hashtbl.create 100 : (string,entity) Hashtbl.t)
    val notations    = (Hashtbl.create 100 : (string,dtd_notation) Hashtbl.t)
    val pinstr       = (Hashtbl.create 100 : (string,proc_instruction) Hashtbl.t)
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
      (new internal_entity self "lt"   w "&#38;#60;" false false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "gt"   w "&#62;"     false false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "amp"  w "&#38;#38;" false false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "apos" w "&#39;"     false false false encoding)
      false;
    self # add_gen_entity 
      (new internal_entity self "quot" w "&#34;"     false false false encoding)
      false;


    method encoding = encoding

    method warner = warner

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


    method add_element el =
      (* raises Not_found if 'el' has already been added *)
      (* Note: 'el' is encoded in the same way as 'self'! *)
      let name = el # name in
      check_name warner name;
      if Hashtbl.mem elements name then
	raise Not_found;
      Hashtbl.add elements name el;
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
      check_name warner name;
      if Hashtbl.mem gen_entities name then begin
	if List.mem name [ "lt"; "gt"; "amp"; "quot"; "apos" ] then begin
	  (* These are allowed to be declared several times *)
	  let (rt,_) = en # replacement_text in
	  let toks = tokens_of_content_string lexerset rt in
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
	  warner # warn ("Entity `" ^ name ^ "' declared twice")
      end
      else begin
	Hashtbl.add gen_entities name (en, extdecl);
	gen_entity_names <- name :: gen_entity_names
      end


    method add_par_entity en =
      if en # encoding <> encoding then
	failwith "Pxp_dtd.dtd # add_par_entity: Inconsistent encodings";
      let name = en # name in
      check_name warner name;
      if not (Hashtbl.mem par_entities name) then begin
	Hashtbl.add par_entities name en;
	par_entity_names <- name :: par_entity_names
      end
      else
	warner # warn ("Entity `" ^ name ^ "' declared twice")


    method add_notation no =
      (* raises Validation_error if 'no' already added *)
      if no # encoding <> encoding then
	failwith "Pxp_dtd.dtd # add_notation: Inconsistent encodings";
      let name = no # name in
      check_name warner name;
      if Hashtbl.mem notations name then
	raise (Validation_error("Notation `" ^ name ^ "' declared twice"));
      Hashtbl.add notations name no;
      notation_names <- name :: notation_names


    method add_pinstr pi =
      if pi # encoding <> encoding then
	failwith "Pxp_dtd.dtd # add_pinstr: Inconsistent encodings";
      let name = pi # target in
      check_name warner name;
      Hashtbl.add pinstr name pi;
      pinstr_names <- name :: pinstr_names;
      if name = "xml:allow_undeclared_elements_and_notations" then
	self # allow_arbitrary;
      if name = "xml:allow_undeclared_attributes" then begin
	let v = pi # value in
	let e = 
	  try
	    Hashtbl.find elements v
	  with
	      Not_found ->
		raise(Validation_error("Reference to undeclared element `" ^ v ^ "'"))
	in
	e # allow_arbitrary;
      end;


    method element name =
      (* returns the element 'name' or raises Validation_error if not found *)
      try
	Hashtbl.find elements name
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
      (* returns the entity 'name' or raises Validation_error if not found *)
      try
	Hashtbl.find gen_entities name
      with
	  Not_found ->
	    raise(WF_error("Reference to undeclared general entity `" ^ name ^ "'"))


    method gen_entity_names = gen_entity_names


    method par_entity name =
      (* returns the entity 'name' or raises Validation_error if not found *)
      try
	Hashtbl.find par_entities name
      with
	  Not_found ->
	    raise(WF_error("Reference to undeclared parameter entity `" ^ name ^ "'"))


    method par_entity_names = par_entity_names


    method notation name =
      (* returns the notation 'name' or raises Validation_error if not found *)
      try
	Hashtbl.find notations name
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
      Hashtbl.find_all pinstr name


    method pinstr_names = pinstr_names

    method write os enc doctype = 
      let wms = 
	write_markup_string ~from_enc:encoding ~to_enc:enc os in

      let write_sysid s =
	if String.contains s '"' then
	  wms ("'" ^ s ^ "'")
	else
	  wms ("\"" ^ s ^ "\"");
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
	     try Hashtbl.find notations name with Not_found -> assert false in
	   notation # write os enc)
	(List.sort compare notation_names);

      (* Unparsed entities: *)
      List.iter
	(fun name ->
	   let ent,_ = 
	     try Hashtbl.find gen_entities name with Not_found -> assert false 
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
		     failwith "#write: External ID Anonymous cannot be represented"
	     );
	     wms (" NDATA " ^ notation ^ ">\n");
	   end
	)
	(List.sort compare gen_entity_names);

      (* Elements: *)
      List.iter
	(fun name ->
	   let element = 
	     try Hashtbl.find elements name with Not_found -> assert false in
	   element # write os enc)
	(List.sort compare element_names);

      (* Processing instructions: *)
      List.iter
	(fun name ->
	   let pi = 
	     try Hashtbl.find pinstr name with Not_found -> assert false in
	   pi # write os enc)
	(List.sort compare pinstr_names);

      if doctype then 
	wms "]>\n";

    method write_compact_as_latin1 os doctype = 
      self # write os `Enc_iso88591 doctype



    (************************************************************)
    (*                    VALIDATION                            *)
    (************************************************************)

    method validate =
      if validated or allow_arbitrary then
	()
      else begin
	Hashtbl.iter
	  (fun n el ->
	     el # validate)
	  elements;
	begin match root with
	    None -> ()
	  | Some r ->
	      begin try
		let _ = Hashtbl.find elements r in ()
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
    val lexerset = Pxp_lexers.get_lexer_set (the_dtd # encoding)
    val mutable content_model = Unspecified
    val mutable content_model_validated = false

    val mutable externally_declared = false

    val mutable attributes = 
	    ([] : (string * ((att_type * att_default) * bool)) list)
    val mutable attributes_validated = false

    val mutable id_att_name = None
    val mutable idref_att_names = []

    val mutable allow_arbitrary = false

    method name = name

    method set_cm_and_extdecl m extdecl =
      if content_model = Unspecified then begin
	content_model <- m;
	content_model_validated <- false;
	externally_declared <- extdecl;
	dtd # invalidate
      end
      else
	raise(Validation_error("Element `" ^ name ^ "' has already a content model"))

    method content_model = content_model

    method externally_declared = externally_declared

    method encoding = dtd # encoding

    method allow_arbitrary =
      allow_arbitrary <- true

    method disallow_arbitrary =
      allow_arbitrary <- false

    method arbitrary_allowed = allow_arbitrary

    method add_attribute aname t d extdecl =
      if aname <> "xml:lang" & aname <> "xml:space" then
	check_name (dtd#warner) aname;
      if List.mem_assoc aname attributes then
	dtd # warner # warn ("More than one declaration for attribute `" ^
			     aname ^ "' of element type `" ^ name ^ "'")
      else begin
	begin match aname with
	    "xml:space" ->
	      begin match t with
		  A_enum l ->
		    let l' = Sort.list ( <= ) l in
		    if l' <> [ "default"; "preserve" ] then
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
	      idref_att_names <- aname :: idref_att_names
	  | _ ->
	      ()
	end;
	attributes <- (aname, ((t,d),extdecl)) :: attributes;
	attributes_validated <- false;
	dtd # invalidate;
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
				     ^ name ^ "' not found"))

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
		normalization_changes_value lexerset atype s
	)
      with
	  Not_found ->
	    if allow_arbitrary then
	      raise Undeclared
	    else
	      raise(Validation_error("Attribute `" ^ attname ^ "' of element `"
				     ^ name ^ "' not found"))


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
	attributes;

      wms ">\n";

    method write_compact_as_latin1 os = 
      self # write os `Enc_iso88591

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
	       check_attribute_value_lexically lexerset (lexical_error()) t v;
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
		       (split_attribute_value lexerset v)
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
	    dtd # warner # warn ("Element type `" ^ name ^ "' mentioned but not declared");
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
    val encoding = (init_encoding : Pxp_types.rep_encoding)
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
	      failwith "#write: External ID Anonymous cannot be represented"
      );
      wms ">\n";

    method write_compact_as_latin1 os = 
      self # write os `Enc_iso88591 

  end

and proc_instruction the_target the_value init_encoding =
object (self)
    val target = the_target
    val value = (the_value : string)
    val encoding = (init_encoding : Pxp_types.rep_encoding)

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

    method write_compact_as_latin1 os = 
      self # write os `Enc_iso88591

  end
;;


(* ======================================================================
 * History:
 *
 * $Log: pxp_dtd.ml,v $
 * Revision 1.7  2000/07/16 17:50:01  gerd
 * 	Fixes in 'write'
 *
 * Revision 1.6  2000/07/16 16:34:41  gerd
 * 	New method 'write', the successor of 'write_compact_as_latin1'.
 *
 * Revision 1.5  2000/07/14 13:56:48  gerd
 * 	Added methods id_attribute_name and idref_attribute_names.
 *
 * Revision 1.4  2000/07/09 00:13:37  gerd
 * 	Added methods gen_entity_names, par_entity_names.
 *
 * Revision 1.3  2000/07/04 22:10:55  gerd
 * 	Update: collect_warnings -> drop_warnings.
 * 	Update: Case ext_id = Anonymous.
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
 *
 * Revision 1.18  2000/05/28 17:24:55  gerd
 * 	Bugfixes.
 *
 * Revision 1.17  2000/05/27 19:21:25  gerd
 * 	Implemented the changes of rev. 1.10 of markup_dtd.mli.
 *
 * Revision 1.16  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.15  2000/05/14 21:50:07  gerd
 * 	Updated: change in internal_entity.
 *
 * Revision 1.14  2000/05/06 23:08:46  gerd
 * 	It is possible to allow undeclared attributes.
 *
 * Revision 1.13  2000/05/01 20:42:46  gerd
 *         New method write_compact_as_latin1.
 *
 * Revision 1.12  2000/05/01 15:16:57  gerd
 * 	The errors "undeclared parameter/general entities" are
 * well-formedness errors, not validation errors.
 *
 * Revision 1.11  2000/03/11 22:58:15  gerd
 * 	Updated to support Markup_codewriter.
 *
 * Revision 1.10  2000/01/20 20:53:47  gerd
 * 	Changed such that it runs with Markup_entity's new interface.
 *
 * Revision 1.9  1999/11/09 22:15:41  gerd
 * 	Added method "arbitrary_allowed".
 *
 * Revision 1.8  1999/09/01 22:52:22  gerd
 * 	If 'allow_arbitrary' is in effect, no validation happens anymore.
 *
 * Revision 1.7  1999/09/01 16:21:24  gerd
 * 	Added several warnings.
 * 	The attribute type of "xml:space" is now strictly checked.
 *
 * Revision 1.6  1999/08/15 20:34:21  gerd
 * 	Improved error messages.
 * 	Bugfix: It is no longer allowed to create processing instructions
 * with target "xml".
 *
 * Revision 1.5  1999/08/15 02:20:16  gerd
 * 	New feature: a DTD can allow arbitrary elements.
 *
 * Revision 1.4  1999/08/15 00:21:39  gerd
 * 	Comments have been updated.
 *
 * Revision 1.3  1999/08/14 22:12:52  gerd
 *         Several functions have now a "warner" as argument which is
 * an object with a "warn" method. This is used to warn about characters
 * that cannot be represented in the Latin 1 alphabet.
 * 	Bugfix: if two general entities with the same name are definied,
 * the first counts, not the second.
 *
 * Revision 1.2  1999/08/11 14:56:35  gerd
 * 	Declaration of the predfined entities {lt,gt,amp,quot,apos}
 * is no longer forbidden; but the original definition cannot be overriddden.
 * 	TODO: If these entities are redeclared with problematic values,
 * the user should be warned.
 *
 * Revision 1.1  1999/08/10 00:35:51  gerd
 * 	Initial revision.
 *
 *
 *)
