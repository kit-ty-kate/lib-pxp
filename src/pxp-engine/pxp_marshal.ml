(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_core_types
open Pxp_document
open Pxp_dtd
open Pxp_aux
open Pxp_yacc

type reconstruction_cmd =
    Head of (string * bool)                         (* Encoding, standalone? *)
  | Start_data_node of string
  | Declare_element_exemplar of (int * string)             (* number, eltype *)
  | Declare_attribute of (int * string)                    (* number, name *)
  | Start_element_node of ( (string * int * int) option *  (* position *)
                            int *                          (* exemplar number *)
                            (int * att_value) list)        (* attributes *)
  | Start_super_root_node of ( (string * int * int) option )  (* position *)
  | Start_comment_node of ( (string * int * int) option *     (* position *)
                            string option )                   (* comment text *)
  | Start_pinstr_node of ( (string * int * int) option *      (* position *)
			   string *                           (* target *)
			   string )                           (* PI text *)
  | Add_pinstr of (string * string)                           (* target, text *)
  | End_node
  | DTD_string of (string *                                  (* root element *) 
		   dtd_id *
                   string)                    (* The declarations as XML text *)
  | Document of string                                         (* XML version *)
  | Root                                                         (* delimiter *)
  | Cmd_array of reconstruction_cmd array
  | Namespace_mapping of (string * string array)
    (* normprefix, URIs (the last is the primary URI) *)
;;

(* Note: Marshalling is new in PXP 1.1 (did not exist in 1.0). The type
 * reconstruction_cmd changed several times during the development of
 * this module.
 *)

(* A node is represented as sequence of reconstruction_cmd values in the
 * following way:
 *
 * - Head
 * - Start_xxx_node ...
 *   Begins the representation of the node with type xxx
 * - Add_pinstr ...
 *   If necessary, one or several processing instructions (not for pinstr nodes)
 * - Now the sub nodes in order: Start_xxx_node ... End_node
 * - End_node
 *
 * If namespaces are used, the namespace_manager is prepended to the first
 * (outermost) node of the tree. It is represented as a sequence of
 * Namespace_mapping commands:
 *
 * - Namespace_mapping ...
 * - Namespace_mapping ...
 * - ...
 *
 * The namespace_info objects are (currently) not marshalled.
 *
 * A DTD is represented as a single DTD_string command.
 *
 * A Document is represented by:
 * - Head
 * - Document ...
 * - DTD_string ...
 * - Add_pinstr ...
 * - Root
 * - The root node: Start_xxx_node ... End_node
 *)

let id (s:string) = s;;

exception Interruption

type 'ext job =
    Marshal_node of 'ext Pxp_document.node 
  | Marshal_cmd  of reconstruction_cmd
 (* A job list is the plan that will marshal the rest of the whole
  * tree
  *)


type jobber =
    Done
  | Task of (unit -> jobber)


let recode_string ~in_enc ~out_enc =
    Netconversion.recode_string 
      ~in_enc
      ~out_enc
      ~subst:(fun k -> 
		failwith ("Pxp_marshal: Cannot recode code point " ^ 
			  string_of_int k ^ " from encoding " ^ 
			  Netconversion.string_of_encoding in_enc ^ 
			  " to encoding " ^ 
			  Netconversion.string_of_encoding out_enc))
;;


let subtree_to_cmd_sequence_nohead ~omit_positions ~recode write n : jobber = 
  (* Calls [write] for every command to write. The function [write] may
   * raise [Interruption] to indicate that the job should be interrupted.
   * This exception may be ignored, however, so it is necessary to raise
   * it several times until it can be honoured.
   * The result value of this function is either [Done] meaning that 
   * all work is done, or it is [Task f] meaning that the work is interrupted
   * and may be restarted by executing f() (resulting again in Done or Task).
   *)
  let m = 100 in
  let current_array = Array.create m End_node in  (* Collects up to [m] cmds *)
  let current_pos = ref 0 in                      (* next free index *)
  let write_nobreak cmd =
    (* Call [write] but ignore interruptions *)
    if !current_pos < Array.length current_array then begin
      current_array.( !current_pos ) <- cmd;
      incr current_pos
    end
    else begin
      ( try
	  write (Cmd_array(Array.copy current_array))
	with
	    Interruption -> ()        (* Ignore here *)
      );
      current_array.( 0 ) <- cmd;
      current_pos := 1;
    end
  in
  let breakpoint()=
    (* Returns [true] if there was an [Interruption] *)
    if !current_pos = Array.length current_array then begin
      begin try 
	write (Cmd_array(Array.copy current_array));
        current_pos := 0;
	false
      with
	  Interruption ->
	    current_pos := 0;
	    true
      end;
    end
    else false
  in
  let finish() =
    try
      write (Cmd_array(Array.sub current_array 0 !current_pos))
    with
	Interruption -> ()        (* Ignore here *)
  in
  let next_ex_number = ref 0 in
  let ex_hash = Hashtbl.create 100 in
  let next_att_number = ref 0 in
  let att_hash = Hashtbl.create 100 in
  let rec do_subtree n : 'ext job list = (
    (* Marshals some information, and returns the rest of what is to do
     * as job list
     *)
    match n # node_type with
	T_data ->
	  write_nobreak (Start_data_node (recode(n#data)));
	  write_nobreak End_node;
	  []
      | T_element eltype ->
	  let eltype = recode eltype in
	  let pos = get_position n in
	  let atts =
	    (* remove all Implied_value; use att_hash *)
	    List.map
	      (fun (name,a) ->
		 let name = recode name in
		 let a = match a with
		     Implied_value -> a
		   | Value s -> Value(recode s)
		   | Valuelist l -> Valuelist(List.map recode l)
		 in
		 try (Hashtbl.find att_hash name, a)
		 with
		     Not_found ->
		       let nr = !next_att_number in
		       incr next_att_number;
		       Hashtbl.add att_hash name nr;
		       write_nobreak (Declare_attribute(nr, name));
		       (nr, a)
	      )
	      (List.filter 
		 (fun (_,a) -> a <> Implied_value) 
		 (n # attributes)
	      )
	  in
	  let ex_nr =
	    try
	      Hashtbl.find ex_hash eltype 
	    with
		Not_found ->
		  let nr = !next_ex_number in
		  incr next_ex_number;
		  Hashtbl.add ex_hash eltype nr;
		  write_nobreak (Declare_element_exemplar(nr,eltype));
		  nr
	  in
	  write_nobreak (Start_element_node (pos, ex_nr, atts));
	  do_pinstr n;
	  plan_subnodes n @ [ Marshal_cmd End_node ]
      | T_super_root ->
	  write_nobreak (Start_super_root_node (get_position n));
	  do_pinstr n;
	  plan_subnodes n @ [ Marshal_cmd End_node ]
      | T_pinstr target ->
	  let pos = get_position n in
	  let l = n # pinstr target in
	  (match l with
	       [ pi ] ->
		 write_nobreak (Start_pinstr_node (pos, recode target, recode pi#value));
		 write_nobreak End_node;
		 []
	     | _ ->
		 assert false
	  )
      | T_comment ->
	  let pos = get_position n in
	  let comment' =
	    match n#comment with
		None -> None
	      | Some c -> Some(recode c)
	  in
	  write_nobreak (Start_comment_node (pos, comment'));
	  write_nobreak End_node;
	  []
      | _ ->
	  assert false
  )
  and do_pinstr n =
    let names = n # pinstr_names in
    List.iter
      (fun name ->
	 let pinstrs = n # pinstr name in
	 List.iter
	   (fun pi ->
	      write_nobreak (Add_pinstr (recode pi#target, recode pi#value));
	   )
	   pinstrs
      )
      names
  and plan_subnodes n =
    List.map (fun sn -> Marshal_node sn) n#sub_nodes
  and get_position n =
    if omit_positions then
      None
    else
      let entity, line, column = n # position in
      if line = 0 then None else Some (recode entity,line,column)
  in
  let emit_namespace_mappings () =
    (* TODO: In general, too many mappings are emitted, even mappings 
     * which are unused in the processed subtree.
     * It would also be possible to emit a mapping just before it is
     * used. 
     *)
    try
      let mng = n # namespace_manager in
      mng # iter_namespaces
	(fun normprefix ->
	   let uris = Array.of_list (mng # get_uri_list normprefix) in
	   let uris' = Array.map recode uris in
	   write_nobreak (Namespace_mapping (recode normprefix, uris'))
	);
      ()
    with
	Namespace_method_not_applicable _ ->
	  ()
  in
  let rec exec_job_list jl =
    match jl with
	[] ->
	  finish(); 
	  Done
      | Marshal_cmd cmd :: jl' ->
	  if breakpoint() then
	    Task(fun () -> exec_job_list jl)
	  else begin
	    write_nobreak cmd;
	    exec_job_list jl'
	  end
      | Marshal_node node :: jl' ->
	  let plan = do_subtree node in
	  exec_job_list (plan @ jl')
  in
  emit_namespace_mappings();
  exec_job_list [Marshal_node n]
;;


let subtree_to_cmd_sequence ?(omit_positions=false) ?enc f n =
  let enc, recode = match enc with
      None -> (n#encoding :> encoding), id
    | Some e -> e, (recode_string 
	              ~in_enc:(n#encoding :> encoding)
		      ~out_enc:e)
  in
  let encname = Netconversion.string_of_encoding enc in
  let sa = n#dtd#standalone_declaration in
  f(Head(encname,sa));
  let r = subtree_to_cmd_sequence_nohead ~omit_positions ~recode f n in
  assert(r = Done)
;;


let subtree_to_channel ?(omit_positions = false) ?enc ch n =
  subtree_to_cmd_sequence
    ~omit_positions:omit_positions
    ?enc
    (fun cmd -> 
       Marshal.to_channel ch cmd [ Marshal.No_sharing ]
    )
    n
;;


let subtree_from_cmd_sequence_nohead ~recode f0 dtd spec =
  let current_array = ref( [| |] ) in
  let current_pos = ref 0 in
  let rec f() =
    if !current_pos < Array.length !current_array then begin
      incr current_pos;
      !current_array.( !current_pos - 1)
    end
    else begin
      let c = f0() in
      match c with
	  Cmd_array a ->
	    current_array := a;
	    current_pos := 0;
	    f()
	| _ ->
	    c
    end
  in
  let recode_pos = 
    function
	None -> None
      | Some (pos_e,pos_l,pos_p) -> Some (recode pos_e,pos_l,pos_p)
  in
  let default = get_data_exemplar spec in
  let eltypes = ref (Array.create 100 ("",default)) in
  let atts = ref (Array.create 100 "") in
  let mng = new namespace_manager in
  let mng_found = ref false in
  let enable_mng, dest_mng = 
    try 
      true, dtd # namespace_manager
    with
	Namespace_method_not_applicable _ -> 
	  false, mng (* value does not matter *)
  in
  let map_nsprefix name =
    let p, l = namespace_split name in
    if p = "" then
      name
    else begin
      (* p is mapped to the primary URI of mng, and then mapped
       * to the normprefix of dest_mng
       *)
      try
	let primary_uri = mng # get_primary_uri p in
	let normprefix = 
	  dest_mng # lookup_or_add_namespace p primary_uri in
	normprefix ^ ":" ^ l
      with
	  Not_found ->
	    failwith "Pxp_marshal.subtree_from_cmd_sequence: Found an undeclared namespace prefix"
    end
  in
  let rec read_node dont_add first_cmd =
    let n =
      match first_cmd with
	  Start_data_node data ->
	    create_data_node spec dtd (recode data)
	| Declare_element_exemplar (nr, eltype) ->
	    let eltype = recode eltype in
	    if nr > Array.length !eltypes then begin
	      eltypes := 
	        Array.append !eltypes (Array.create 100 ("",default));
	    end;
	    let eltype' =
	      if !mng_found then
		(* Map the namespace prefix to a working normprefix *)
		map_nsprefix eltype
	      else
		eltype
	    in
	    !eltypes.(nr) <- (eltype',
			      get_element_exemplar spec eltype []);
	    read_node true (f())
	| Declare_attribute (nr, name) ->
	    let name = recode name in
	    if nr > Array.length !atts then begin
	      atts := 
	        Array.append !atts (Array.create 100 "");
	    end;
	    let name' =
	      if !mng_found then
		map_nsprefix name
	      else
		name
	    in
	    !atts.(nr) <- name';
	    read_node true (f())
	| Start_element_node (pos, nr, a) ->
	    let pos = recode_pos pos in
	    let eltype, ex = !eltypes.(nr) in
(* -- saves 4% time, but questionable approach:
	    ex # create_element 
	      ?position:pos
	      dtd
	      (T_element eltype)
	      atts
*)
	    let a' =
	      List.map 
		(fun (nr, v) -> 
		   let v' = match v with
		       Implied_value -> Implied_value
		     | Value s -> Value(recode s)
		     | Valuelist l -> Valuelist(List.map recode l)
		   in
		   !atts.(nr), v'
		) 
		a in
	    let e =
	      create_element_node
	        ~att_values: a'
	        ?position:pos
		spec
		dtd
		eltype
		[] in
	    e
	| Start_super_root_node pos ->
	    let pos = recode_pos pos in
	    create_super_root_node ?position:pos spec dtd
	| Start_comment_node (pos, comment) ->
	    let pos = recode_pos pos in
	    (match comment with
		 Some c ->
		   create_comment_node
		   ?position:pos
		     spec
		     dtd
		     (recode c)
	       | None ->
		   let cn = create_comment_node
			      ?position:pos
			      spec
			      dtd
			      "" in
		   cn # set_comment None;
		   cn
	    )
	| Start_pinstr_node (pos, target, value) ->
	    let pos = recode_pos pos in
	    create_pinstr_node
	      ?position:pos
	      spec
	      dtd
	      (new proc_instruction (recode target) (recode value) (dtd # encoding))
	| Namespace_mapping (normprefix, uris) ->
	    let normprefix = recode normprefix in
	    if enable_mng then begin
	      let primary_uri = recode uris.( Array.length uris - 1 ) in
	      if normprefix <> "xml" then
		mng # add_namespace normprefix primary_uri;
	      for i=0 to Array.length uris - 2 do
		mng # add_uri normprefix (recode uris.(i))
	      done;
	      mng_found := true;
	    end;
	    read_node true (f())
	| _ ->
	    assert false
    in
    let rec add () =
      match f () with
	  (Start_data_node _ |
	   Start_element_node (_,_,_) |
	   Start_super_root_node _ |
	   Start_comment_node (_,_) |
	   Start_pinstr_node (_,_,_) |
	   (* Declare_xxx is always followed by Start_element_node: *)
	   Declare_element_exemplar(_,_) |
	   Declare_attribute(_,_)) as cmd ->
	    (* Add a new sub node *)
	    let n' = read_node false cmd in
	    n # add_node ~force:true n';
	    add()
	| Add_pinstr(target,value) ->
	    let pi = new proc_instruction 
		       (recode target) (recode value) (dtd # encoding) in
	    n # add_pinstr pi;
	    add()
	| End_node ->
	    ()
	| _ ->
	    failwith "Pxp_marshal.subtree_from_cmd_sequence"
    in
    if not dont_add then
      add();
    n
  in
  read_node false (f())
;;


let subtree_from_cmd_sequence f dtd spec =
  match f() with
      Head(enc_s,_) ->
	let enc = Netconversion.encoding_of_string enc_s in
	let recode = 
	  if (dtd # encoding :> encoding) = enc then
	    id
	  else
	    recode_string 
	      ~in_enc:enc
	      ~out_enc:(dtd # encoding :> encoding)
	in

	subtree_from_cmd_sequence_nohead ~recode f dtd spec

    | _ ->
	failwith "Pxp_marshal.subtree_from_cmd_sequence"
;;


let subtree_from_channel ch dtd spec =
  try
    subtree_from_cmd_sequence
      (fun () -> 
	 Marshal.from_channel ch)
      dtd 
      spec
  with
      End_of_file ->
	failwith "Pxp_marshal.subtree_from_channel"
;;


let document_to_cmd_sequence ?(omit_positions = false) ?enc f 
       (doc : 'ext Pxp_document.document) =
  let enc = 
    match enc with
	None -> (doc # encoding :> encoding)
      | Some e -> e
  in
  let encname = Netconversion.string_of_encoding enc in
  let recode = recode_string
		 ~in_enc:(doc # encoding :> encoding)
		 ~out_enc:enc in
  let sa = doc # dtd # standalone_declaration in
  f (Head (encname, sa));
  f (Document (doc # xml_version));
  let dtd_buffer = Buffer.create 1000 in
  doc # dtd # write (`Out_buffer dtd_buffer) enc false; 
  let r = 
    match doc # dtd # root with
	None -> ""
      | Some x -> recode x
  in
  let id =
    match doc # dtd # id with
	None -> Internal
      | Some x -> x   
	  (* Do not need to recode because DTD IDs are always UTF-8 *)
  in
  f (DTD_string (r,
		 id,
		 Buffer.contents dtd_buffer));
  List.iter
    (fun pi_name ->
       List.iter
	 (fun pi ->
	    f (Add_pinstr (recode pi#target, recode pi#value))
	 )
	 (doc # pinstr pi_name)
    )
    (doc # pinstr_names);
  f Root;
  let r = subtree_to_cmd_sequence_nohead
	    ~omit_positions
	    ~recode
	    f
	    (doc # root) in
  assert(r=Done)
;;


let document_to_channel ?(omit_positions = false) ?enc ch doc =
  document_to_cmd_sequence
    ~omit_positions:omit_positions
    ?enc
    (fun cmd -> 
       Marshal.to_channel ch cmd [ Marshal.No_sharing ]
    )
    doc
;;


let document_from_cmd_sequence f config spec =
  let cmd0 = f() in
  let enc_s, sa =
    match cmd0 with
	Head(e,s) -> e,s
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let enc = Netconversion.encoding_of_string enc_s in
  let recode =
    if enc = (config.encoding :> encoding) then
      id
    else
      recode_string 
        ~in_enc:enc
        ~out_enc:(config.encoding :> encoding)
  in
  let cmd1 = f() in
  let xml_version =
    match cmd1 with
	Document v -> v
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let cmd2 = f() in
  let root_type, id, dtd_string =
    match cmd2 with
	DTD_string(r,i,s) -> r,i,s
      | _ -> failwith "Pxp_marshal.document_from_cmd_sequence"
  in
  let dtd =
    Pxp_yacc.parse_dtd_entity 
      config
      (Pxp_yacc.from_string 
         ~fixenc:enc
	 dtd_string) in
  if root_type <> "" then dtd # set_root (recode root_type);
  dtd # set_id id;
  dtd # set_standalone_declaration sa;
  let doc = new Pxp_document.document 
	      ?swarner:config.swarner config.warner config.encoding in
  doc # init_xml_version xml_version;
  let cmd = ref (f()) in
  while !cmd <> Root do
    ( match !cmd with
	  Add_pinstr(target,value) ->
	    let pi = new proc_instruction 
		       (recode target) (recode value) config.encoding in
	    doc # add_pinstr pi
	| _ ->
	    failwith "Pxp_marshal.document_from_cmd_sequence"
    );
    cmd := f();
  done;
  let root = 
    subtree_from_cmd_sequence_nohead
      ~recode
      f dtd spec in
  doc # init_root root (recode root_type);
  doc
;;

let document_from_channel ch config spec =
  try
    document_from_cmd_sequence
      (fun () -> 
	 Marshal.from_channel ch)
      config
      spec
  with
      End_of_file ->
	failwith "Pxp_marshal.document_from_channel"
;;


let relocate_subtree tree new_dtd new_spec =
  let remaining_job = ref Done in
  let available_cmds = Queue.create() in

  let continue() =
    match !remaining_job with
	Done -> 
	  assert false
      | Task f ->
	  remaining_job := f()
  in
  
  let encname = Netconversion.string_of_encoding(tree#encoding :> encoding) in
  let sa = tree#dtd#standalone_declaration in
  Queue.add (Head(encname,sa)) available_cmds;

  remaining_job :=
    subtree_to_cmd_sequence_nohead 
      ~omit_positions:false ~recode:id 
      (fun cmd ->
	 Queue.add cmd available_cmds;
	 raise Interruption
      )
      tree;

  let rec next_cmd() =
    let n_cmd = 
      try Some(Queue.take available_cmds) with Queue.Empty -> None in
    match n_cmd with
	None ->
	  continue();
	  assert(ignore(Queue.peek available_cmds); true);
	  next_cmd()
      | Some cmd ->
	  cmd
  in

  let tree' = 
    subtree_from_cmd_sequence
      next_cmd
      new_dtd
      new_spec in
  assert(!remaining_job = Done);
  tree'
;;


let relocate_document (doc : 'ext document) new_conf new_spec =
  let recode =
    recode_string 
    ~in_enc:(doc # encoding :> encoding)
    ~out_enc:(new_conf.encoding :> encoding)
  in

  (* Relocate the DTD: *)
  let dtd = doc # dtd in
  let buf = Buffer.create 128 in
  let enc = (new_conf.encoding :> encoding) in
  dtd # write (`Out_buffer buf) enc false;
  let new_dtd = parse_dtd_entity 
		  new_conf (from_string ~fixenc:enc (Buffer.contents buf)) in
  (* The following properties of the DTD do not survive a write/parse cycle: *)
  if dtd # arbitrary_allowed then new_dtd # allow_arbitrary;
  List.iter
    (fun eltype -> 
       if (dtd # element eltype) # arbitrary_allowed then
	 (new_dtd # element eltype) # allow_arbitrary
    )
    (dtd # element_names);
  new_dtd # set_standalone_declaration (dtd # standalone_declaration);
  ( match dtd # id with
	None -> ()
      | Some id -> new_dtd # set_id id;
  );
  ( match dtd # root with
	None -> ()
      | Some r -> new_dtd # set_root (recode r)
  );
  (* Note: namespace_manager is set according to new_conf *)

  (* Relocate the XML tree: *)
  let new_root = relocate_subtree doc#root new_dtd new_spec in

  (* Create a new document containing the new DTD and the new XML tree: *)
  let new_doc = new document 
		  ?swarner:new_conf.swarner 
		  new_conf.warner new_conf.encoding in
  new_doc # init_xml_version (doc # xml_version);
  let root_name = match new_dtd # root with
      Some rn -> rn 
    | None    -> failwith "Pxp_marshal.relocate_document"
  in
  new_doc # init_root new_root root_name;

  new_doc
;;
	  

