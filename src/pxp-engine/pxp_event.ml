(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_dtd
open Pxp_aux
open Printf

let to_list get_ev =
  (* This function must be tail-recursive! *)
  let rec collect l =
    match get_ev() with
	Some ev -> collect (ev::l)
      | None    -> List.rev l
  in
  collect []
;;


let of_list l =
  let l = ref l in
  fun () ->
    match !l with
	x :: l' ->
	  l := l';
	  Some x
      | [] ->
	  None
;;


let concat get_ev_list =
  let get_ev_list = ref get_ev_list in
  let rec get_ev' () =
    match !get_ev_list with
	get_ev :: get_ev_list' ->
	  let ev = get_ev() in
	  ( match ev with
		Some _ -> ev
	      | None   -> get_ev_list := get_ev_list';
		          get_ev'()
	  )
      | [] ->
	  None
  in
  get_ev'
;;


let rec iter f get_ev =
  match get_ev() with
      Some e ->
	f e;
	iter f get_ev
    | None ->
	()
;;


let extract e get_ev =
  let counter = ref 0 in
  let eof = ref false in
  
  let count_event e =
    match e with
	E_start_doc(_,_)
      | E_start_tag(_,_,_,_)
      | E_start_super ->
	  incr counter
      | E_end_doc _
      | E_end_tag(_,_)
      | E_end_super ->
	  decr counter;
	  eof := (!counter <= 0);
      | E_position(_,_,_) ->
	  ()
      | _ ->
	  eof := (!counter <= 0);
  in

  let rec get_ev'() =
    if !eof then (
      if !counter <> 0 then failwith "Pxp_event.extract: Unbalanced events";
      None
    ) else
      let e_opt = get_ev() in
      match e_opt with
	  Some e' ->
	    count_event e';
	    e_opt
	| None ->
	    if !counter <> 0 then failwith "Pxp_event.extract: Unbalanced events";
	    None
  in

  count_event e;
  concat [ of_list [e]; get_ev' ]
;;


let pfilter p get_ev =
  let rec get_ev'() =
    match get_ev () with
	Some e as e_opt ->
	  if p e then
	    e_opt
	  else
	    get_ev'()
      | None ->
	  None
  in
  get_ev'
;;


type filter = (unit -> event option) -> (unit -> event option)

let norm_cdata_filter get_ev =
  let q = Queue.create () in
  let rec get_ev' thing =
    try
      Queue.pop q
    with
	Queue.Empty ->
	  let ev = get_ev thing in
	  match ev with
	      Some (E_char_data s) ->
		if s = "" then
		  get_ev' thing
		else
		  gather_string [s] thing
	    | _ ->
		ev
  and gather_string sl thing =
    let ev = get_ev thing in
    match ev with
	Some (E_char_data s) ->
	  gather_string (s :: sl) thing
      | _ ->
	  Queue.add (Some(E_char_data(String.concat "" (List.rev sl)))) q;
	  Queue.add ev q;
	  get_ev' thing
  in
  get_ev'
;;
	

let drop_ignorable_whitespace_filter get_ev =
  let found_dtd = ref None in
  let elements = Stack.create() in
  let pos = ref("",0,0) in

  let has_ignorable_ws elname =
    match !found_dtd with
	Some dtd ->
	  ( try 
	      let el = dtd # element elname in
	      let cm = el # content_model  in
	      ( match cm with
		    Regexp _ -> true
		  | Mixed ml -> not (List.mem MPCDATA ml)
		  | _ -> false
	      )
	    with
		Undeclared -> false
	      | Validation_error _ -> false   (* element not found *)
	  )
      | None ->
	  false
  in

  let pop() =
    try
      ignore(Stack.pop elements)
    with
	Stack.Empty ->
	  failwith "Pxp_event.drop_ignorable_whitespace_filter: bad event stream"
  in

  let rec get_ev' thing =
    let ev = get_ev thing in
    match ev with
	Some(E_start_doc(_,dtd)) ->
	  if !found_dtd <> None then
	    failwith "Pxp_event.drop_ignorable_whitespace_filter: More than one E_start_doc event";
	  found_dtd := Some dtd;
	  ev
      | Some(E_position(e,line,col)) ->
	  pos := (e,line,col);
	  ev
      |	Some(E_start_tag(name,_,_,_)) ->
	  let ign_ws = has_ignorable_ws name in
	  Stack.push ign_ws elements;
	  ev
      | Some(E_end_tag(name,_)) ->
	  pop();
	  ev
      | Some(E_char_data s) ->
	  let ign_ws = try Stack.top elements with Stack.Empty -> true in
	  if ign_ws then (
	    if not (Pxp_lib.only_whitespace s) then
	      let (e,line,col) = !pos in
	      let where = "In entity " ^ e ^ ", at line " ^ 
			  string_of_int line ^ ", position " ^ 
			  string_of_int col ^ ":\n" in
	      raise(At(where,WF_error("Data not allowed here")))
	    else
	      (* drop this event, and continue with next: *)
	      get_ev' thing
	  ) 
	  else ev
	  
      | _ ->
	  ev
  in

  get_ev'
;;


let unwrap_document pull =
  let doc_details = ref None in
  let first_event_done = ref false in

  let get_doc_details() =
    if not !first_event_done then (
      match pull() with
	| E_start_doc(v,dtd) ->
	    doc_details := Some(v,dtd);
	    first_event_done := true
	| _ ->
	    ()   (* Will cause an exception! *)
    );
    match !doc_details with
      | None ->
	  failwith "Pxp_event.unwrap_document: No E_start_doc event found"
      | Some (v,dtd) ->
	  (v,dtd)
  in

  let pull' =
    pfilter
      (function
	 | E_start_doc(v,dtd) ->
	     doc_details := Some(v,dtd);
	     first_event_done := true;
	     false
	 | E_end_doc | E_start_super | E_end_super | E_end_of_stream ->
	     false
	 | E_error e ->
	     raise e
	 | _ ->
	     true
      )
      pull
  in

  (get_doc_details, pull')
;;


let namespace_split = Pxp_aux.namespace_split
;;


let extract_prefix = Pxp_aux.extract_prefix
;;


type dtd_style =
    [ `Ignore
    | `Include
    | `Reference
    ]
;;

let wr_dsp do_display default_prefix dtd_style minimization out enc rep_enc get_ev =
  (* do_display: whether [display_events] was called, not [write_events].
   * default_prefix: The (optional) default prefix (only [write_events])
   *)

  let wms =
    write_markup_string ~from_enc:rep_enc ~to_enc:enc out in

  let write_att prefix (aname, avalue) =
    wms ("\n" ^ prefix ^ aname ^ "=\"");
    write_data_string ~from_enc:rep_enc ~to_enc:enc out avalue;
    wms "\"";
  in

  let write_stack = Stack.create() in
    (* Stack elements: 
     * (literal tag name, map of declared prefixes => uris, declared default uri)
     *)

  let write_start_tag name atts scope_opt minimized =
    let (_, prefixes, default) = 
      try Stack.top write_stack
      with Stack.Empty -> ("", StringMap.empty, None) in
    (* prefixes: namespaces already in scope
     * default: current xmlns default declaration
     *)

    let name_p, name_local = namespace_split name in

    let name' = 
      (* Strip prefix if it is the default prefix.
       * Note: we ignore [default], and take always [default_prefix] as
       * reference.
       *)
      match default_prefix with
	  Some d -> if name_p = d then name_local else name
	| None   -> name in

    (* Output start tag, and contained attributes: *)
    wms ("<" ^ name');
    List.iter (write_att "") atts;

    ( match scope_opt with
	  Some scope ->
	    (* Namespace case: *)
	    let mng = scope # namespace_manager in
	    (* Check whether additional namespace prefixes need to be
	     * declared:
	     *)
	    let (nsdecls, prefixes') = 
	      List.fold_left
		(fun (decls_acc, pref_acc) (n,_) ->
		   let p = extract_prefix n in
		   if p = "" then
		     (decls_acc, pref_acc)
		   else
		     let uri = mng # get_primary_uri p in
		     if StringMap.mem p pref_acc then
		       (decls_acc, pref_acc)
		     else
		       ( (p,uri) :: decls_acc,
			 StringMap.add p uri pref_acc )
		)
		([], prefixes)
		((name,"") :: atts)
	    in
	    (* Check whether the default namespace needs to be declared: *)
	    let nsdefault =
	      match default_prefix with
		  Some p ->
		    if default = None then 
		      let uri = mng # get_primary_uri p in
		      [ uri ] 
		    else []
		| None ->
		    []
	    in
	    (* Print the declarations: *)
	    List.iter (write_att "xmlns:") nsdecls;
	    List.iter (fun (  v) -> write_att "" ("xmlns", v)) nsdefault;
	    (* Push new state to stack:*)
	    let default' = 
	      match nsdefault with
		  [ uri ] -> Some uri
		| [] -> None
		| _ -> assert false in
	    Stack.push (name', prefixes', default') write_stack;

	| None ->
	    (* Non-namespace case: *)
	    Stack.push (name', prefixes, default) write_stack;
    );
    if minimized then (
      wms "\n/>";
      ignore(Stack.pop write_stack)
    )
    else
      wms "\n>";
  in

  let display_start_tag name atts scope_opt minimized =
    let (_, prefixes, default) = 
      try Stack.top write_stack
      with Stack.Empty -> ("", StringMap.empty, None) in
    (* prefixes: namespaces already in scope. The default declaration
     *   is represented as prefix "".
     * default: not used
     *)

    match scope_opt with
	Some scope ->
	  (* namespace case *)
	  let name_p, name_local = namespace_split name in
  
	  (* Get the required declarations: *)
	  let mng = scope # namespace_manager in
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

	  let write_att_remap (aname, avalue) =
            let (p,local) = namespace_split aname in
            if p = "" then
              write_att "" (aname,avalue)
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
              write_att (d ^ ":") (local, avalue)
	  in

	  let this_display_prefix =
            try 
	      scope # display_prefix_of_normprefix name_p
            with Namespace_not_in_scope _ ->
              (* Display prefix is missing. This is an error, but we
               * can search or invent a new prefix on the fly.
               *)
              ( let uri = mng # get_primary_uri name_p in
		try
		  search_prefix uri
		with
                    Not_found ->
                      invent_new_prefix uri )
	  in
          
	  let name' =
            if this_display_prefix = "" then
              name_local  (* within default namespace *)
            else
              this_display_prefix ^ ":" ^ name_local in

	  wms ("<" ^ name');
	  List.iter write_att_remap atts;
	  List.iter
            (fun (n,v) ->
               if n = "" then
		 write_att "" ("xmlns",v)
               else
		 write_att "xmlns:" (n,v))
	    (eff_decl_to_add @ !eff_decl_to_add');

	  if minimized then
	    wms "\n/>"
	  else (
	    wms "\n>";
	    Stack.push (name', !prefixes', default) write_stack;
	  )

      | None ->
	  (* non-namespace case *)
	  (* Output start tag, and contained attributes: *)
	  wms ("<" ^ name);
	  List.iter (write_att "") atts;
	  if minimized then
	    wms "\n/>"
	  else (
	    wms "\n>";
	    Stack.push (name, prefixes, default) write_stack;
	  )
  in

  let write_end_tag() =
    let (name, _, _) = 
      try Stack.pop write_stack 
      with Stack.Empty -> 
	failwith "Pxp_event.write/display: Unbalanced start/end tags"
    in
    wms "</";
    wms name;
    wms "\n>"
  in

  let rec wr_dsp_event ev_opt =
    let ev =
      match ev_opt with
	| Some ev -> ev
	| None -> get_ev() in
    match ev with
      | Some(E_start_doc(version,dtd)) ->
	  wms ("<?xml version=\"" ^ version ^ "\" ");
	  wms ("encoding=\"" ^ Netconversion.string_of_encoding enc ^ "\" ");
	  if dtd # standalone_declaration then
	    wms "standalone=\"yes\" ";
	  wms "?>\n";
	  ( match dtd_style with
		`Ignore -> ()
	      | `Include -> 
		  dtd # write out enc true
	      | `Reference ->
		  ( match dtd # id with
			Some (External _) -> 
			  dtd # write_ref out enc
		      | _ ->
			  failwith "Pxp_event.write/display: Cannot output DTD as reference"
		  )
	  );
	  wr_dsp_event None
      | Some (E_end_doc lit_name) ->
	  wr_dsp_event None
      | Some (E_start_tag (name, atts, scope_opt, _)) ->
	  let minimized, ev_opt =
	    match minimization with
	      | `None -> 
		  (false, None)
	      | `AllEmpty ->
		  (* Peek at the next event: *)
		  let ev' = get_ev() in
		  let do_minimize =
		    match ev' with
		      | Some(E_end_tag (name', _)) ->
			  name = name'
		      | _ -> 
			  false in
		  if do_minimize then
		    (true, None)   (* ==> consume ev'! *)
		  else
		    (false, Some ev') in
	  if do_display then
	    display_start_tag name atts scope_opt minimized
	  else
	    write_start_tag name atts scope_opt minimized;
	  wr_dsp_event ev_opt
      | Some (E_end_tag (_, _)) ->
	  write_end_tag();
	  wr_dsp_event None
      | Some (E_char_data data) ->
	  write_data_string ~from_enc:rep_enc ~to_enc:enc out data;
	  wr_dsp_event None
      | Some (E_pinstr (target, value, ent_id)) ->
	  wms "<? "; wms target; wms " "; wms value; wms "?>";
	  wr_dsp_event None
      | Some (E_pinstr_member (target, value, ent_id)) ->
	  wms "<? "; wms target; wms " "; wms value; wms "?>";
	  wr_dsp_event None
      | Some (E_comment data) ->
	  wms "<!--"; wms data; wms "-->";
	  wr_dsp_event None
      | Some E_start_super ->
	  wr_dsp_event None
      | Some E_end_super ->
	  wr_dsp_event None
      | Some (E_position (_,_,_)) ->
	  wr_dsp_event None
      | Some (E_error exn) ->
	  failwith "Pxp_event.write/display: Cannot output E_error event"
      | Some E_end_of_stream ->
	  wr_dsp_event None
      | None ->
	  ()
  in

  wr_dsp_event None
;;


let write_events ?default ?(dtd_style = `Include) ?(minimization=`None) = 
  wr_dsp false default dtd_style minimization ;;
let display_events ?(dtd_style = `Include) ?(minimization=`None) = 
  wr_dsp true None dtd_style minimization ;;


let string_of_event e =
  match e with
    | E_start_doc(v,dtd) ->
	sprintf "E_start_doc(%s,<%d>)\n" v (Oo.id dtd)
    | E_end_doc ->
	"E_end_doc\n"
    | E_start_tag(name,attlist,scope_opt,entid) ->
	sprintf "E_start_tag(%s,%s,%s,<%d>)"
	  name 
	  (String.concat " " (List.map (fun (n,v) -> n ^ "=" ^ v) attlist))
	  (match scope_opt with
	     | None -> "None"
	     | Some scope -> sprintf "<%d>" (Oo.id scope)
	  )
	  (Oo.id entid)
    | E_end_tag(name,entid) ->
	sprintf "E_end_tag(%s,<%d>)" name (Oo.id entid)
    | E_start_super ->
	"E_start_super"
    | E_end_super ->
	"E_end_super"
    | E_char_data data ->
	sprintf "E_char_data(\"%s\")" (String.escaped data)
    | E_pinstr(target,data,entid) ->
	sprintf "E_pinstr(%s,%s,<%d>)" target data (Oo.id entid)
    | E_pinstr_member(target,data,entid) ->
	sprintf "E_pinstr_member(%s,%s,<%d>)" target data (Oo.id entid)
    | E_comment data ->
	sprintf "E_comment(\"%s\")" (String.escaped data)
    | E_position(ent,line,col) ->
	sprintf "E_position(%s,%d,%d)" ent line col
    | E_error e ->
	sprintf "E_error(%s)" (Pxp_types.string_of_exn e)
    | E_end_of_stream ->
	"E_end_of_stream\n"
;;
