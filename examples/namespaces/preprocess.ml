(* $Id *)

open Pxp_document
open Pxp_yacc
open Pxp_types

exception User_error of string

let user_error (where,line,pos) subject =
  let message =
    (if line <> 0 then
       "In " ^ where ^ ", at line " ^ string_of_int line ^ " position " ^ 
       string_of_int pos ^ ":\n"
     else
       ""
    )
    ^ subject
  in
  raise(User_error message)
;;


let macro_use_uri = "http://www.ocaml-programming.de/macro/use" ;;

let collect_macro_define tree =
  let container = Hashtbl.create 10 in
  (* Collect all macro:define elements and put them into [container].
   * Remove these elements from the tree.
   *)
  iter_tree
    ~pre:(fun node ->
	    if node # node_type = T_element "macro:define" then begin
	      let name = node # required_string_attribute "name" in
	      if Hashtbl.mem container name then
		user_error 
		  (node # position)
		  "Macro already defined";
	      if List.length (node # sub_nodes) <> 1 then
		user_error
		  (node # position)
		  "Macro must contain exactly one subnode";
	      node # remove();
	      Hashtbl.add container name (node # nth_node 0);
	      raise Skip        (* Do not iterate over the children of [node] *)
	    end
	 )
    tree;
  container
;;


let parameter_re = Str.regexp "\\$[-a-zA-Z0-9_.]+";;

let replace_macro_use container tree =
  let replace_params position data atts =
    let splitted = Str.full_split parameter_re data in
    let splitted' =
      List.map
	(function
	     Str.Text s  -> s
	   | Str.Delim s ->
	       let param_name = String.sub s 1 (String.length s - 1) in
	       ( try
		   let param_value = 
		     List.assoc param_name atts in   (* or Not_found *)
		   match param_value with
		       Value v -> v
		     | _       -> assert false
	         with
		       Not_found ->
			 user_error
			   position
			   ("Parameter " ^ param_name ^ " not found")
	       )
	)
	splitted in
    String.concat "" splitted'
  in

  let replace_use position name atts = 
    try
      let subst = Hashtbl.find container name in   (* or Not_found *)
      (* Make a copy of [subst], and replace the parameters: *)
      map_tree
	~pre:(fun node -> node # orphaned_flat_clone)
	~post:(fun node ->
		 match node # node_type with
		     T_data ->
		       (* It is possible that the data node contains
			* parameters
			*)
		       let data = node # data in
		       let data' = replace_params
				     (node # position)
				     data
				     atts in
		       node # set_data data';
		       node
		   | _ ->
		       node
	      )
	subst
    with
	Not_found ->
	  user_error position ("Macro not found: " ^ name)
  in
  (* Make a copy of [tree], and replace the macro calls: *)
  map_tree
    ~pre:(fun node -> node # orphaned_flat_clone)
    ~post:(fun node ->
	     match node # node_type with
		 T_element "macro:use" ->
		   let name = node # required_string_attribute "name" in
		   let atts = node # attributes in
		   let atts' = List.remove_assoc "name" atts in
		   replace_use (node # position) name atts'
	       | T_element _ when node # namespace_uri = macro_use_uri ->
		   let name = node # localname in
		   replace_use (node # position) name (node # attributes)
	       | _ ->
		   node
	  )
    tree
;;


let read_macro_dtd config =
  parse_dtd_entity config (from_file "macro.dtd")
;;


let copy_general_entities dtd1 dtd2 =
  (* Copy the general entities from dtd1 to dtd2 *)
  let names = dtd1 # gen_entity_names in
  List.iter
    (fun name ->
       let ent, is_external = dtd1 # gen_entity name in
       dtd2 # add_gen_entity ent is_external
    )
    names
;;


let transform config dtd filename =
  (* Read the document: *)
  let found_dtd_id = ref None in
  let found_root = ref None in
  let doc = parse_document_entity
	      ~transform_dtd:(fun found_dtd -> 
				(* Save the DTD ID: *)
				found_dtd_id := found_dtd # id;
				found_root := found_dtd # root;
				(* Copy general entities to [dtd]: *)
				copy_general_entities found_dtd dtd;
				(* Replace the found DTD by this one: *)
				dtd)
	      config
	      (from_file filename)
	      default_namespace_spec in
  let root = doc # root in
  (* Collect the macro definitions: 
   * (As a side effect, remove the definitions from the tree [root].)
   *)
  let definitions = collect_macro_define root in
  (* Expand the macro calls: *)
  let root' = replace_macro_use definitions root in
  (* Output the result: *)
  let root_element = find (fun node ->
			     match node # node_type with
				 T_element _ -> true 
			       | _ -> false
			  ) 
		          root' in
  let default_prefix = root_element # normprefix in
  print_string "<?xml version='1.0'?>\n";
  (* Output the DOCTYPE line, if needed. This is a bit delicate. *)
  ( match !found_dtd_id with
	Some (External _)
      | Some (Derived _) ->
	  let id = (match !found_dtd_id with
			Some (External x) -> x
		      | Some (Derived x) -> x
		      | _ -> assert false
		   ) in
	  (* If the id is Derived, the internal subset of the DTD is
	   * silently dropped.
	   *)
	  print_string "<!DOCTYPE ";
	  ( match root_element # node_type with
		T_element r -> 
		  (* Remove the default prefix: *)
		  let p, l = Pxp_aux.namespace_split r in
		  print_string (if p = default_prefix then l else r);
	      | _           -> assert false
	  );
	  ( match id with
		System sysid ->
		  print_string " SYSTEM \"";
		  print_string sysid;
		  print_string "\""
	      | Public(pubid,sysid) ->
		  print_string " PUBLIC \"";
		  print_string pubid;
		  print_string "\" \"";
		  print_string sysid;
		  print_string "\"";
	      | _ ->
		  assert false
	  );
	  print_string ">\n";
      | Some Internal
      | None ->
	  (* Internal DTDs are silently dropped *)
	  ()
  );
  root' # write ~default:default_prefix (`Out_channel stdout) `Enc_utf8
;;


let main() =
  let filename = ref "" in
  Arg.parse
      []
      (fun s ->
	 if !filename <> "" then
	   raise(Arg.Bad "Please, only one file at once!");
	 filename := s
      )
      "usage: preprocess [ options ] filename";
  if !filename = "" then
    user_error ("",0,0) "No input file";
  let config =
    { default_namespace_config with
	encoding = `Enc_utf8;
	enable_pinstr_nodes = true;
	enable_super_root_node = true;
	enable_comment_nodes = true;
    }
  in
  let dtd = read_macro_dtd config in
  transform config dtd !filename
;;

try
  main() 
with
    User_error message ->
      prerr_endline message;
      exit 1
  | other ->
      prerr_endline (string_of_exn other);
      exit 1
;;
