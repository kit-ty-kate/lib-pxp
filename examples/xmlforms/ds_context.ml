(* $Id: ds_context.ml,v 1.6 2000/07/23 20:25:05 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_document
open Pxp_yacc

let empty_record = new element_impl (Pxp_yacc.default_extension);;
let empty_dnode = new data_impl Pxp_yacc.default_extension;;

class context the_filename the_obj_dtd the_index the_root the_topframe =
  object (self)
    val filename = the_filename
    val obj_dtd = the_obj_dtd
    val node_index = the_index
    val mutable obj = empty_record # create_element
			the_obj_dtd (T_element "record") []
    val root = the_root
    val topframe = the_topframe
    val mutable wdg = None

    val mutable history = ( [| |] : string array )
    val mutable index = 0

    initializer
      self # load_obj

    method obj = obj

    (* history *)

    method private leave_node =
      begin match wdg with
	  None -> ()
	| Some w -> Tk.destroy w
      end;
      wdg <- None

    method private enter_node =
      let where = history.(index) in
      let n =
	try node_index # find where with
	    Not_found -> failwith ("Mask not found: " ^ where) in
      let w = n # extension # create_widget topframe self in
      Tk.pack [w] (n # extension # pack_opts @ [ Tk.Expand true] );
      wdg <- Some w



    method previous =
      if index > 0 then
	index <- index - 1
      else
	raise Not_found;
      self # leave_node;
      self # enter_node;


    method next =
      if index < Array.length history - 1 then
	index <- index + 1
      else
	raise Not_found;
      self # leave_node;
      self # enter_node;


    method goto where =
      assert (index <= Array.length history);
      self # leave_node;
      let persisting_history =
	if index < Array.length history then
	  Array.sub history 0 (index+1)
	else
	  history
      in
      history <- Array.concat [ persisting_history; [| where |] ];
      index <- Array.length history - 1;
      self # enter_node;


    method current =
      if index < Array.length history then
	history.(index)
      else
	raise Not_found


    (* read, write the slots of object *)

    method search_slot name =
      let rec search n =
	match n # node_type with
	    T_element "string" ->
	      if n # required_string_attribute "name" = name then
		n
	      else raise Not_found
	  | T_element _ ->
	      search_list (n # sub_nodes)
	  | T_data ->
	      raise Not_found
	      
       and search_list l =
         match l with
	     x :: l' ->
	       (try search x with Not_found -> search_list l')
 	   | [] ->
	       raise Not_found
      in
      search obj

    method get_slot name =
      let d = (self # search_slot name) # data in
      d

    method set_slot name value =
      let dtd = obj # dtd in
      begin try
	let n = self # search_slot name in
	n # delete
      with
	  Not_found -> ()
      end;
      let e_string = empty_record # create_element dtd (T_element "string")
		[ "name", name ] in
      let dnode = empty_dnode # create_data dtd value in
      e_string # add_node dnode;
      e_string # local_validate();
      obj # add_node e_string;
      assert(self # get_slot name = value)

    (* load, save object *)


    method load_obj =
      if Sys.file_exists filename then begin
	obj <- parse_content_entity
	  default_config
	  (from_file filename)
	  obj_dtd
	  default_spec
      end
      else begin
	print_string "New file!\n";
	flush stdout
      end


    method save_obj =
      let fd = open_out filename in
      try

	let re1 = Str.regexp "&" in
	let re2 = Str.regexp "<" in
	let re3 = Str.regexp "'" in
	let re4 = Str.regexp ">" in
	let protect s =
	  let s1 = Str.global_replace re1 "&amp;" s in
	  let s2 = Str.global_replace re2 "&lt;" s1 in
	  let s3 = Str.global_replace re3 "&apos;" s2 in
	  let s4 = Str.global_replace re2 "&gt;" s1 in
	  s3
	in

	let rec iterate (n : 'node extension node as 'node) =
	  match n # node_type with
	      T_data ->
		output_string fd (protect (n # data))
	    | T_element name ->
		output_string fd ("<" ^ name ^ "\n");
		let anames = n # attribute_names in
		List.iter
		  (fun aname ->
		     let aval = n # attribute aname in
		     let v =
		       match aval with
			   Value s ->
			     aname ^ "='" ^ protect s ^ "'\n"
			 | Valuelist l ->
			     aname ^ "='" ^ String.concat " " (List.map protect l) ^ "'\n"
			 | Implied_value ->
			     ""
		     in
		     output_string fd v)
		  anames;
		output_string fd ">";
		List.iter iterate (n # sub_nodes);
		output_string fd ("</" ^ name ^ "\n>");
	in

	output_string fd "<?xml version='1.0' encoding='ISO-8859-1'?>\n";
	iterate obj;
	close_out fd
      with
	  e ->
	    close_out fd;
	    raise e

  end
;;


(* ======================================================================
 * History:
 *
 * $Log: ds_context.ml,v $
 * Revision 1.6  2000/07/23 20:25:05  gerd
 * 	Update because of API change: local_validate.
 *
 * Revision 1.5  2000/07/16 19:36:03  gerd
 * 	Updated.
 *
 * Revision 1.4  2000/07/08 22:03:11  gerd
 * 	Updates because of PXP interface changes.
 *
 * Revision 1.3  2000/06/04 20:29:19  gerd
 * 	Updates because of renamed PXP modules.
 *
 * Revision 1.2  2000/05/30 00:09:08  gerd
 * 	Minor fix.
 *
 * Revision 1.1  1999/08/21 19:11:05  gerd
 * 	Initial revision.
 *
 *
 *)
