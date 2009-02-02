(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_document
open Pxp_tree_parser

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
      (* Tk.pack [w] (n # extension # pack_opts @ [ Tk.Expand true] ); *) (*X*)
      n # extension # pack 
	?expand:(Some true) ?anchor:None ?fill:None ?side:None 
	[Widget.forget_type w];
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
	  | _ ->
	      assert false
	      
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
	output_string fd "<?xml version='1.0' encoding='ISO-8859-1'?>\n";
        obj # write (`Out_channel fd) `Enc_iso88591;
	close_out fd
      with
	  e ->
	    close_out fd;
	    raise e

  end
;;


