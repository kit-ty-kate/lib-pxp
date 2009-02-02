(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Tk
open Pxp_types
open Pxp_document
open Pxp_tree_parser
open Ds_context
open Ds_style


let installdir       =
  try Sys.getenv "DATASHEETS" with
      Not_found -> "/opt/xmlforms/lib"
let style_sysid      = ref ""
let object_dtd_sysid = Filename.concat installdir "ds-object.dtd"
let object_dtd_root  = "record" 


let rec print_error e =
  print_endline (string_of_exn e)
;;


let run f arg1 arg2 =
  try f arg1 arg2 with
      e -> print_error e
;;


let edit filename cmd =
  (* read in style definition *)
  let index = new hash_index in
  let style =
    parse_document_entity
      ~id_index:(index :> 'ext index)
      default_config
      (from_file !style_sysid)
      tag_map
  in
  let root = style # root in
  root # extension # prepare (index :> 'ext index);

  let obj_dtd =
    Pxp_dtd_parser.parse_dtd_entity
      default_config
      (from_file object_dtd_sysid)
  in
  obj_dtd # set_root object_dtd_root;

  let topframe = openTk() in
  let topframe_frame = Frame.create ~borderwidth:0 topframe in
  let context = new context filename obj_dtd index root topframe_frame in

  Toplevel.configure ~width:(Tk.pixels (`Cm 20.0))
                     ~height:(Tk.pixels (`Cm 12.0)) topframe;
  Pack.propagate_set topframe false;
  Wm.title_set topframe cmd;
  pack [topframe_frame];
  context # goto (root # extension # start_node_name);
  mainLoop()
;;


let main() =
  let cmd = Filename.basename Sys.argv.(0) in
  match Sys.argv with
      [| _; filename |] ->
	style_sysid := Filename.concat installdir (cmd ^ "-style.xml");
	run edit filename cmd
    | _ ->
	prerr_endline ("usage: " ^ cmd ^ " filename");
	exit(1)
;;

main();;

