(* $Id: ds_app.ml,v 1.5 2000/07/08 22:03:11 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Tk
open Pxp_types
open Pxp_document
open Pxp_yacc
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
  let style =
    parse_document_entity
      default_config
      (from_file !style_sysid)
      tag_map
  in
  let root = style # root in
  root # extension # prepare;

  let obj_dtd =
    parse_dtd_entity
      default_config
      (from_file object_dtd_sysid)
  in
  obj_dtd # set_root object_dtd_root;

  let topframe = openTk() in
  let context = new context filename obj_dtd root topframe in

  Toplevel.configure topframe [ Width (Centimeters 20.0);
                                Height (Centimeters 12.0);
                              ];
  Pack.propagate_set topframe false;
  Wm.title_set topframe cmd;
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

(* ======================================================================
 * History:
 *
 * $Log: ds_app.ml,v $
 * Revision 1.5  2000/07/08 22:03:11  gerd
 * 	Updates because of PXP interface changes.
 *
 * Revision 1.4  2000/06/04 20:29:19  gerd
 * 	Updates because of renamed PXP modules.
 *
 * Revision 1.3  2000/05/01 16:48:45  gerd
 * 	Using the new error formatter.
 *
 * Revision 1.2  1999/12/17 21:34:29  gerd
 * 	The name of the root element is set to "record" in the
 * object_dtd; otherwise the parser would not check that the root
 * element is the right element.
 *
 * Revision 1.1  1999/08/21 19:11:05  gerd
 * 	Initial revision.
 *
 *
 *)
