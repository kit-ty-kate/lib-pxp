(* $Id: ds_app.ml,v 1.1 1999/08/21 19:11:05 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Tk
open Markup_types
open Markup_document
open Markup_yacc
open Ds_context
open Ds_style


let installdir       =
  try Sys.getenv "DATASHEETS" with
      Not_found -> "/opt/xmlforms/lib"
let style_sysid      = ref ""
let object_dtd_sysid = Filename.concat installdir "ds-object.dtd"


let rec print_error e =
  match e with
      Markup_types.At(where,what) ->
        print_endline where;
        print_error what
    | _ ->
        print_endline (Printexc.to_string e)
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
      (File !style_sysid)
      tag_map
  in
  let root = style # root in
  root # extension # prepare;

  let obj_dtd =
    parse_dtd_entity
      default_config
      (File object_dtd_sysid)
  in

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
 * Revision 1.1  1999/08/21 19:11:05  gerd
 * 	Initial revision.
 *
 *
 *)
