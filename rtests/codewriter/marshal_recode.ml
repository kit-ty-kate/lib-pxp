(* $Id: marshal_recode.ml,v 1.2 2003/06/22 15:10:52 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_yacc
open Pxp_document
open Pxp_types

let conf =
  { default_config with
      enable_pinstr_nodes = true;
      enable_super_root_node = true;
      enable_comment_nodes = true;
      encoding = `Enc_iso88591;
  };;

let conf' =
  { conf with
      encoding = `Enc_utf8;
  };;

let main() =
  let doc = parse_document_entity
	      conf
	      (from_file "sample003.xml")
	      default_spec in
  let out1 = open_out "sample003.xml.out1" in
  doc # write (`Out_channel out1) `Enc_utf8;
  close_out out1;

  let doc' = Pxp_marshal.relocate_document doc conf' default_spec in
  let out2 = open_out "sample003.xml.out2" in
  doc' # write (`Out_channel out2) `Enc_utf8;
  close_out out2;

  assert(Sys.command "cmp sample003.xml.out1 sample003.xml.out2" = 0);
  
  ()
;;

try
  main()
with
    exn ->
      prerr_endline("ERROR: " ^ string_of_exn exn);
      exit 1
;;





(* ======================================================================
 * History:
 * 
 * $Log: marshal_recode.ml,v $
 * Revision 1.2  2003/06/22 15:10:52  gerd
 * 	Updated
 *
 * Revision 1.1  2002/03/10 23:44:40  gerd
 * 	Initial revision.
 *
 * 
 *)
