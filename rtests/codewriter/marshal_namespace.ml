(* $Id: marshal_namespace.ml,v 1.3 2001/06/28 21:24:36 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


(* This is not a good test whether marshalling works. It does:
 * (a) serializes a document (sample002.d1), then unserializes it
 * (b) serializes the output of (a) (sample002.d2), then unserializes it
 * (c) serializes the output of (b) (sample002.d3)
 * After that, it is checked whether the serialized document of (a) is
 * equal to the output of (c).
 * However, if information is lost during serialization/unserialization,
 * this test cannot detect the loss. Because of this, a fourth file is
 * written: sample002.xml.out (output of (a))
 *)

open Pxp_yacc
open Pxp_document
open Pxp_types
open Pxp_dtd

let conf =
  { default_config with
      enable_pinstr_nodes = true;
      enable_super_root_node = true;
      enable_comment_nodes = true;
      enable_namespace_processing = Some (new namespace_manager);
      encoding = `Enc_utf8;
  };;

let spec = default_namespace_spec;;

let main() =
  let doc = parse_document_entity
	      conf
	      (from_file "sample002.xml")
	      spec in
  
  let out = open_out_bin "sample002.d1" in
  Pxp_marshal.document_to_channel out doc;
  close_out out;
  
  let inc = open_in_bin "sample002.d1" in
  let doc' = Pxp_marshal.document_from_channel inc conf spec in
  close_in inc;

  let xmlout = open_out "sample002.xml.out" in
  doc' # write (`Out_channel xmlout) `Enc_utf8;
  close_out xmlout;
  
  let out' = open_out_bin "sample002.d2" in
  Pxp_marshal.document_to_channel out' doc';
  close_out out';
  
  let inc' = open_in_bin "sample002.d2" in
  let doc'' = Pxp_marshal.document_from_channel inc' conf spec in
  close_in inc';

  let out'' = open_out_bin "sample002.d3" in
  Pxp_marshal.document_to_channel out'' doc'';
  close_out out'';

  assert(Sys.command "cmp sample002.d1 sample002.d3" = 0);
  
  ()
;;

try
  main()
with
    ex ->
      prerr_endline ("Error happened:\n" ^ string_of_exn ex);
      exit 1
;;


(* ======================================================================
 * History:
 * 
 * $Log: marshal_namespace.ml,v $
 * Revision 1.3  2001/06/28 21:24:36  gerd
 * 	Out_channel -> `Out_channel
 *
 * Revision 1.2  2001/06/08 01:16:55  gerd
 * 	Updated.
 *
 * Revision 1.1  2001/06/07 20:14:47  gerd
 * 	Added tests for marshalling and namespaces.
 *
 * 
 *)
