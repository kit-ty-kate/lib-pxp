(* $Id: marshal_simple.ml,v 1.1 2001/06/07 20:14:47 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


(* This is not a good test whether marshalling works. It does:
 * (a) serializes a document (sample001.d1), then unserializes it
 * (b) serializes the output of (a) (sample001.d2), then unserializes it
 * (c) serializes the output of (b) (sample001.d3)
 * After that, it is checked whether the serialized document of (a) is
 * equal to the output of (c).
 * However, if information is lost during serialization/unserialization,
 * this test cannot detect the loss. Because of this, a fourth file is
 * written: sample001.xml.out (output of (a))
 *)

open Pxp_yacc
open Pxp_document
open Pxp_types

let conf =
  { default_config with
      enable_pinstr_nodes = true;
      enable_super_root_node = true;
      enable_comment_nodes = true;
      encoding = `Enc_utf8;
  };;

let main() =
  let doc = parse_document_entity
	      conf
	      (from_file "sample001.xml")
	      default_spec in
  
  let out = open_out_bin "sample001.d1" in
  Pxp_marshal.document_to_channel out doc;
  close_out out;
  
  let inc = open_in_bin "sample001.d1" in
  let doc' = Pxp_marshal.document_from_channel inc conf default_spec in
  close_in inc;

  let xmlout = open_out "sample001.xml.out" in
  doc' # write (Out_channel xmlout) `Enc_utf8;
  close_out xmlout;
  
  let out' = open_out_bin "sample001.d2" in
  Pxp_marshal.document_to_channel out' doc';
  close_out out';
  
  let inc' = open_in_bin "sample001.d2" in
  let doc'' = Pxp_marshal.document_from_channel inc' conf default_spec in
  close_in inc';

  let out'' = open_out_bin "sample001.d3" in
  Pxp_marshal.document_to_channel out'' doc'';
  close_out out'';

  assert(Sys.command "cmp sample001.d1 sample001.d3" = 0);
  
  ()
;;


main();;

(* ======================================================================
 * History:
 * 
 * $Log: marshal_simple.ml,v $
 * Revision 1.1  2001/06/07 20:14:47  gerd
 * 	Added tests for marshalling and namespaces.
 *
 * 
 *)
