(* $Id: pxp_marshal.mli,v 1.1 2000/09/17 00:10:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


(* This module allows fast marshalling of subtrees. The standard marshalling
 * implementation does not work because O'Caml does not support marshalling
 * of objects. Because of this, the objects must be transformed into a
 * representation for which no marshalling restriction applies.
 * 
 * The subtree is converted into a sequence of reconstruction_cmd values
 * which can be marshaled using the standard implementation.
 *)

type reconstruction_cmd

val subtree_to_cmd_sequence : 
      ?omit_positions:bool ->
      f:(reconstruction_cmd -> unit) -> 
      'ext Pxp_document.node ->
          unit
  (* The passed node is the root of the subtree to be marshaled. The function
   * ~f is called several times with the reconstruction_cmd values which
   * contain the contents of the subtree.
   *
   * ~omit_positions: If true, the position strings of the nodes which contain
   *   line numbers are omitted. Default: false
   *)

val subtree_to_channel : 
      ?omit_positions:bool ->
      out_channel -> 
      'ext Pxp_document.node -> 
          unit
  (* The passed node is the root of the subtree to be marshaled; the external
   * representation is written to the out_channel (which must have been opened
   * in binary mode).
   *
   * ~omit_positions: If true, the position strings of the nodes which contain
   *   line numbers are omitted. Default: false
   *)

val document_to_cmd_sequence :
      ?omit_positions:bool ->
      f:(reconstruction_cmd -> unit) -> 
      'ext Pxp_document.document ->
	  unit
val document_to_channel :
      ?omit_positions:bool ->
      out_channel ->
      'ext Pxp_document.document ->
	  unit
  (* The same for documents. *)

val subtree_from_cmd_sequence : 
      f:(unit -> reconstruction_cmd) ->
      Pxp_dtd.dtd ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.node
  (* Reconstructs the subtree from a sequence of reconstruction_cmd values.
   * The function ~f is called to get the next reconstruction_cmd.
   *)

val subtree_from_channel : 
      in_channel ->
      Pxp_dtd.dtd ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.node
  (* Reconstructs the subtree from an in_channel. *)

val document_from_cmd_sequence :
      f:(unit -> reconstruction_cmd) ->
      Pxp_yacc.config ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.document
val document_from_channel :
      in_channel ->
      Pxp_yacc.config ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.document
  (* The same for documents. *)


(* ======================================================================
 * History:
 * 
 * $Log: pxp_marshal.mli,v $
 * Revision 1.1  2000/09/17 00:10:32  gerd
 * 	Initial revision.
 *
 * 
 *)
