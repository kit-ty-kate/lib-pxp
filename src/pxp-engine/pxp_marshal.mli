(* $Id: pxp_marshal.mli,v 1.2 2001/06/07 22:47:31 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* TODO: 
 * - namespace_info
 * - new function "relocate" that maps a tree on a second tree, but the
 *   second tree can have a different type for extensions, and a different
 *   namespace manager. (I need some better understanding of how to use
 *   continuations in O'Caml.)
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
      (reconstruction_cmd -> unit) ->
      'ext Pxp_document.node ->
          unit
  (* The passed node is the root of the subtree to be marshaled. The function
   * is called several times with the reconstruction_cmd values which
   * contain the contents of the subtree.
   *   If the subtree has a namespace manager, the information contained
   * in this object is marshaled, too. However, the namespace_info object
   * is not represented in the output stream (if any). This means that
   * the stream contains all namespace URIs, but not the original prefixes
   * (source prefixes). This limitation is practically meaningless
   * (who wants to know source prefixes?).
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
   *    (Info about namespaces: see subtree_to_cmd_sequence.)
   *
   * ~omit_positions: If true, the position strings of the nodes which contain
   *   line numbers are omitted. Default: false
   *)

val document_to_cmd_sequence :
      ?omit_positions:bool ->
      (reconstruction_cmd -> unit) -> 
      'ext Pxp_document.document ->
	  unit
val document_to_channel :
      ?omit_positions:bool ->
      out_channel ->
      'ext Pxp_document.document ->
	  unit
  (* The same for documents. *)

val subtree_from_cmd_sequence : 
      ?enable_namespace_processing:Pxp_document.namespace_manager ->
      (unit -> reconstruction_cmd) ->
      Pxp_dtd.dtd ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.node
  (* Reconstructs the subtree from a sequence of reconstruction_cmd values.
   * The passed function is called to get the next reconstruction_cmd.
   *
   * enable_namespace_processing: (default: None)
   *   You must pass a namespace_manager to enable the namespace code.
   *   Note that the normprefixes found in the input stream are remapped
   *   to unique normprefixes, if this is necessary. This means that the
   *   namespace_manager should be filled with (normprefix, uri) pairs
   *   if you want to ensure that certain normprefixes are used.
   *     If you pass an empty namespace_manager, it is guaranteed that
   *   such remapping is not necessary, so the normprefixes are the same
   *   as in the original document.
   *)

val subtree_from_channel : 
      ?enable_namespace_processing:Pxp_document.namespace_manager ->
      in_channel ->
      Pxp_dtd.dtd ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.node
  (* Reconstructs the subtree from an in_channel. *)

val document_from_cmd_sequence :
      (unit -> reconstruction_cmd) ->
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
 * Revision 1.2  2001/06/07 22:47:31  gerd
 * 	The new support for namespaces is reflected in the signature:
 * New option enable_namespace_processing.
 *
 * Revision 1.1  2000/09/17 00:10:32  gerd
 * 	Initial revision.
 *
 * 
 *)
