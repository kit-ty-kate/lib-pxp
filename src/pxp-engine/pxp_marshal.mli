(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* TODO: 
 * - namespace_info
 *)


(* This module allows fast marshalling of subtrees. The standard marshalling
 * implementation does not work because O'Caml does not support marshalling
 * of objects. Because of this, the objects must be transformed into a
 * representation for which no marshalling restriction applies.
 * 
 * The subtree is converted into a sequence of reconstruction_cmd values
 * which can be marshaled using the standard implementation.
 *
 * While the tree is written or read it is possible to change the character
 * encoding. Furthermore, the namespace prefixes can be changed to other
 * conventions.
 *)

type reconstruction_cmd

val subtree_to_cmd_sequence : 
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
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
   *
   * ~enc: if passed, the character encoding is changed to this type. If 
   *   omitted, the written sequence is encoded in the same manner as the
   *   node tree.
   *)

val subtree_to_channel : 
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
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
   *
   * ~enc: if passed, the character encoding is changed to this type. If 
   *   omitted, the written sequence is encoded in the same manner as the
   *   node tree.
   *)

val document_to_cmd_sequence :
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
      (reconstruction_cmd -> unit) -> 
      'ext Pxp_document.document ->
	  unit
val document_to_channel :
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
      out_channel ->
      'ext Pxp_document.document ->
	  unit
  (* The same for documents. *)

val subtree_from_cmd_sequence : 
      (unit -> reconstruction_cmd) ->
      Pxp_dtd.dtd ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.node
  (* Reconstructs the subtree from a sequence of reconstruction_cmd values.
   * The passed function is called to get the next reconstruction_cmd.
   *
   * If the DTD contains a namespace_manager:
   *   You must pass a namespace_manager to enable the namespace code.
   *   Note that the normprefixes found in the input stream are remapped
   *   to unique normprefixes, if this is necessary. This means that the
   *   namespace_manager should be filled with (normprefix, uri) pairs
   *   if you want to ensure that certain normprefixes are used.
   *     If you pass an empty namespace_manager, it is guaranteed that
   *   such remapping is not necessary, so the normprefixes are the same
   *   as in the original document.
   *
   * The character encoding of the node tree is set to the encoding of the
   * DTD. If necessary, the read strings are recoded.
   *)

val subtree_from_channel : 
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
  (* The same for documents. 
   *
   * The character encoding of the node tree is set to the encoding of the
   * configuration. If necessary, the read strings are recoded.
   *)

val relocate_subtree : 
  'ext_a Pxp_document.node ->
  Pxp_dtd.dtd ->
  'ext_b Pxp_document.spec ->
    'ext_b Pxp_document.node
  (* Creates a copy of the passed subtree by marshalling the tree, and
   * restoring the marshaled tree. The new tree will have the passed DTD
   * and the passed spec, i.e. this function can _change_ the DTD and the
   * spec of an existing tree. Note that you can also change the type of
   * the extensions.
   * This function is optimized, and works block by block in order to avoid
   * large temporary values.
   *)

val relocate_document :
  'ext_a Pxp_document.document ->
  Pxp_yacc.config ->
  'ext_b Pxp_document.spec ->
    'ext_b Pxp_document.document
  (* Creates a copy of the passed document by marshalling it, and
   * restoring the document. The new document will have a copy of the
   * original DTD, and a copy of the XML tree that will have been created
   * according to the passed spec. The new configuration is used when
   * building the new document, so it is possible to change the character
   * encoding and the namespace management.
   *
   * KNOWN BUG: The new DTD is not really a copy, because the entities are
   * missing. This will be solved when it is possible to copy entities.
   *)


