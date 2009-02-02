(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Marshalling of XML trees *)

(** This module allows fast marshalling of subtrees. The standard O'Caml marshalling
 * implementation does not work because O'Caml does not support marshalling
 * of objects. Because of this, the objects must be transformed into a
 * representation for which no marshalling restriction applies.
 * 
 * The subtree is converted into a sequence of [reconstruction_cmd] values
 * which can be marshaled using the standard implementation.
 *
 * While the tree is written or read it is possible to change the character
 * encoding. Furthermore, the namespace prefixes can be changed to other
 * conventions.
 *)

type reconstruction_cmd
  (** A tree is translated into a sequence of [reconstrucion_cmd] tokens.
      These tokens can be subsequently translated into byte strings using
      the standard [Marshal] module.
   *)

(** {2 Marshalling} *)

val subtree_to_cmd_sequence : 
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
      (reconstruction_cmd -> unit) ->
      'ext Pxp_document.node ->
          unit
  (** [subtree_to_cmd_sequence f n]:
   * The passed node [n] is the root of the subtree to be marshaled. The function
   * [f] is called several times with the sequence of [reconstruction_cmd] values
   * that contain the contents of the subtree.
   * 
   * If the subtree has a namespace manager, the information contained
   * in this object is marshaled, too. The namespace scope objects are
   * also represented in the command sequence.
   *
   * [omit_positions]: If true, the position strings of the nodes which contain
   *   line numbers are omitted. Default: false
   *
   * [enc]: if passed, the character encoding is changed to this type. If 
   *   omitted, the written sequence is encoded in the same manner as the
   *   node tree.
   *)

val subtree_to_channel : 
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
      out_channel -> 
      'ext Pxp_document.node -> 
          unit
  (** [subtree_to_channel ch n]:
   * The node [n] is the root of the subtree to be marshaled; the external
   * representation is written to [ch] (which must have been opened
   * in binary mode).
   *    (Info about namespaces: see subtree_to_cmd_sequence.)
   *
   * [omit_positions]: If true, the position strings of the nodes which contain
   *   line numbers are omitted. Default: false
   *
   * [enc]: if passed, the character encoding is changed to this type. If 
   *   omitted, the written sequence is encoded in the same manner as the
   *   node tree.
   *)

val document_to_cmd_sequence :
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
      (reconstruction_cmd -> unit) -> 
      'ext Pxp_document.document ->
	  unit
  (** The same for documents *)

val document_to_channel :
      ?omit_positions:bool ->
      ?enc:Netconversion.encoding ->
      out_channel ->
      'ext Pxp_document.document ->
	  unit
  (** The same for documents. *)

(** {2 Unmarshalling} *)

val subtree_from_cmd_sequence : 
      (unit -> reconstruction_cmd) ->
      Pxp_dtd.dtd ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.node
  (** Reconstructs the subtree from a sequence of [reconstruction_cmd] values.
   * The passed function is called repeatedly to get the stream of
   * [reconstruction_cmd].
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
   * The namespace scope objects are retained, and thus the display
   * prefixes are the same as in the original tree.
   *
   * The character encoding of the node tree is set to the encoding of the
   * DTD. If necessary, the read strings are recoded.
   *)

val subtree_from_channel : 
      in_channel ->
      Pxp_dtd.dtd ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.node
  (** Reconstructs the subtree from an [in_channel]. *)

val document_from_cmd_sequence :
      (unit -> reconstruction_cmd) ->
      Pxp_yacc.config ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.document
  (** The same for documents *)

val document_from_channel :
      in_channel ->
      Pxp_yacc.config ->
      'ext Pxp_document.spec ->
	  'ext Pxp_document.document
  (** The same for documents. 
   *
   * The character encoding of the node tree is set to the encoding of the
   * configuration. If necessary, the read strings are recoded.
   *)

(** {2 Relocation} *)

(** The term "relocation" is here used for creating a copy of a tree or
    document by printing and reparsing. This can be useful, because other
    model specifications can be used while building the copy (i.e. a different
    set of classes is instantiated for the same elements).

    The following functions avoid expensive reparsing, but take the shortcut
    of decomposing the original tree into [reconstruction_cmd] sequences and
    recombining them afterwards to create the copy.
 *)

val relocate_subtree : 
  'ext_a Pxp_document.node ->
  Pxp_dtd.dtd ->
  'ext_b Pxp_document.spec ->
    'ext_b Pxp_document.node
  (** Creates a copy of the passed subtree by marshalling the tree, and
   * restoring the marshaled tree. The new tree will have the passed DTD
   * and the passed spec, i.e. {b this function can change the DTD and the
   * spec while copying a tree}. Note that you can also change the type of
   * the extensions.
   *
   * This function is optimized, and works block by block in order to avoid
   * large temporary values.
   *
   * See also [relocate_documents] for known problems of relocation.
   *)

val relocate_document :
  'ext_a Pxp_document.document ->
  Pxp_yacc.config ->
  'ext_b Pxp_document.spec ->
    'ext_b Pxp_document.document
  (** Creates a copy of the passed document by marshalling it, and
   * restoring the document. The new document will have a copy of the
   * original DTD, and a copy of the XML tree that will have been created
   * according to the passed spec. The new configuration is used when
   * building the new document, so it is possible to change the character
   * encoding and the namespace management.
   *
   * {b Known problems.} Although it is tried to keep as much information
   * as possible, there are unfortunately a few losses. Especially the
   * entities are not copied. They are missing in the copied DTD object,
   * and also the entity ID's in the nodes are only fake ID's that are
   * not connected with real entities.
   *)


