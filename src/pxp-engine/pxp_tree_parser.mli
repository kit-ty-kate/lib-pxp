(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types
open Pxp_dtd
open Pxp_document


exception ID_not_unique        (* see class index below *)

class type [ 'ext ] index =
object 
  (* The type of indexes over the ID attributes of the elements. This type
   * is the minimum requirement needed by the parser to create such an index.
   *)
  constraint 'ext = 'ext node #extension
  method add : string -> 'ext node -> unit
    (* Add the passed node to the index. If there is already an ID with
     * the passed string value, the exception ID_not_unique should be
     * raised. (But the index is free also to accept several identical IDs.)
     *)
  method find : string -> 'ext node
    (* Finds the node with the passed ID value, or raises Not_found *)
end



class [ 'ext ] hash_index : 
object 
  (* This is a simple implementation of 'index' using a hash table. *)
  constraint 'ext = 'ext node #extension
  method add : string -> 'ext node -> unit
    (* See above. *)
  method find : string -> 'ext node
    (* See above. *)
  method index : (string, 'ext node) Hashtbl.t
    (* Returns the hash table. *)
end

val default_extension : ('a node extension) as 'a
  (* A "null" extension; an extension that does not extend the functionality *)

val default_spec : ('a node extension as 'a) spec
  (* Specifies that you do not want to use extensions. *)

val default_namespace_spec : ('a node extension as 'a) spec
  (* Specifies that you want to use namespace, but not extensions *)


val parse_document_entity : 
  ?transform_dtd:(dtd -> dtd) ->
  ?id_index:('ext index) ->
  config -> source -> 'ext spec -> 'ext document
  (* Parse a closed document, i.e. a document beginning with <!DOCTYPE...>,
   * and validate the contents of the document against the DTD contained
   * and/or referenced in the document.
   *
   * If the optional argument ~transform_dtd is passed, the following 
   * modification applies: After the DTD (both the internal and external
   * subsets) has been parsed, the function ~transform_dtd is called,
   * and the resulting DTD is actually used to validate the document.
   *
   * If the optional argument ~transform_dtd is missing, the parser
   * behaves in the same way as if the identity were passed as ~transform_dtd.
   *
   * If the optional argument ~id_index is present, the parser adds
   * any ID attribute to the passed index. An index is required to detect
   * violations of the uniqueness of IDs.
   *)

val parse_wfdocument_entity : 
  ?transform_dtd:(dtd -> dtd) ->
  config -> source -> 'ext spec -> 'ext document
  (* Parse a closed document (see parse_document_entity), but do not
   * validate it. Only checks on well-formedness are performed.
   *
   * The option ~transform_dtd works as for parse_document_entity,
   * but the resulting DTD is not used for validation. It is just
   * included into the returned document (e.g. useful to get entity 
   * declarations).
   *)

val parse_content_entity  : 
  ?id_index:('ext index) ->
  config -> source -> dtd -> 'ext spec -> 'ext node
  (* Parse a file representing a well-formed fragment of a document. The
   * fragment must be a single element (i.e. something like <a>...</a>;
   * not a sequence like <a>...</a><b>...</b>). The element is validated
   * against the passed DTD, but it is not checked whether the element is
   * the root element specified in the DTD.
   *
   * If the optional argument ~id_index is present, the parser adds
   * any ID attribute to the passed index. An index is required to detect
   * violations of the uniqueness of IDs.
   *)

val parse_wfcontent_entity : 
  config -> source -> 'ext spec -> 'ext node
  (* Parse a file representing a well-formed fragment of a document
   * (see parse_content_entity). The fragment is not validated, only
   * checked for well-formedness.
   *)


