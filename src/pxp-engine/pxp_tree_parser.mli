(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(** Calling the parser in tree mode *)

(** The following functions return the parsed XML text as tree, i.e.
    as {!Pxp_document.node} or {!Pxp_document.document}.
 *)

open Pxp_dtd
open Pxp_types
open Pxp_document


(** {1 ID indices} *)

(** These indices are used to check the uniqueness of elements declared
    as [ID]. Of course, the indices can also be used to quickly look up
    such elements.
 *)

exception ID_not_unique
  (** Used inside {!Pxp_tree_parser.index} to indicate that the same ID is
      attached to several nodes
   *)

(** The type of indexes over the ID attributes of the elements. This type
 * is the minimum requirement needed by the parser to create such an index.
 *)
class type [ 'ext ] index =
object 
  constraint 'ext = 'ext node #extension
  method add : string -> 'ext node -> unit
    (** Add the passed node to the index. If there is already an ID with
     * the passed string value, the exception {!Pxp_tree_parser.ID_not_unique}
     * can be raised. However, index implementations are free to also accept
     * several identical IDs, although this does not comply to the standard.
     *)
  method find : string -> 'ext node
    (** Finds the node with the passed ID value, or raises [Not_found] *)
end



(** This is a simple implementation of {!Pxp_tree_parser.index} using
    a hash table.
 *)
class [ 'ext ] hash_index : 
object 
  constraint 'ext = 'ext node #extension
  method add : string -> 'ext node -> unit
    (** Add the passed node to the index. If there is already an ID with
     * the passed string value, the exception {!Pxp_tree_parser.ID_not_unique}
     * is raised.
     *)
  method find : string -> 'ext node
    (** Finds the node with the passed ID value, or raises [Not_found] *)
  method index : (string, 'ext node) Hashtbl.t
    (** Returns the hash table mapping IDs to nodes. *)
end


(** {1 Parsing functions} *)

(** There are two types of XML texts one can parse:
     - Closed XML documents
     - External XML entities

    Usually, the functions for closed XML documents are the right ones.
    The exact difference between both types is subtle, as many texts
    are parseable in both ways. The idea, however, is that an external
    XML entity is text from a different file that is included by reference
    into a closed document. Some XML features are only meaningful for
    the whole document, and are not available when only an external entity
    is parsed. This includes:
     - The DOCTYPE and the DTD declarations
     - The standalone declaration

    It is a syntax error to use these features in an external XML entity.

    An external entity is a file referenced by another XML text.
    For example, this document includes "file.xml" as external entity:

    {[
       <?xml version="1.0"?>
       <!DOCTYPE root [
          <!ENTITY extref SYSTEM "file.xml">
       ]>
       <root>
         &extref;
       </root>
     ]}

    (In contrast to this, an internal entity would give the definition
    text immediately, e.g. [<!ENTITY intref "This is the entity text">].)
    Of course, it does not make sense that the external entity has
    another DOCTYPE definition, and hence it is forbidden to use this
    feature in "file.xml".

    There is no function to exactly parse a file like "file.xml"
    as if it was included into a bigger document. The closest behavior show
    {!Pxp_tree_parser.parse_content_entity} and 
    {!Pxp_tree_parser.parse_wfcontent_entity}. They implement the
    additional constraint that the file has to have a single top-most element.

    The following functions also distinguish between validating and
    well-formedness mode. In the latter mode, many formal document
    constraints are not enforced. For instance, elements and
    attributes need not to be declared.

    There are, unfortunately, a number of myths about well-formed XML
    documents. One says that the declarations are completely
    ignored. This is of course not true. For example, the above shown
    example includes the external XML entity "file.xml" by reference.
    The [<!ENTITY>] declaration is respected no matter in which mode
    the parser is run. Also, it is not true that the presence of
    [DOCTYPE] indicates validated mode and the absence well-formedness
    mode. The presence of [DOCTYPE] is perfectly compatible with
    well-formedness mode - only that the declarations are interpreted
    in a different way.

    If it is tried to parse a document in validating mode, but the
    [DOCTYPE] is missing, this parser will fail when the root element
    is parsed, because its declaration is missing. This conforms to the
    XML standard, and also follows the logic that the program calling
    the parser is written in the expectation that the parsed file is
    validated. If this validation is missing, the program can run into
    failed assertions (or worse).
 *)

val parse_document_entity : 
  ?transform_dtd:(dtd -> dtd) ->
  ?id_index:('ext index) ->
  config -> source -> 'ext spec -> 'ext document
  (** Parse a closed document,
   * and validate the contents of the document against the DTD contained
   * and/or referenced in the document.
   *
   * If the optional argument [transform_dtd] is passed, the following 
   * modification applies: After the DTD (both the internal and external
   * subsets) has been read, the function [transform_dtd] is called,
   * and the resulting DTD is actually used to validate the document.
   * This makes it possible
   * - to check which DTD is used (e.g. by comparing {!Pxp_dtd.dtd.id}
   *   with a list of allowed ID's)
   * - to apply modifications to the DTD before content parsing is started
   * - to even switch to a built-in DTD, and to drop all user-defined
   *   declarations.
   *
   * If the optional argument [transform_dtd] is missing, the parser
   * behaves in the same way as if the identity were passed as [transform_dtd],
   * i.e. the DTD is left unmodified.
   *
   * If the optional argument [id_index] is present, the parser adds
   * any ID attribute to the passed index. An index is required to detect
   * violations of the uniqueness of IDs.
   *)

val parse_wfdocument_entity : 
  ?transform_dtd:(dtd -> dtd) ->
  config -> source -> 'ext spec -> 'ext document
  (** Parse a closed document, but do not
   * validate it. Only checks on well-formedness are performed.
   *
   * The option [transform_dtd] works as for [parse_document_entity],
   * but the resulting DTD is not used for validation. It is just
   * included into the returned document (e.g. useful to get entity 
   * declarations).
   *)

val parse_content_entity  : 
  ?id_index:('ext index) ->
  config -> source -> dtd -> 'ext spec -> 'ext node
  (** Parse a file representing a well-formed fragment of a document. The
   * fragment must be a single element (i.e. something like [<a>...</a>];
   * not a sequence like [<a>...</a><b>...</b>]). The element is validated
   * against the passed DTD, but it is not checked whether the element is
   * the root element specified in the DTD. {b This function is almost
   * always the wrong one to call. Rather consider {!parse_document_entity}.}
   *
   * Despite its name, this function {b cannot} parse the [content]
   * production defined in the XML specification! This is a misnomer
   * I'm sorry about. The [content] production would allow to parse
   * a list of elements and other node kinds.
   *
   * If the optional argument [id_index] is present, the parser adds
   * any ID attribute to the passed index. An index is required to detect
   * violations of the uniqueness of IDs.
   *)

val parse_wfcontent_entity : 
  config -> source -> 'ext spec -> 'ext node
  (** Parse a file representing a well-formed fragment of a document.
   * The fragment is not validated, only checked for well-formedness.
   * See also the notes for {!Pxp_tree_parser.parse_content_entity}.
   *)


(** {1 Helpers} *)

val default_extension : ('a node extension) as 'a
  (** A "null" extension; an extension that does not extend the functionality *)

val default_spec : ('a node extension as 'a) spec
  (** Specifies that you do not want to use extensions. *)

val default_namespace_spec : ('a node extension as 'a) spec
  (** Specifies that you want to use namespace, but not extensions *)


