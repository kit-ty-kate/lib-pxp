(* $Id: pxp_document.mli,v 1.15 2001/05/17 21:40:55 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(**********************************************************************)
(*                                                                    *)
(* Pxp_document:                                                      *)
(*     Object model of the document/element instances                 *)
(*                                                                    *)
(**********************************************************************)


(* ======================================================================
 * OVERVIEW
 *
 * class type node ............. The common class type of the nodes of
 *                               the element tree. Nodes are either
 *                               elements (inner nodes) or data nodes
 *                               (leaves)
 * class type extension ........ The minimal properties of the so-called
 *                               extensions of the nodes: Nodes can be
 *                               customized by applying a class parameter
 *                               that adds methods/values to nodes.
 * class data_impl : node ...... Implements data nodes.
 * class element_impl : node ... Implements element nodes
 * class document .............. A document is an element with some additional
 *                               properties
 *
 * ======================================================================
 *
 * THE STRUCTURE OF NODE TREES:
 *
 * Every node except the root node has a parent node. The parent node is
 * always an element, because data nodes never contain other nodes.
 * In the other direction, element nodes may have children; both elements
 * and data nodes are possible as children.
 * Every node knows its parent (if any) and all its children (if any);
 * the linkage is maintained in both directions. A node without a parent
 * is called a root.
 * It is not possible that a node is the child of two nodes (two different nodes
 * or a multiple child of the same node).
 * You can break the connection between a node and its parent; the method
 * "delete" performs this operations and deletes the node from the parent's
 * list of children. The node is now a root, for itself and for all
 * subordinate nodes. In this context, the node is also called an orphan,
 * because it has lost its parent (this is a bit misleading because the
 * parent is not always the creator of a node).
 * In order to simplify complex operations, you can also set the list of
 * children of an element. Nodes that have been children before are unchanged;
 * new nodes are added (and the linkage is set up), nodes no more occurring
 * in the list are handled if they have been deleted.
 * If you try to add a node that is not a root (either by an "add" or by a
 * "set" operation) the operation fails.
 *
 * CREATION OF NODES
 *
 * The class interface supports creation of nodes by cloning a so-called
 * exemplar. The idea is that it is sometimes useful to implement different
 * element types by different classes, and to implement this by looking up
 * exemplars.
 * Imagine you have three element types A, B, and C, and three classes
 * a, b, and c implementing the node interface (for example, by providing
 * different extensions, see below). The XML parser can be configured to
 * have a lookup table
 *   { A --> a0,  B --> b0, C --> c0 }
 * where a0, b0, c0 are exemplars of the classes a, b, and c, i.e. empty
 * objects belonging to these classes. If the parser finds an instance of
 * A, it looks up the exemplar a0 of A and clones it (actually, the method
 * "create_element" performs this for elements, and "create_data" for data
 * nodes). Clones belong to the same class as the original nodes, so the
 * instances of the elements have the same classes as the configured
 * exemplars.
 * Note: This technique assumes that the interface of all exemplars is the
 * same!
 *
 * THE EXTENSION
 *
 * The class type node and all its implementations have a class parameter
 * 'ext which must at least fulfil the properties of the class type "extension".
 * The idea is that you can add properties, for example:
 *
 * class my_extension =
 *   object
 *     (* minimal properties required by class type "extension": *)
 *     method clone = ...
 *     method node = ...
 *     method set_node n = ...
 *     (* here my own methods: *)
 *     method do_this_and_that ...
 *   end
 *
 * class my_element_impl = [ my_extension ] element_impl
 * class my_data_impl    = [ my_extension ] data_impl
 *
 * The whole XML parser is parameterized with 'ext, so your extension is
 * visible everywhere (this is the reason why extensibility is solved by
 * parametric polymorphism and not by inclusive polymorphism (subtyping)).
 *
 *
 * SOME COMPLICATED TYPE EXPRESSIONS
 *
 * Sometimes the following type expressions turn out to be necessary:
 *
 * 'a node extension as 'a
 *      This is the type of an extension that belongs to a node that
 *      has an extension that is the same as we started with.
 *
 * 'a extension node as 'a
 *      This is the type of a node that has an extension that belongs to a
 *      node of the type we started with.
 *
 *
 * DOCUMENTS
 * ...
 *
 * ======================================================================
 *
 * SIMPLE USAGE: ...
 *)


open Pxp_dtd


type node_type =
  (* The basic and most important node types:
   * - T_element element_type   is the type of element nodes
   * - T_data                   is the type of text data nodes
   * By design of the parser, neither CDATA sections nor entity references
   * are represented in the node tree; so there are no types for them.
   *)
    T_element of string
  | T_data

  (* The following types are extensions to my original design. They have mainly
   * been added to simplify the implementation of standards (such as
   * XPath) that require that nodes of these types are included into the
   * main document tree.
   * There are options (see Pxp_yacc) forcing the parser to insert such
   * nodes; in this case, the nodes are actually element nodes serving
   * as wrappers for the additional data structures. The options are:
   * enable_super_root_node, enable_pinstr_nodes, enable_comment_nodes.
   * By default, such nodes are not created.
   *)
  | T_super_root                        (* XPath calls them simply root nodes *)
  | T_pinstr of string                  (* The string is the target of the PI *)
  | T_comment

  (* The following types are fully virtual. This means that it is impossible
   * to make the parser insert such nodes into the regular tree. They are
   * normally created by special methods to allow additional views on the
   * document tree.
   *)
  | T_none
  | T_attribute of string          (* The string is the name of the attribute *)
  | T_namespace of string            (* The string is the namespace srcprefix *)
;;


(* Very experimental namespace support: *)

class namespace_manager :
  object
    method add : string -> string -> string
      (* let normprefix = mng # add prefix uri
       * Normalizes prefix and adds the pair (normprefix, uri) to the
       * managed set of namespaces.
       * "Normalizing" means that the prefix is changed such that it becomes
       * unique.
       *)
    method get_uri : string -> string
      (* Return the URI for a normprefix, or raises Not_found.
       * get_uri "" raises always Not_found.
       *)
    method get_normprefix : string -> string
      (* Return the normprefix for a URI, or raises Not_found *)

    (* Encodings: prefixes and URIs are always encoded in the default
     * encoding of the document
     *)
  end
;;


(* Regular definition: *)


class type [ 'node ] extension =
  object ('self)
    method clone : 'self
      (* "clone" should return an exact deep copy of the object. *)
    method node : 'node
      (* "node" returns the corresponding node of this extension. This method
       * intended to return exactly what previously has been set by "set_node".
       *)
    method set_node : 'node -> unit
      (* "set_node" is invoked once the extension is associated to a new
       * node object.
       *)
  end
;;


class type [ 'ext ] node =
  object ('self)
    constraint 'ext = 'ext node #extension

    method extension : 'ext
      (* Return the extension of this node: *)

    method delete : unit
      (* Delete this node from the parent's list of sub nodes. This node gets
       * orphaned.
       * 'delete' does nothing if this node does not have a parent.
       *)

    method parent : 'ext node
      (* Get the parent, or raise Not_found if this node is an orphan. *)

    method root : 'ext node
      (* Get the direct or indirect parent that does not have a parent itself,
       * i.e. the root of the tree.
       *)

    method orphaned_clone : 'self
      (* return an exact clone of this element and all sub nodes (deep copy)
       * except string values which are shared by this node and the clone.
       * The other exception is that the clone has no parent (i.e. it is now
       * a root).
       *)

    method orphaned_flat_clone : 'self
      (* return a clone of this element where all subnodes are omitted.
       * The type of the node, and the attributes are the same as in the
       * original node.
       * The clone has no parent.
       *)

    method add_node : ?force:bool -> 'ext node -> unit
      (* Append new sub nodes -- mainly used by the parser itself, but
       * of course open for everybody. If an element is added, it must be
       * an orphan (i.e. does not have a parent node); and after addition
       * *this* node is the new parent.
       * The method performs some basic validation checks if the current node
       * has a regular expression as content model, or is EMPTY. You can
       * turn these checks off by passing ~force:true to the method.
       *)

    method add_pinstr : proc_instruction -> unit
      (* Add a processing instruction to the set of processing instructions of
       * this node. Usually only elements contain processing instructions.
       *)

    method pinstr : string -> proc_instruction list
      (* Get all processing instructions with the passed name *)

    method pinstr_names : string list
      (* Get a list of all names of processing instructions *)

    method node_position : int
      (* Returns the position of this node among all children of the parent
       * node. Positions are counted from 0.
       * Raises Not_found if the node is the root node.
       *)

    method node_path : int list
      (* Returns the list of node positions of the ancestors of this node,
       * including this node. The first list element is the node position
       * of this child of the root, and the last list element is the
       * node position of this node.
       * Returns [] if the node is the root node.
       *
       * Attribute and namespace nodes: By definition of the document order
       * these nodes occur after their parent and before the regular children
       * (elements). Because of this, attribute nodes at position p have the 
       * node path [ -1; p ] relative to their parent, and namespace nodes
       * at position p have the node path [ -2; p ] relative to their
       * parent.
       *)

    method sub_nodes : 'ext node list
      (* Get the list of sub nodes *)

    method iter_nodes : ('ext node -> unit) -> unit
      (* iterate over the sub nodes *)

    method iter_nodes_sibl :
      ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
      (* Here every iteration step can also access to the previous and to the
       * following node if present.
       *)

    method nth_node : int -> 'ext node
      (* Returns the n-th sub node of this node, n >= 0. Raises Not_found
       * if the index is out of the valid range.
       * Note that the first invocation of this method requires additional
       * overhead.
       *)

    method previous_node : 'ext node
    method next_node : 'ext node
      (* Return the previous and next nodes, respectively. These methods are
       * equivalent to
       * - parent # nth_node (self # node_position - 1) and
       * - parent # nth_node (self # node_position + 1), respectively.
       *)

    method set_nodes : 'ext node list -> unit
      (* Set the list of sub nodes. Elements that are no longer sub nodes gets
       * orphaned, and all new elements that previously were not sub nodes
       * must have been orphaned.
       *)

    method data : string
      (* Get the data string of this node. (This method conforms to the
       * string-value method of XPath.)
       *
       * Data nodes: returns the character data
       * Element nodes, Super root node: returns the concatenation of all
       *   subordinate data nodes (ignores attributes, comments, PIs, and
       *   namespaces)
       * Attribute nodes: Returns the attribute value as string
       *   (raises Not_found for implied values)
       * Comment nodes: Returns the comment string (inside <-- and -->)
       *   (raises Not_found if there is no comment string)
       * PI nodes: Returns the data part of the processing instruction
       *   (returns "" if the data part is missing)
       * Namespace nodes: Returns the namespace URI
       *)

    method node_type : node_type
      (* Get the name of the element type. *)

    method position : (string * int * int)
      (* Return the name of the entity, the line number, and the column
       * position (byte offset) of the beginning of the element.
       * Only available if the element has been created with position
       * information.
       * Returns "?",0,0 if not available. (Note: Line number 0 is not
       * possible otherwise.)
       *)

    method attribute : string -> Pxp_types.att_value
    method attribute_names : string list
    method attribute_type : string -> Pxp_types.att_type
    method attributes : (string * Pxp_types.att_value) list
      (* Get a specific attribute; get the names of all attributes; get the
       * type of a specific attribute; get names and values of all attributes.
       * Only elements have attributes.
       * Note: If the DTD allows arbitrary for this element, "attribute_type"
       * raises Undeclared.
       *)

    method required_string_attribute : string -> string
    method required_list_attribute : string -> string list
      (* Return the attribute or fail if the attribute is not present:
       * The first version passes the value always as string back;
       * the second version always as list.
       *)

    method optional_string_attribute : string -> string option
    method optional_list_attribute : string -> string list
      (* Return some attribute value or return None if the attribute is not
       *  present:
       * The first version passes the value always as string back;
       * the second version always as list.
       *)

    method id_attribute_name : string
    method id_attribute_value : string
      (* Return the name and value of the ID attribute. The methods may
       * raise Not_found if there is no ID attribute in the DTD, or no
       * ID attribute in the element, respectively.
       *)

    method idref_attribute_names : string list
      (* Returns the list of attribute names of IDREF or IDREFS type. *)

    method quick_set_attributes : (string * Pxp_types.att_value) list -> unit
      (* Sets the attributes but does not check whether they match the DTD.
       *)

    method attributes_as_nodes : 'ext node list
      (* Experimental feature: Return the attributes as node list. Every node
       * has type T_attribute n, and contains only the single attribute n.
       * This node list is computed on demand, so the first invocation of this
       * method will create the list, and following invocations will only
       * return the existing list.
       *)

    method set_comment : string option -> unit
      (* Sets the comment string; only applicable for T_comment nodes *)

    method comment : string option
      (* Get the comment string.
       * Returns always None for nodes with a type other than T_comment.
       *
       * Note: The 'data' method also returns the comment string, and ""
       * if the string is not available.
       *)

    method normprefix : string
      (* For namespace-aware implementations of the node class, this method
       * returns the normalized prefix of the element or attribute.
       * If the object does not have a prefix, "" will be passed back.
       *
       * This method is only supported by the implementations
       * namespace_element_impl, namespace_attribute_impl.
       * When invoked for other classes, it will fail.
       *)

    method localname : string
      (* For namespace-aware implementations of the node class, this method
       * returns the local part of the name of the element or attribute.
         *
       * This method is only supported by the implementations
       * namespace_element_impl, namespace_attribute_impl.
       * When invoked for other classes, it will fail.
       *)

    method namespace_uri : string
      (* For namespace-aware implementations of the node class, this method
       * returns the namespace URI of the element, attribute or namespace.
       * It is required that a namespace manager is available.
       *
       * If the object does not have a namespace prefix, and there is no
       * default namespace, this method returns "".
       *
       * This method is only supported by the implementations
       * namespace_element_impl, namespace_attribute_impl, namespace_impl.
       * When invoked for other classes, it will fail.
       *)

    method namespace_manager : namespace_manager
      (* For namespace-aware implementations of the node class, this method
       * returns the namespace manager. If the namespace manager has not been
       * set, the exception Not_found is raised.
       *
       * This method is only supported by the implementations
       * namespace_element_impl, namespace_attribute_impl, namespace_impl.
       * When invoked for other classes, it will fail.
       *)

    method set_namespace_manager : namespace_manager -> unit
      (* Sets the namespace manager as returned by namespace_manager.
       *
       * This method is only supported by the implementations
       * namespace_element_impl, namespace_attribute_impl, namespace_impl.
       * When invoked for other classes, it will fail.
       *)

    method namespace_info : 'ext namespace_info
      (* Returns additional information about the namespace prefixes
       * in the parsed XML source. This method has been added for
       * better XPath conformance.
       *
       * This record is only available if the parser has been configured
       * to support namespaces, and if the parser has been configured
       * to set this record (requires a lot of memory). Furthermore, only
       * the implementation namespace_element_impl supports this method.
       *
       * This method raises Not_found if the namespace_info field has not
       * been set.
       *
       * This method fails if the class does not support it.
       *)

    method set_namespace_info : 'ext namespace_info option -> unit
      (* Sets the namespace_info field.
       * Only the implementation namespace_element_impl supports this
       * method.
       *)

    method dtd : dtd
      (* Get the DTD. Fails if no DTD is specified (which is impossible if
       * 'create_element' or 'create_data' have been used to create this
       * object)
       *)

    method encoding : Pxp_types.rep_encoding
      (* Get the encoding which is always the same as the encoding of the
       * DTD. See also method 'dtd' (Note: This method fails, too, if
       * no DTD is present.)
       *)

    method create_element :
             ?name_pool_for_attribute_values:Pxp_types.pool ->
             ?position:(string * int * int) ->
             dtd -> node_type -> (string * string) list -> 'ext node
      (* create an "empty copy" of this element:
       * - new DTD
       * - new node type (which must not be T_data)
       * - new attribute list
       * - empty list of nodes
       *)

    method create_data : dtd -> string -> 'ext node
      (* create an "empty copy" of this data node: *)

    method local_validate :
             ?use_dfa:bool ->
             unit -> unit
      (* Check that this element conforms to the DTD.
       * Option ~use_dfa: If true, the deterministic finite automaton of
       *   regexp content models is used for validation, if available.
       *   Defaults to false.
       *)

    method keep_always_whitespace_mode : unit
      (* Normally, add_node does not accept data nodes when the DTD does not
       * allow data nodes or only whitespace ("ignorable whitespace").
       * Once you have invoked this method, ignorable whitespace is forced
       * to be included into the document.
       *)

    method write : 
             ?prefixes:string list ->
             Pxp_types.output_stream -> Pxp_types.encoding -> unit
      (* Write the contents of this node and the subtrees to the passed
       * output stream; the passed encoding is used. The format
       * is compact (the opposite of "pretty printing").
       *
       * Option ~prefixes: The class namespace_element_impl interprets this
       *   option and passes it recursively to subordinate invocations of
       *   'write'. The meaning is that the normprefixes enumerated by this list
       *   have already been declared by surrounding elements. The option
       *   defaults to [] forcing the method to output all necessary prefix
       *   declarations.
       *)

    method write_compact_as_latin1 : Pxp_types.output_stream -> unit
      (* DEPRECATED METHOD; included only to keep compatibility with
       * older versions of the parser
       *)


    (* ---------------------------------------- *)
    (* The methods 'find' and 'reset_finder' are no longer supported.
     * The functionality is provided by the configurable index object
     * (see Pxp_yacc).
     *)


    (* ---------------------------------------- *)
    (* internal methods: *)
    method internal_adopt : 'ext node option -> int -> unit
    method internal_set_pos : int -> unit
    method internal_delete : 'ext node -> unit
    method internal_init : (string * int * int) ->
                           Pxp_types.pool option ->
                           dtd -> string -> (string * string) list -> unit
    method internal_init_other : (string * int * int) ->
                                 dtd -> node_type -> unit
  end

and ['ext] namespace_info =
  object
    method srcprefix : string
      (* Returns the prefix before it is normalized *)

    method declaration : 'ext node list
      (* Returns the currently active namespace declaration. The list
       * enumerates all namespace objects with
       *   namespace # node_type = T_namespace "srcprefix"
       * meaning that the srcprefix is declared to correspond to the
       * namespace URI
       *   namespace # namespace_uri.
       * This list always declares the prefix "xml". If there is a default
       * namespace, it is declared for the prefix "".
       *)
  end
;;


class [ 'ext ] data_impl : 'ext -> [ 'ext ] node
    (* Creation:
     *   new data_impl an_extension
     * creates a new data node with the given extension and the empty string
     * as content.
     *)
;;


class [ 'ext ] element_impl : 'ext -> [ 'ext ] node
    (* Creation:
     *   new element_impl an_extension
     * creates a new empty element node with the given extension.
     *)
;;


class [ 'ext ] comment_impl : 'ext -> [ 'ext ] node ;;

class [ 'ext ] super_root_impl : 'ext -> [ 'ext ] node ;;

class [ 'ext ] pinstr_impl : 'ext -> [ 'ext ] node ;;
    (* These classes work like element_impl, but create new empty nodes
     * for comments, super roots, and processing instructions, resp.
     *)


(* Attribute nodes are experimental: *)

class [ 'ext ] attribute_impl :
  element:string -> name:string -> Pxp_types.att_value -> dtd -> [ 'ext ] node
;;
    (* Creation:
     *   new attribute_impl element_name attribute_name attribute_value dtd
     * Note that attribute nodes do intentionally not have extensions.
     *
     * Attribute nodes are created on demand by the first invocation of
     * attributes_as_nodes of the element node. Attribute nodes are
     * created directly and not by copying exemplar nodes, so you never
     * need to create them yourself.
     *
     * Attribute nodes have the following properties:
     * - The node type is T_attribute name.
     * - The parent node is the element node.
     * - The method "attributes" returns [ name, value ], i.e. such nodes
     *   have a single attribute "name". To get the value, call
     *   n # attribute name.
     * - The method "data" returns the string representation of the 
     *   attribute value.
     * - Attribute nodes are leaves of the tree.
     *
     * Attribute nodes are designed to be members of XPath node sets, and
     * are only useful if you need such sets.
     *)

(* Very experimental namespace support: *)

class [ 'ext ] namespace_element_impl :
  'ext -> [ 'ext ] node
;;

  (* namespace_element_impl: the namespace-aware implementation of element
   * nodes. 
   *
   * This class has an extended definition of the create_element method.
   * It accepts element names of the form "normprefix:localname" where
   * normprefix must be a prefix managed by the namespace_manager. Note
   * that create_element does not itself normalize prefixes; it is expected
   * that the prefixes are already normalized.
   *
   * Such nodes have a node type T_element "normprefix:localname".
   *
   * Furthermore, this class implements the methods:
   * - normprefix
   * - localname
   * - namespace_uri
   * - namespace_info
   * - set_namespace_info
   * - namespace_manager
   * - set_namespace_manager
   *)


class [ 'ext ] namespace_attribute_impl :
  element:string -> name:string -> Pxp_types.att_value -> dtd -> [ 'ext ] node
;;

  (* namespace_attribute_impl: the namespace-aware implementation of
   * attribute nodes.
   *)


class [ 'ext ] namespace_impl :
  (* srcprefix: *) string -> (* normprefix: *) string -> dtd -> [ 'ext ] node
;;
  (* Namespace objects are only used to represent the namespace declarations
   * occurring in the attribute lists of elements.
   * They are stored in the namespace_info object if that is requested.
   *)


class [ 'ext ] namespace_info_impl :
  (* srcprefix: *) string -> 
  (* element: *)   'ext node -> 
  (* src_norm_mapping: *) (string * string) list ->
     [ 'ext ] namespace_info
;;


(********************************** spec *********************************)

type 'ext spec
constraint 'ext = 'ext node #extension
    (* Contains the exemplars used for the creation of new nodes
     *)


val make_spec_from_mapping :
      ?super_root_exemplar : 'ext node ->
      ?comment_exemplar : 'ext node ->
      ?default_pinstr_exemplar : 'ext node ->
      ?pinstr_mapping : (string, 'ext node) Hashtbl.t ->
      data_exemplar: 'ext node ->
      default_element_exemplar: 'ext node ->
      element_mapping: (string, 'ext node) Hashtbl.t ->
      unit ->
        'ext spec
    (* Specifies:
     * - For new data nodes, the ~data_exemplar must be used
     * - For new element nodes: If the element type is mentioned in the
     *   ~element_mapping hash table, the exemplar found in this table is
     *   used. Otherwise, the ~default_element_exemplar is used.
     * Optionally:
     * - You may also specify exemplars for super root nodes, for comments
     *   and for processing instructions
     *)

val make_spec_from_alist :
      ?super_root_exemplar : 'ext node ->
      ?comment_exemplar : 'ext node ->
      ?default_pinstr_exemplar : 'ext node ->
      ?pinstr_alist : (string * 'ext node) list ->
      data_exemplar: 'ext node ->
      default_element_exemplar: 'ext node ->
      element_alist: (string * 'ext node) list ->
      unit ->
        'ext spec
    (* This is a convenience function: You can pass the mappings from
     * elements and PIs to exemplar by associative lists.
     *)

val create_data_node :
      'ext spec -> dtd -> string -> 'ext node
val create_element_node :
      ?name_pool_for_attribute_values:Pxp_types.pool ->
      ?position:(string * int * int) ->
      'ext spec -> dtd -> string -> (string * string) list -> 'ext node
val create_super_root_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> 'ext node
val create_comment_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> string -> 'ext node
val create_pinstr_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> proc_instruction -> 'ext node
  (* These functions use the exemplars contained in a spec and create fresh
   * node objects from them.
   *)

val create_no_node :
       ?position:(string * int * int) -> 'ext spec -> dtd -> 'ext node
  (* Creates a T_none node with limited functionality 
   * NOTE: This function is conceptually broken and may be dropped in the
   * future.
   *)

val get_data_exemplar :
      'ext spec -> 'ext node
val get_element_exemplar :
      'ext spec -> string -> (string * string) list -> 'ext node
val get_super_root_exemplar :
      'ext spec -> 'ext node
val get_comment_exemplar :
      'ext spec -> 'ext node
val get_pinstr_exemplar :
      'ext spec -> proc_instruction -> 'ext node
  (* These functions just return the exemplars (or raise Not_found).
   * Notes:
   * (1) In future versions, it may be possible that the element exemplar
   *     depends on attributes, too, so the attlist must be passed
   *     to get_element_exemplar
   * (2) In future versions, it may be possible that the pinstr exemplar
   *     depends on the full value of the processing instruction and
   *     not only on the target, so the full proc_instruction must be
   *     passed to get_pinstr_exemplar.
   *)


(*********************** Ordering of nodes ******************************)

(* The functions compare and ord_compare implement the so-called
 * "document order". The basic principle is that the nodes are linearly
 * ordered by their occurence in the textual XML representation of the
 * tree. While this is clear for element nodes, data nodes, comments, and
 * processing instructions, a more detailed definition is necessary for the
 * other node types. In particular, attribute nodes of an element node
 * occur before any regular subnode of the element, and namespace nodes
 * of that element occur even before the attribute nodes. So the order
 * of nodes of
 *   <sample a1="5" a2="6"><subnode/></sample> 
 * is
 *   1. element "sample"
 *   2. attribute "a1"
 *   3. attribute "a2"
 *   4. element "subnode"
 * Note that the order of the attributes of the same element is unspecified,
 * so "a2" may alternatively be ordered before "a1". If there were namespace
 * nodes, they would occur between 1 and 2.
 *   If there is a super root node, it will be handled as the very first
 * node.
 *)

val compare : 'ext node -> 'ext node -> int
  (* Returns -1 if the first node is before the second node, or +1 if the
   * first node is after the second node, or 0 if both nodes are identical.
   * If the nodes are unrelated (do not have a common ancestor), the result
   * is undefined.
   * This test is rather slow, but it works even if the XML tree changes
   * dynamically (in contrast to ord_compare below).
   *)

type 'ext ord_index
constraint 'ext = 'ext node #extension
  (* The type of ordinal indexes *)

val create_ord_index : 'ext node -> 'ext ord_index
  (* Creates an ordinal index for the subtree starting at the passed node.
   * This index assigns to every node an ordinal number (beginning with 0) such
   * that nodes are numbered upon the order of the first character in the XML
   * representation (document order).
   * Note that the index is not automatically updated when the tree is
   * modified.
   *)

val ord_number : 'ext ord_index -> 'ext node -> int
  (* Returns the ordinal number of the node, or raises Not_found.
   * Note that attribute nodes and namespace nodes are treated specially:
   * All attribute nodes for a certain element node have the _same_
   * ordinal index. All namespace nodes for a certain element node
   * have the _same_ ordinal index.
   * (So ord_number x = ord_number y does not imply x == y for these
   * nodes. However, this is true for the other node types.)
   * It is not recommended to work with the ordinal number directly but
   * to call ord_compare which already handles the special cases.
   *)

val ord_compare : 'ext ord_index -> 'ext node -> 'ext node -> int
  (* Compares two nodes like 'compare':
   * Returns -1 if the first node is before the second node, or +1 if the
   * first node is after the second node, or 0 if both nodes are identical.
   * If one of the nodes does not occur in the ordinal index, Not_found
   * is raised. (Note that this is a different behaviour than what 'compare'
   * would do.)
   * This test is much faster than 'compare'.
   *)


(***************************** Iterators ********************************)

(* General note: The iterators ignore attribute and namespace nodes *)

val find : ?deeply:bool ->
           f:('ext node -> bool) -> 'ext node -> 'ext node
  (* Searches the first node for which the predicate f is true, and returns
   * it. Raises Not_found if there is no such node.
   * By default, ~deeply=false. In this case, only the children of the
   * passed node are searched.
   * If passing ~deeply=true, the children are searched recursively
   * (depth-first search).
   *)

val find_all : ?deeply:bool ->
               f:('ext node -> bool) -> 'ext node -> 'ext node list
  (* Searches all nodes for which the predicate f is true, and returns them.
   * By default, ~deeply=false. In this case, only the children of the
   * passed node are searched.
   * If passing ~deeply=true, the children are searched recursively
   * (depth-first search).
   *)

val find_element : ?deeply:bool ->
                   string -> 'ext node -> 'ext node
  (* Searches the first element with the passed element type.
   * By default, ~deeply=false. In this case, only the children of the
   * passed node are searched.
   * If passing ~deeply=true, the children are searched recursively
   * (depth-first search).
   *)

val find_all_elements : ?deeply:bool ->
                        string -> 'ext node -> 'ext node list
  (* Searches all elements with the passed element type.
   * By default, ~deeply=false. In this case, only the children of the
   * passed node are searched.
   * If passing ~deeply=true, the children are searched recursively
   * (depth-first search).
   *)

exception Skip
val map_tree :  pre:('exta node -> 'extb node) ->
               ?post:('extb node -> 'extb node) ->
               'exta node ->
                   'extb node
  (* Traverses the passed node and all children recursively. After entering
   * a node, the function ~pre is called. The result of this function must
   * be a new node; it must not have children nor a parent (you can simply
   * pass (fun n -> n # orphaned_flat_clone) as ~pre).
   * After that, the children are processed in the same way (from left to
   * right); the results of the transformation will be added to the
   * new node as new children.
   * Now, the ~post function is invoked with this node as argument, and
   * the result is the result of the function (~post should return a root
   * node, too; if not specified, the identity is the ~post function).
   * Both ~pre and ~post may raise Skip, which causes that the node is
   * left out. If the top node is skipped, the exception Not_found is
   * raised.
   *)

val map_tree_sibl :
        pre: ('exta node option -> 'exta node -> 'exta node option ->
                  'extb node) ->
       ?post:('extb node option -> 'extb node -> 'extb node option ->
                  'extb node) ->
       'exta node ->
           'extb node
   (* Works like map_tree, but the function ~pre and ~post have additional
    * arguments:
    * - ~pre l n r: The node n is the node to map, and l is the previous
    *   node, and r is the next node (both None if not present). l and r
    *   are both nodes before the transformation.
    * - ~post l n r: The node n is the node which is the result of ~pre
    *   plus adding children. l and r are again the previous and the next
    *   node, respectively, but after being transformed.
    *)

val iter_tree : ?pre:('ext node -> unit) ->
                ?post:('ext node -> unit) ->
                'ext node ->
                    unit
   (* Iterates only instead of mapping the nodes. *)

val iter_tree_sibl :
       ?pre: ('ext node option -> 'ext node -> 'ext node option -> unit) ->
       ?post:('ext node option -> 'ext node -> 'ext node option -> unit) ->
       'ext node ->
           unit
   (* Iterates only instead of mapping the nodes. *)


(******************************* document ********************************)


class [ 'ext ] document :
  Pxp_types.collect_warnings ->
  object
    (* Documents: These are containers for root elements and for DTDs.
     *
     * Important invariant: A document is either empty (no root element,
     * no DTD), or it has both a root element and a DTD.
     *
     * A fresh document created by 'new' is empty.
     *)

    method init_xml_version : string -> unit
	(* Set the XML version string of the XML declaration. *)

    method init_root : 'ext node -> unit
	(* Set the root element. It is expected that the root element has
	 * a DTD.
	 * Note that 'init_root' checks whether the passed root element
	 * has the type expected by the DTD. The check takes into account
	 * that the root element might be a virtual root node.
	 *)

    method xml_version : string
      (* Returns the XML version from the XML declaration. Returns "1.0"
       * if the declaration is missing.
       *)

    method xml_standalone : bool
      (* Returns whether this document is declared as being standalone.
       * This method returns the same value as 'standalone_declaration'
       * of the DTD (if there is a DTD).
       * Returns 'false' if there is no DTD.
       *)

    method dtd : dtd
      (* Returns the DTD of the root element.
       * Fails if there is no root element.
       *)

    method encoding : Pxp_types.rep_encoding
      (* Returns the string encoding of the document = the encoding of
       * the root element = the encoding of the element tree = the
       * encoding of the DTD.
       * Fails if there is no root element.
       *)

    method root : 'ext node
      (* Returns the root element, or fails if there is not any. *)

    method add_pinstr : proc_instruction -> unit
      (* Adds a processing instruction to the document container.
       * The parser does this for PIs occurring outside the DTD and outside
       * the root element.
       *)

    method pinstr : string -> proc_instruction list
      (* Return all PIs for a passed target string. *)

    method pinstr_names : string list
      (* Return all target strings of all PIs. *)

    method write : Pxp_types.output_stream -> Pxp_types.encoding -> unit
      (* Write the document to the passed
       * output stream; the passed encoding used. The format
       * is compact (the opposite of "pretty printing").
       * If a DTD is present, the DTD is included into the internal subset.
       *)

    method write_compact_as_latin1 : Pxp_types.output_stream -> unit
      (* DEPRECATED METHOD; included only to keep compatibility with
       * older versions of the parser
       *)

  end
;;


(* ======================================================================
 * History:
 *
 * $Log: pxp_document.mli,v $
 * Revision 1.15  2001/05/17 21:40:55  gerd
 * 	Changed comments.
 *
 * Revision 1.14  2001/05/17 21:38:12  gerd
 * 	Updated signatures for namespace functionality:
 * 	- methods namespace_manager, set_namespace_manager
 * 	- classes namespace_element_impl, namespace_impl, namespace_info_impl
 *
 * 	Added comments for attribute_impl, document order
 *
 * Revision 1.13  2001/04/26 23:59:36  gerd
 * 	Experimental support for namespaces: classes namespace_impl,
 * namespace_element_impl, namespace_attribute_impl.
 * 	New classes comment_impl, pinstr_impl, super_root_impl. These
 * classes have been added for stricter (runtime) type checking.
 *
 * Revision 1.12  2000/09/21 21:29:41  gerd
 * 	New functions get_*_exemplar.
 *
 * Revision 1.11  2000/09/09 16:41:03  gerd
 * 	Effort to reduce the amount of allocated memory: The number of
 * instance variables in document nodes has been miminized; the class
 * default_ext no longer stores anything; string pools have been implemented.
 *
 * Revision 1.10  2000/08/30 15:47:37  gerd
 * 	New method node_path.
 * 	New function compare.
 * 	New type ord_index with functions.
 *
 * Revision 1.9  2000/08/26 23:27:53  gerd
 * 	New function: make_spec_from_alist.
 * 	New iterators: find, find_all, find_element, find_all_elements,
 * map_tree, map_tree_sibl, iter_tree, iter_tree_sibl.
 * 	New node methods: node_position, nth_node, previous_node,
 * next_node.
 * 	Attribute and namespace types have now a string argument:
 * the name/prefix. I hope this simplifies the handling of view nodes.
 * 	First implementation of view nodes: attribute_impl. The
 * method attributes_as_nodes returns the attributes wrapped into
 * T_attribute nodes which reside outside the document tree.
 *
 * Revision 1.8  2000/08/18 20:14:00  gerd
 * 	New node_types: T_super_root, T_pinstr, T_comment, (T_attribute),
 * (T_none), (T_namespace).
 *
 * Revision 1.7  2000/07/23 02:16:34  gerd
 * 	Support for DFAs.
 *
 * Revision 1.6  2000/07/16 16:34:41  gerd
 * 	New method 'write', the successor of 'write_compact_as_latin1'.
 *
 * Revision 1.5  2000/07/14 13:56:11  gerd
 * 	Added methods id_attribute_name, id_attribute_value,
 * idref_attribute_names.
 *
 * Revision 1.4  2000/07/09 17:51:14  gerd
 * 	Element nodes can store positions.
 *
 * Revision 1.3  2000/07/04 22:05:10  gerd
 * 	New functions make_spec_from_mapping, create_data_node,
 * create_element_node.
 *
 * Revision 1.2  2000/06/14 22:19:06  gerd
 * 	Added checks such that it is impossible to mix encodings.
 *
 * Revision 1.1  2000/05/29 23:48:38  gerd
 * 	Changed module names:
 * 		Markup_aux          into Pxp_aux
 * 		Markup_codewriter   into Pxp_codewriter
 * 		Markup_document     into Pxp_document
 * 		Markup_dtd          into Pxp_dtd
 * 		Markup_entity       into Pxp_entity
 * 		Markup_lexer_types  into Pxp_lexer_types
 * 		Markup_reader       into Pxp_reader
 * 		Markup_types        into Pxp_types
 * 		Markup_yacc         into Pxp_yacc
 * See directory "compatibility" for (almost) compatible wrappers emulating
 * Markup_document, Markup_dtd, Markup_reader, Markup_types, and Markup_yacc.
 *
 * ======================================================================
 * Old logs from markup_document.mli:
 *
 * Revision 1.13  2000/05/27 19:15:08  gerd
 * 	Removed the method init_xml_standalone.
 *
 * Revision 1.12  2000/05/01 20:42:34  gerd
 *         New method write_compact_as_latin1.
 *
 * Revision 1.11  2000/04/30 18:15:57  gerd
 * 	Beautifications.
 * 	New method keep_always_whitespace_mode.
 *
 * Revision 1.10  2000/03/11 22:58:15  gerd
 * 	Updated to support Markup_codewriter.
 *
 * Revision 1.9  2000/01/27 21:51:56  gerd
 * 	Added method 'attributes'.
 *
 * Revision 1.8  2000/01/27 21:19:07  gerd
 * 	Added further methods.
 *
 * Revision 1.7  1999/11/09 22:20:14  gerd
 * 	Removed method init_dtd from class "document". The DTD is
 * implicitly passed to the document by the root element.
 *
 * Revision 1.6  1999/09/01 22:51:40  gerd
 * 	Added methods to store processing instructions.
 *
 * Revision 1.5  1999/09/01 16:19:57  gerd
 * 	The "document" class has now a "warner" as class argument.
 *
 * Revision 1.4  1999/08/19 21:59:13  gerd
 * 	Added method "reset_finder".
 *
 * Revision 1.3  1999/08/19 01:08:29  gerd
 * 	Added method "find".
 *
 * Revision 1.2  1999/08/15 02:19:41  gerd
 * 	Some new explanations: That unknown elements are not rejected
 * if the DTD allows them.
 *
 * Revision 1.1  1999/08/10 00:35:51  gerd
 * 	Initial revision.
 *
 *
 *)
