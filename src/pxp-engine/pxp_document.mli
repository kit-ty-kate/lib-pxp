(* $Id: pxp_document.mli,v 1.1 2000/05/29 23:48:38 gerd Exp $
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
    T_element of string
  | T_data
;;


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

    method add_node : 'ext node -> unit
      (* Append new sub nodes -- mainly used by the parser itself, but
       * of course open for everybody. If an element is added, it must be
       * an orphan (i.e. does not have a parent node); and after addition
       * *this* node is the new parent.
       *)

    method add_pinstr : proc_instruction -> unit
      (* Add a processing instruction to the set of processing instructions of
       * this node. Usually only elements contain processing instructions.
       *)

    method pinstr : string -> proc_instruction list
      (* Get all processing instructions with the passed name *)

    method pinstr_names : string list
      (* Get a list of all names of processing instructions *)

    method sub_nodes : 'ext node list
      (* Get the list of sub nodes *)

    method iter_nodes : ('ext node -> unit) -> unit
      (* iterate over the sub nodes *)

    method iter_nodes_sibl :
      ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
      (* Here every iteration step can also access to the previous and to the
       * following node if present:
       *)

    method find : string -> 'ext node
      (* Get the node that has an ID attribute with this value, or raise
       * Not_found.
       * "find" may also cause a Validation_error if something is wrong
       * with the IDs.
       *)

    method reset_finder : unit
      (* makes that newly added nodes will also be found *)

    method set_nodes : 'ext node list -> unit
      (* Set the list of sub nodes. Elements that are no longer sub nodes gets
       * orphaned, and all new elements that previously were not sub nodes
       * must have been orphaned.
       *)

    method data : string
      (* Get the data string of this node. For data nodes, this string is just
       * the content. For elements, this string is the concatenation of all
       * subordinate data nodes.
       *)

    method node_type : node_type
      (* Get the name of the element type. *)

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

    method quick_set_attributes : (string * Pxp_types.att_value) list -> unit
      (* Sets the attributes but does not check whether they match the DTD.
       *)

     method dtd : dtd
       (* Get the DTD *)

    method create_element : dtd -> node_type -> (string * string) list -> 'ext node
      (* create an "empty copy" of this element:
       * - new DTD
       * - new node type
       * - new attribute list
       * - empty list of nodes
       *)

    method create_data : dtd -> string -> 'ext node
      (* create an "empty copy" of this data node: *)

    method local_validate : unit
      (* Check that this element conforms to the DTD: *)

    method keep_always_whitespace_mode : unit
      (* Normally, add_node does not accept data nodes when the DTD does not
       * allow data nodes or only whitespace ("ignorable whitespace").
       * Once you have invoked this method, ignorable whitespace is forced
       * to be included into the document.
       *)

    method write_compact_as_latin1 : Pxp_types.output_stream -> unit
      (* Write the contents of this node and the subtrees to the passed
       * output stream; the character set ISO-8859-1 is used. The format
       * is compact (the opposite of "pretty printing").
       *)

    (* ---------------------------------------- *)
    (* internal methods: *)
    method internal_adopt : 'ext node option -> unit
    method internal_delete : 'ext node -> unit
    method internal_init : dtd -> string -> (string * string) list -> unit
  end
;;


class [ 'ext ] data_impl : 'ext -> string -> [ 'ext ] node
    (* Creation:
     *   new data_impl an_extension a_string
     * creates a new data node with the given extension and the given string
     * as content.
     *)
;;


class [ 'ext ] element_impl : 'ext -> [ 'ext ] node
    (* Creation:
     *   new element_impl an_extension
     * creates a new empty element node with the given extension.
     *)
;;


class [ 'ext ] document :
  Pxp_types.collect_warnings -> 
  object
    method init_xml_version : string -> unit
    method init_root : 'ext node -> unit

    method xml_version : string
    method xml_standalone : bool
    method dtd : dtd
    method root : 'ext node

    method add_pinstr : proc_instruction -> unit
    method pinstr : string -> proc_instruction list
    method pinstr_names : string list

    method write_compact_as_latin1 : Pxp_types.output_stream -> unit
      (* Write the document to the passed
       * output stream; the character set ISO-8859-1 is used. The format
       * is compact (the opposite of "pretty printing").
       * If a DTD is present, the DTD is included into the internal subset.
       *)

  end
;;


(* ======================================================================
 * History:
 *
 * $Log: pxp_document.mli,v $
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
