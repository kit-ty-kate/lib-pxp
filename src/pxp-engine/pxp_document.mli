(* $Id: pxp_document.mli,v 1.26 2003/10/03 20:59:08 gerd Exp $
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

(* QUESTIONS:
 * - T_attribute of (string * att_value)
 *   may be better. Attributes do not have attributes (XPATH?)
 *)


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

(* ======================================================================
 * THE DYNAMIC MODIFICATION OF NODE TREES AND VALIDATION
 * ======================================================================
 *
 * The parser creates a node tree while parsing the input text, and the
 * node tree can be modified later by some transformation algorithm. For
 * both tasks the same interface may be used. However, PXP 1.0 introduced
 * an interface that did not separate the two aspects "modification of the
 * tree" and "validation of the tree", i.e. modification methods also
 * did some validation. The following two sections describe: The PXP 1.0
 * model, and the PXP 1.1 changes.
 *
 * -------
 * PXP 1.0
 * -------
 *
 * Method add_node: There are two different modes selected by the optional
 *   argument ~force. ~force:true simply adds the node as last child to the
 *   current node. However, ~force:false (the default) performs some validation
 *   checks that may have three results: (1) The node is added, (2) The node
 *   is silently dropped, (3) An error condition is detected, and an exception
 *   is raised. The mode ~force:false is used by the parser, and historically,
 *   add_node was designed as the parser's method of adding new nodes; ~force
 *   was added later.
 * 
 *   The checks are only performed if the added node is a text node (node type
 *   is T_data), and if the current element node has a type restricting the
 *   addition of text nodes. In detail, the following is checked:
 *    - If the element has type EMPTY, the addition of whitespace text is
 *      not rejected, but the text is dropped (case 2). The addition of
 *      other text material is an error (case 3).
 *    - If the element has a regexp type, the addition of whitespace text is
 *      not rejected, but the text is dropped (case 2); however there is 
 *      a special mode forcing to add such whitespace text nodes (see below).
 *      The addition of other text material is an error (case 3).
 *      Furthermore, it is also an error if whitespace text is added, and the
 *      document is stand-alone, and the element is declared in an external
 *      entity.
 *
 * Method keep_always_whitespace_mode: turns a special mode on forcing that
 *   whitespace text nodes inside regexp-type elements are always added.
 *
 * Method internal_init (i.e. object creation): When an element node is
 *   created, the attribute list is passed as (string * string) list. This
 *   method compares this list with the declared attlist of the DTD, and
 *    - adds missing attributes if the DTD has a default value
 *    - rejects nondeclared attributes
 *    - checks whether required attributes are passed
 *    - parses and normalizes attribute values
 *    - checks some conditions for stand-alone documents
 *
 * Method local_validate: Checks whether the subnodes of the element match
 *   the type of the element.
 *
 * ---------------------
 * PROBLEMS WITH PXP 1.0
 * ---------------------
 *
 * - It is not very obvious when validation checks are performed (which
 *   methods do them and under which conditions)
 * - It is difficult to transform trees because the transformation algorithm
 *   might call a modification method that also performs some validation checks,
 *   but the tree is not yet valid because the algorithm is in the middle of
 *   the transformation
 *
 * -------
 * PXP 1.1
 * -------
 *
 * New method append_node: always adds the node to the node list (same as
 *   add_node ~force:true)
 *
 * New method classify_data_node: performs the checks of add_node ~force:false,
 *   and returns the result:
 *     - CD_other: The node to add is not a text node and cannot be classified
 *     - CD_normal: The text node can be added
 *     - CD_empty: The node is ignorable (= empty), and the containing
 *       element is declared as EMPTY. The parser must not add the node.
 *     - CD_ignorable: The node contains ignorable whitespace, and the parser
 *       should not add the node unless a special configuration forces the
 *       addition
 *     - CD_error: the rules do not allow to add the text node here
 *
 * Method add_node: is now deprecated. For compatibility, the method
 *   classifies the node to add, and decides whether to add the node, not
 *   to add the node, or whether to raise an exception.
 *
 * Method keep_always_whitespace_mode: is removed. A new parser option 
 *   modifies the behaviour of the parser such that ignorable whitespace is
 *   added anyway (option drop_ignorable_whitespace = false).
 *
 * Object creation: You can pass attributes as (string * string) list, and
 *   as (string * att_value) list; internal_init simply processes both lists.
 *   Attributes passed as att_value are already normalized (and compatible
 *   with the stand-alone declaration, if any); the method does not normalize
 *   them again. Two options control validation:
 *     ~valcheck: (default true) It is checked that there
 *         is an element type declaration, or that the DTD is in well-formed
 *         mode. Passing 'false' means that it is not checked whether the
 *         element type exists, that you can add any attributes.
 *
 * New method validate_contents: The new name for local_validate; it is
 *   checked whether the elements contained in the list of sub nodes match
 *   the declared content model. 
 * (The name local_validate is deprecated.)
 *
 * New method validate_attlist: Checks whether the attlist matches the
 *   ATTLIST declaration.
 *   (Impl.: Call create_element again with valcheck options ON.)
 *
 * New method validate: This method can be called after manual modifications
 *   of the tree to ensure that the changed tree is still valid:
 *    - All text subnodes must be classified as non-errorneous
 *    - All element subnodes are validated by validate_subelements
 *    - The attributes are validated by validate_attlist
 *   Note that this method is not used by the parser.
 *)

open Pxp_types
open Pxp_dtd


type node_type =
    T_element of string
  | T_data
  | T_super_root                        (* XPath calls them simply root nodes *)
  | T_pinstr of string                  (* The string is the target of the PI *)
  | T_comment
  | T_none
  | T_attribute of string          (* The string is the name of the attribute *)
  | T_namespace of string            (* The string is the namespace srcprefix *)
  (* <ID:type-node-type>
   * <TYPE:type>
   * <CALL>   [node_type]
   * <SIG>    AUTO
   * <DESCR>  This type enumerates the possible node types:
   *   - [T_element name]: The node is an element and has element type [name]
   *   - [T_data]: The node is a data node
   *   - [T_super_root]: The node is a super root node
   *   - [T_pinstr name]: The node contains a processing instruction with
   *     target [name]
   *   - [T_comment]: The node is a comment
   *   - [T_attribute name]: The node contains an attribute called [name]
   *   - [T_namespace prefix]: The node identifies a namespace for the
   *     [prefix]
   *   - [T_none]: This is a "bottom value" used if there is no reasonable
   *     type.
   *     --
   * </ID>
   *)

  (* About T_super_root, T_pinstr, T_comment: 
   * These types are extensions to my original design. They have mainly
   * been added to simplify the implementation of standards (such as
   * XPath) that require that nodes of these types are included into the
   * main document tree.
   * There are options (see Pxp_yacc) forcing the parser to insert such
   * nodes; in this case, the nodes are actually element nodes serving
   * as wrappers for the additional data structures. The options are:
   * enable_super_root_node, enable_pinstr_nodes, enable_comment_nodes.
   * By default, such nodes are not created.
   *)

  (* About T_attribute, T_namespace:
   * These types are fully virtual. This means that it is impossible
   * to make the parser insert such nodes into the regular tree. They are
   * normally created by special methods to allow additional views on the
   * document tree.
   *)


(* The result type of the method classify_data_node: *)
type data_node_classification =
    CD_normal
  | CD_other
  | CD_empty
  | CD_ignorable
  | CD_error of exn
  (* <ID:type-data-node-classification>
   * <CALL>   [data_node_classification]
   * <SIG>    AUTO
   * <DESCR>  This type enumerates the result values of the method
   *   [classify_data_node]. See the description of this method.
   * </ID>
   *)


(* QUESTION: Perhaps we should reexport att_value here. It is the only
 * type from Pxp_types that is needed regularly.
 *)


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
  (* <ID:type-node>
   * <CALL>   'ext [node]
   * <SIG>    class type 'ext node = object ... end
   * <DESCR>  This is the common class type of all classes representing 
   *    nodes.
   *
   *    Not all classes implement all methods. As the type system of O'Caml
   *    demands that there must be always a method definition for all
   *    methods of the type, methods will raise the exception
   *    [Method_not_applicable] if they are called on a class not supporting
   *    them. The exception [Namespace_method_not_applicable] is reserved
   *    for the special case that a namespace method is invoked on a 
   *    class that does not support namespaces.
   * <SEE> sig-class-type-node
   * </ID>
   *)

  object ('self)
    constraint 'ext = 'ext node #extension

    method extension : 'ext
      (* <ID:type-node-extension>
       * <TYPE:method>
       * <CALL>   obj # [extension]
       * <SIG>    AUTO
       * <DESCR>  Returns the extension object of the node object [obj].
       * <DOMAIN> Applicable to element, data, comment, processing instruction,
       *   and super root nodes.
       * </ID>
       *)

    method remove : unit -> unit
      (* <ID:type-node-remove>
       * <CALL>   obj # [remove] ()
       * <SIG>    AUTO
       * <DESCR>  Removes [obj] from the tree. After this
       *    operation, [obj] is no longer the child of the former father node,
       *    i.e. it does neither occur in the former father's list of children
       *    nor is the former father the parent of [obj]. The node [obj]
       *    becomes orphaned.
       *
       *    If [obj] is already a root, [remove] does nothing.
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN>  Elements, comments, processing instructions, data nodes,
       *   super root nodes.
       * <SEE>     node-delete
       * </ID>
       *)

    method delete : unit
      (* DEPRECATED METHOD
       * remove() does exactly the same
       *)

    method remove_nodes : ?pos:int -> ?len:int -> unit -> unit
      (* <ID:type-node-remove-nodes>
       * <CALL>   obj # [remove_nodes] ~pos ~len ()
       * <SIG>    AUTO
       * <DESCR>  Removes the specified nodes from the list of children of
       *    [obj]. The method deletes the nodes from position [pos] to 
       *    [pos+len-1]. The optional argument [pos] defaults to 0. The 
       *    optional argument [len] defaults to the length of the children
       *    list.
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN> Elements.
       * </ID>
       *)

    method parent : 'ext node
      (* <ID:type-node-parent>
       * <CALL>   obj # [parent]
       * <SIG>    AUTO
       * <DESCR>  Get the parent node, or raise [Not_found] if this node is
       *   a root node. For attribute and namespace nodes, the parent is
       *   artificially defined as the element to which these nodes apply.
       * <DOMAIN> All node types.
       * </ID>
       *)

    method root : 'ext node
      (* <ID:type-node-root>
       * <CALL>   obj # [root]
       * <SIG>    AUTO
       * <DESCR>  Gets the root node of the tree.
       *   Every node is contained in a tree with a root, so this method always 
       *   succeeds. Note that this method searches the root,
       *   which costs time proportional to the length of the path to the root.
       * <DOMAIN> All node types.
       * </ID>
       *)

    method orphaned_clone : 'self
      (* <ID:type-node-orphaned-clone>
       * <CALL>   obj # [orphaned_clone]
       * <SIG>    AUTO
       * <DESCR>  Returns a clone of the node and the complete tree below
       *    this node (deep clone). The clone does not have a parent (i.e. the
       *    reference to the parent node is not cloned). While copying the
       *    subtree strings are skipped; normally the original tree and the
       *    copy tree share strings. Extension objects are cloned by invoking
       *    the [clone] method on the original objects; how much of
       *    the extension objects is cloned depends on the implemention of
       *    this method.
       * <DOMAIN> All node types.
       * <SEE> node-clone
       * </ID>
       *)

    method orphaned_flat_clone : 'self
      (* <ID:type-node-orphaned-flat-clone>
       * <CALL>   obj # [orphaned_flat_clone]
       * <SIG>    AUTO
       * <DESCR>  return a clone of this element where all subnodes are omitted.
       *     The type of the node, and the attributes are the same as in the
       *     original node. The clone has no parent.
       * <DOMAIN> All node types.
       * </ID>
       *)

    method append_node : 'ext node -> unit
      (* <ID:type-node-append-node>
       * <CALL>   obj # [append_node] n
       * <SIG>    AUTO
       * <DESCR>  Adds the node [n] to the list of children of [obj]. The 
       *   method expects that [n] is a root, and it requires that [n] and
       *   [obj] share the same DTD.
       *
       *   Note: This method does not check whether the modified XML tree
       *   is still valid.
       * <DOMAIN> This method is only applicable to element nodes.
       * <SEE> node-add
       * </ID>
       *)

    method classify_data_node : 'ext node -> data_node_classification
      (* <ID:type-node-classify-data-node>
       * <CALL>   obj # [classify_data_node] n
       * <SIG>    AUTO
       * <DESCR>  Classifies the passed data node [n], and returns whether it
       *      is reasonable to append the data node to the list of subnodes
       *     (using [append_node]). The following return values are possible:
       *      - [CD_normal]: Adding [n] does not violate any validation 
       *        constraint
       *      - [CD_other]: [n] is not a data node
       *      - [CD_empty]: The element [obj] is declared as [EMTPY], and
       *        [n] contains the empty string. It is allowed to append
       *        [n] but it does not make sense
       *      - [CD_ignorable]: The element [obj] is declared such that
       *        it is forbidden to put character data into it. However,
       *        the node [n] only contains white space which is allowed
       *        as an exception to this rule. This means that it is allowed
       *        to append [n] but [n] would not contain any information
       *        except formatting hints.
       *      - [CD_error e]: It is an error to append [n]. The exception
       *        [e], usually a [Validation_error], contains details about
       *        the problem.
       *      --
       * Note that the method always returns and never raises an exception.
       * <DOMAIN> Elements.
       * </ID>
       *)

    method add_node : ?force:bool -> 'ext node -> unit
      (* add_node is now DEPRECATED; use append_node instead! *)
      (* Append new sub nodes -- mainly used by the parser itself, but
       * of course open for everybody. If an element is added, it must be
       * an orphan (i.e. does not have a parent node); and after addition
       * *this* node is the new parent.
       * The method performs some basic validation checks if the current node
       * has a regular expression as content model, or is EMPTY. You can
       * turn these checks off by passing ~force:true to the method.
       *)

    method insert_nodes : ?pos:int -> 'ext node list -> unit
      (* <ID:type-node-insert-nodes>
       * <CALL>   obj # [insert_nodes] ~pos nl
       * <SIG>    AUTO
       * <DESCR>  Inserts the list of nodes [nl] in-place into the list of
       *    children of [obj]. The insertion is performed at position [pos],
       *    i.e. in the modified list of children, the first element of
       *    [nl] will have position [pos]. If the optional argument [pos]
       *    is not passed to the method, the list [nl] is appended
       *    to the list of children. 
       *
       *    The method requires that all elements of
       *    the list [nl] are roots, and that all elements and [obj]
       *    share the same DTD.
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN>  Elements.
       * </ID>
       *)

    method set_nodes : 'ext node list -> unit
      (* <ID:type-node-set-nodes>
       * <CALL>   obj # [set_nodes] l
       * <SIG>    AUTO
       * <DESCR>  Sets the list of children to [l]. It is required that
       *     every member of [l] is either a root or was already a children
       *     of this node before the method call, and it is required that 
       *     all members and the current object share the same DTD.
       *
       *     Former children which are not members of [l] are removed from
       *     the tree and get orphaned (see method [remove]).
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN>  Elements.
       * </ID>
       *)

    method add_pinstr : proc_instruction -> unit
      (* <ID:type-node-add-pinstr>
       * <CALL>   obj # [add_pinstr] pi
       * <SIG>    AUTO
       * <DESCR>  Adds the processing instruction [pi] to the set of
       *   processing instructions contained in [obj]. If [obj] is an
       *   element node, you can add any number of processing instructions.
       *   If [obj] is a processing instruction node, you can put at most
       *   one processing instruction into this node.
       * <DOMAIN> Elements, and processing instruction nodes.
       * </ID>
       *)

    method pinstr : string -> proc_instruction list
      (* <ID:type-node-pinstr>
       * <CALL>   obj # [pinstr] n
       * <SIG>    AUTO
       * <DESCR>  Returns all processing instructions that are
       *   directly contained in [obj] and that have a target
       *   specification of [n].
       * <DOMAIN> All node types. However, this method is only reasonable
       *   for processing instruction nodes, and for elements; for all
       *   other node types the method will return the empty list. Note
       *   that the parser can be configured such that it creates 
       *   processing instruction nodes or not; in the first case, only
       *   the processing instruction nodes contain processing instruction,
       *   in the latter case, only the elements embracing the instructions
       *   contain them.
       * </ID>
       *)

    method pinstr_names : string list
      (* <ID:type-node-pinstr-names>
       * <CALL>   obj # [pinstr_names]
       * <SIG>    AUTO
       * <DESCR>  Returns the targets of all processing instructions that are
       *   directly contained in [obj].
       * <DOMAIN> All node types. However, this method is only reasonable
       *   for processing instruction nodes, and for elements; for all
       *   other node types the method will return the empty list. Note
       *   that the parser can be configured such that it creates 
       *   processing instruction nodes or not; in the first case, only
       *   the processing instruction nodes contain processing instruction,
       *   in the latter case, only the elements embracing the instructions
       *   contain them.
       * </ID>
       *)

    method node_position : int
      (* <ID:type-node-node-position>
       * <CALL>   obj # [node_position]
       * <SIG>    AUTO
       * <DESCR>  Returns the position of [obj] among all children of the parent
       *   node. Positions are counted from 0. There are several cases:
       *    - The regular nodes get positions from 0 to l-1 where l is the
       *      length of the list of regular children.
       *    - Attribute nodes and namespace nodes are irregular nodes, 
       *      which means here that their positions are counted seperately.
       *      All attribute nodes have positions from 0 to m-1; all namespace
       *      nodes have positions from 0 to n-1.
       *    - If [obj] is a root, this method raises [Not_found]
       *      --
       * <DOMAIN> All node types.
       * </ID>
       *)

    method node_path : int list
      (* <ID:type-node-node-path>
       * <CALL>  obj # [node_path]
       * <SIG>    AUTO
       * <DESCR> Returns the list of node positions describing
       *   the location of this node in the whole tree. The list describes 
       *   the path from the root node down to this node; the first path
       *   element is the index of the child of the root, the second path
       *   element is the index of the child of the child, and so on, and
       *   the last path element is the index of this node. The method returns 
       *   [[[]]] if this node is the root node.
       *
       *   Attribute and namespace nodes are not part of the regular tree, so 
       *   there is a special rule for them. Attribute nodes of an element 
       *   node [x] have the node path [[x # node_path @ [-1; p]]] where 
       *   [p] is the position of the attribute node. Namespace nodes of an 
       *   element node [x] have the node path [[x # node_path @ [-2; p]]] 
       *   where [p] is the position of the namespace node.
       *   (This definition respects the document order.)
       * <DOMAIN> All node types.
       * </ID>
       *)

    method sub_nodes : 'ext node list
      (* <ID:type-node-sub-nodes>
       * <CALL>   obj # [sub_nodes]
       * <SIG>    AUTO
       * <DESCR>  Returns the regular children of the node as list. Only
       *   Elements, data nodes, comments, and processing instructions can
       *   occur in this list; attributes and namespace nodes are not
       *   considered as regular nodes, and super root nodes can only
       *   be root nodes and will never be children of another node.
       *   The returned list is always empty if [obj] is a data node,
       *   comment, processing instruction, attribute, or namespace.
       * <DOMAIN> All node types.
       * </ID>
       *)

    method iter_nodes : ('ext node -> unit) -> unit
      (* <ID:type-node-iter-nodes>
       * <CALL>   obj # [iter_nodes] f
       * <SIG>    AUTO
       * <DESCR>  Iterates over the regular children of [obj], and
       *   calls the function [f] for every child ch: [f ch]. The
       *   regular children are the nodes returned by [sub_nodes], see
       *   there for an explanation.
       * <DOMAIN> All node types.
       * <SEE>    document-iterators
       * </ID>
       *)

    method iter_nodes_sibl :
      ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
      (* <ID:type-node-iter-nodes-sibl>
       * <CALL>   obj # [iter_nodes_sibl] f
       * <SIG>    AUTO
       * <DESCR>  Iterates over the regular children of [obj], and
       *   calls the function [f] for every child: [f pred ch succ].
       *   - [ch] is the child
       *   - [pred] is [None] if the child is the first in the list,
       *     and [Some p] otherwise; [p] is the predecessor of [ch]
       *   - [succ] is [None] if the child is the last in the list,
       *     and [Some s] otherwise; [s] is the successor of [ch]
       *     --
       *   The
       *   regular children are the nodes returned by [sub_nodes], see
       *   there for an explanation.
       * <DOMAIN> All node types.
       * <SEE>    document-iterators
       * </ID>
       *)

    method nth_node : int -> 'ext node
      (* <ID:type-node-nth-node>
       * <CALL>   obj # [nth_node] n
       * <SIG>    AUTO
       * <DESCR>  Returns the n-th regular child of [obj], [n >= 0].
       *    Raises [Not_found] if the index [n] is out of the valid range.
       * <DOMAIN> All node types.
       * </ID>
       *)

    method previous_node : 'ext node
      (* <ID:type-node-previous-node>
       * <CALL>   obj # [previous_node]
       * <SIG>    AUTO
       * <DESCR>  Returns the predecessor of [obj]
       *   in the list of regular children of the parent, or raise [Not_found]
       *   if this node is the first child. This is equivalent to
       *   [obj # parent # nth_node (obj # node_position - 1)].
       * <DOMAIN> All node types.
       * </ID>
       *)

    method next_node : 'ext node
      (* <ID:type-node-next-node>
       * <CALL>   obj # [next_node]
       * <SIG>    AUTO
       * <DESCR>  Returns the successor of [obj]
       *   in the list of regular children of the parent, or raise [Not_found]
       *   if this node is the last child. This is equivalent to
       *   [obj # parent # nth_node (obj # node_position + 1)].
       * <DOMAIN> All node types.
       * </ID>
       *)

    method data : string
      (* <ID:type-node-data>
       * <CALL>   obj # [data]
       * <SIG>    AUTO
       * <DESCR>  This method returns what is considered as
       *   the data of the node which depends on the node type:
       *    - Data nodes: the method returns the character string the node 
       *      represents
       *    - Element nodes, super root nodes: the method returns the
       *      concatenated character strings of all (direct or indirect)
       *      data nodes below [obj]
       *    - Comment nodes: the method returns the
       *      comment string (without delimiters), or it raises Not_found if the
       *      comment string is not set
       *    - Processing instructions: the
       *      method returns the data part of the instruction, or "" if the data
       *      part is missing
       *    - Attribute nodes: the method returns the attribute
       *      value as string, or it raises [Not_found] if the attribute
       *      is implied.
       *    - Namespace nodes: the method returns the namespace
       *      URI
       *      --
       * <DOMAIN> All node types.
       * </ID>
       *)

    method set_data : string -> unit
      (* <ID:type-node-set-data>
       * <CALL>   obj # [set_data] s
       * <SIG>    AUTO
       * <DESCR>  This method sets the character string contained in 
       *   data nodes.
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN> Data nodes.
       * </ID>
       *)

    method node_type : node_type
      (* <ID:type-node-node-type>
       * <CALL>   obj # [node_type]
       * <SIG>    AUTO
       * <DESCR>  Returns the type of [obj]:
       *   - [T_element t]: The node is an element with type [t]
       *   - [T_data]: The node is a data node
       *   - [T_comment]: The node is a comment node
       *   - [T_pinstr n]: The node is a processing instruction with
       *     target [n]
       *   - [T_super_root]: The node is a super root node
       *   - [T_attribute n]: The node is an attribute with name [n]
       *   - [T_namespace p]: The node is a namespace with prefix [p]
       *     --
       * <DOMAIN> All node types.
       * </ID>
       * XXX: <SEE> Where attribute and namespace nodes are discussed
       *)

    method position : (string * int * int)
      (* <ID:type-node-position>
       * <CALL>   obj # [position]
       * <SIG>    AUTO
       * <DESCR>  Returns a triple [(entity,line,pos)] describing the 
       *   location of the element in the original XML text. This triple is
       *   only available for elements, and only if the parser has been
       *   configured to store positions (see parser option
       *   [store_element_positions]). If available, [entity] describes 
       *   the entity where the element occurred, [line] is the line number
       *   [>= 1], and [pos] is the byte position of the first character
       *   of the element in the line. 
       *
       *   If unavailable, the method will return the triple [("?",0,0)].
       * <DOMAIN> All node types. Note that the method will always return
       *   [("?",0,0)] for non-element nodes.
       * </ID>
       *)

    method attribute : string -> Pxp_core_types.att_value
      (* <ID:type-node-attribute>
       * <CALL>   obj # [attribute] name
       * <SIG>    AUTO
       * <DESCR>  Returns the value of the attribute [name].
       *
       *   If the parser is in validating mode, the method is able to return
       *   values for declared attributes, and it raises [Not_found] for any 
       *   undeclared attribute. Note that it even returns a value if the
       *   attribute is actually missing but is declared as [#IMPLIED] or 
       *   has a default value. 
       *
       *   If the parser (more precisely, the DTD object) is in 
       *   well-formedness mode, the method is able to return values for 
       *   defined attributes, and it raises [Not_found] for any
       *   unknown attribute name.
       *
       *   Possible return values are:
       *     -  [Implied_value]: The attribute has been declared with the
       *        keyword [#IMPLIED], and the attribute definition is missing
       *        in the attribute list of the element.
       *     -  [Value s]: The attribute has been declared as type [CDATA], 
       *        as [ID], as [IDREF], as [ENTITY], or as [NMTOKEN], or as 
       *        enumeration or notation, and one of the two conditions holds: 
       *        (1) The attribute value is defined in the attribute list in
       *        which case this value is returned in the string [s]. (2) The
       *        attribute has been omitted, and the DTD declares the attribute
       *        with a default value. The default value is returned in [s]. 
       *        
       *        Summarized, [Value s] is returned for non-implied, non-list 
       *        attribute values.
       *
       *        Furthermore, [Value s] is returned for non-declared attributes
       *        if the DTD object allows this, for instance, if the DTD
       *        object specifies well-formedness mode.
       *     -  [Valuelist l]: The attribute has been declared as type
       *        [IDREFS], as [ENTITIES], or [NMTOKENS], and one of the two
       *        conditions holds: (1) The attribute value is defined in the 
       *        attribute list in which case the space-separated tokens of
       *        the value are returned in the string list [l]. (2) The
       *        attribute has been omitted, and the DTD declares the attribute 
       *        with a default value. The default value is returned in [l]. 
       *
       *        Summarized, [Valuelist l] is returned for all list-type
       *        attribute values.
       *        --
       *   Note that before the attribute value is returned, the value is
       *   normalized. This means that newlines are converted to spaces, and
       *   that references to character entities (i.e. [&#n;]) and
       *   general entities (i.e. [&name;]) are expanded; if necessary, 
       *   the expansion is performed recursively.
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return values, all other node types always raise [Not_found].
       * </ID>
       *)


    method attribute_names : string list
      (* <ID:type-node-attribute-names>
       * <CALL>    obj # [attribute_names]
       * <SIG>    AUTO
       * <DESCR>   Returns the list of all attribute names of this element.
       *   In validating mode, this list is simply the list of declared
       *   attributes. In well-formedness mode, this list is the list of
       *   defined attributes.
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return a non-empty list, all other node types always return
       *   the empty list.
       * </ID>
       *)

    method attribute_type : string -> Pxp_core_types.att_type
      (* <ID:type-node-attribute-type>
       * <CALL>   obj # [attribute_type] name
       * <SIG>    AUTO
       * <DESCR>  Returns the type of the attribute [name]. If the attribute
       *   is declared, the declared type is returned. If the attribute is
       *   defined but undeclared, the type [A_cdata] will be returned.
       *   (The module [Pxp_types] contains the Caml type of attribute types.)
       *   This method raises [Not_found] if the attribute is unknown.
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return values, all other node types always raise [Not_found].
       * </ID>
       *)

    method attributes : (string * Pxp_core_types.att_value) list
      (* <ID:type-node-attributes>
       * <CALL>   obj # [attributes]
       * <SIG>    AUTO
       * <DESCR>  Returns the list of [(name,value)] pairs describing
       *    all attributes (declared attributes plus defined attributes).
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return non-empty values, all other node types always
       *   return the empty list.
       * </ID>
       *)

    method required_string_attribute : string -> string
      (* <ID:type-node-required-string-attribute>
       * <CALL>    obj # [required_string_attribute] name
       * <SIG>    AUTO
       * <DESCR>   Returns the value of the attribute [name] as string,
       *    i.e. if the value of the attribute is [Value s], this method
       *    will return simply [s], and if the value is [Valuelist l],
       *    this method will return the elements of [l] separated by
       *    spaces. If the attribute value is [Implied_value], the method
       *    will fail.
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return values, all other node types always fail.
       * </ID>
       *)

    method required_list_attribute : string -> string list
      (* <ID:type-node-required-list-attribute>
       * <CALL>    obj # [required_list_attribute] name
       * <SIG>    AUTO
       * <DESCR>   Returns the value of the attribute [name] as string list,
       *    i.e. if the value of the attribute is [Valuelist l], this method
       *    will return simply [l], and if the value is [Value s],
       *    this method will return the one-element list [[[s]]].
       *    If the attribute value is [Implied_value], the method
       *    will fail.
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return values, all other node types always fail.
       * </ID>
       *)

    method optional_string_attribute : string -> string option
      (* <ID:type-node-optional-string-attribute>
       * <CALL>    obj # [optional_string_attribute] name
       * <SIG>    AUTO
       * <DESCR>   Returns the value of the attribute [name] as optional string,
       *    i.e. if the value of the attribute is [Value s], this method
       *    will return [Some s], and if the value is [Valuelist l],
       *    this method will return [Some s] where [s] consists of the
       *    concatenated elements of [l] separated by spaces. If the
       *    attribute value is [Implied_value], the method will return [None].
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return [Some] values, all other node types always return [None].
       * </ID>
       *)

    method optional_list_attribute : string -> string list
      (* <ID:type-node-optional-list-attribute>
       * <CALL>    obj # [required_list_attribute] name
       * <SIG>    AUTO
       * <DESCR>   Returns the value of the attribute [name] as string list,
       *    i.e. if the value of the attribute is [Valuelist l], this method
       *    will return simply [l], and if the value is [Value s],
       *    this method will return the one-element list [[[s]]].
       *    If the attribute value is [Implied_value], the method
       *    will return the empty list [[[]]].
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return non-empty values, all other node types always
       *   return the empty list.
       * </ID>
       *)

    method id_attribute_name : string
      (* <ID:type-node-id-attribute-name>
       * <CALL>   obj # [id_attribute_name]
       * <SIG>    AUTO
       * <DESCR>  Returns the name of the (at most one) attribute being
       *    declared as type [ID]. The method raises [Not_found] if there 
       *    is no declared [ID] attribute for the element type.
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return names, all other node types always raise [Not_found].
       * </ID>
       *)

    method id_attribute_value : string
      (* <ID:type-node-id-attribute-value>
       * <CALL>   obj # [id_attribute_value]
       * <SIG>    AUTO
       * <DESCR>  Returns the string value of the (at most one) attribute being
       *    declared as type [ID]. The method raises [Not_found] if there 
       *    is no declared [ID] attribute for the element type.
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return names, all other node types always raise [Not_found].
       * </ID>
       *)

    method idref_attribute_names : string list
      (* <ID:type-node-idref-attribute-names>
       * <CALL>   obj # [idref_attribute_names]
       * <SIG>    AUTO
       * <DESCR>  Returns the names of the attributes being
       *    declared as type [IDREF] or [IDREFS]. 
       * <DOMAIN> All node types. However, only elements and attribute nodes
       *   will return names, all other node types always return the empty
       *   list.
       * </ID>
       *)

    method quick_set_attributes : 
             (string * Pxp_core_types.att_value) list -> unit
      (* DEPRECATED METHOD! set_attributes does exactly the same. *)

    method set_attributes : (string * Pxp_core_types.att_value) list -> unit
      (* <ID:type-node-set-attributes>
       * <CALL>   obj # [set_attributes] al
       * <SIG>    AUTO
       * <DESCR>  Sets the attributes of this element to [al].
       *
       *    Note that this method does not add missing attributes that are
       *    declared in the DTD. It also never rejects undeclared attributes.
       *    The passed values are not checked.
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN>   Elements.
       * </ID>
       *)

    method set_attribute : ?force:bool -> string -> Pxp_core_types.att_value -> unit
      (* <ID:type-node-set-attribute>
       * <CALL>   obj # [set_attribute] ~force n v
       * <SIG>    AUTO
       * <DESCR>  Sets the attribute [n] of this element to the value [v].
       *    By default, it is required that the attribute [n] has already
       *    some value. If you pass ~force:true, the attribute is added
       *    to the attribute list if it is missing.
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN>   Elements.
       * </ID>
       *)

    method reset_attribute : string -> unit
      (* <ID:type-node-reset-attribute>
       * <CALL>   obj # [reset_attribute] n
       * <SIG>    AUTO
       * <DESCR>  If the attribute [n] is a declared attribute, it is set
       *   to its default value, or to [Implied_value] if there is no default 
       *   (the latter is performed even if the attribute is [#REQUIRED]).
       *   If the attribute is an undeclared attribute, it is removed
       *   from the attribute list.
       *
       *   The idea of this method is to simulate what had happened if [n]
       *   had not been defined in the attribute list of the XML element.
       *   In validating mode, the parser would have chosen the default
       *   value if possible, or [Implied_value] otherwise, and in 
       *   well-formedness mode, the attribute would be simply missing
       *   in the attribute list.
       *
       *   Note: It is intentionally not possible to remove a declared
       *   attribute. (However, you can remove it by calling
       *   set_attributes, but this would be very inefficient.)
       *
       *    Note: This method does not check whether the modified XML tree
       *    is still valid.
       * <DOMAIN>   Elements.
       * </ID>
       *)


    method attributes_as_nodes : 'ext node list
      (* <ID:type-node-attributes-as-nodes>
       * <CALL>   obj # [attributes_as_nodes]
       * <SIG>    AUTO
       * <DESCR>  Returns all attributes (i.e. declared plus defined
       *   attributes) as a list of attribute nodes with node type 
       *   [T_attribute name]. 
       *
       *   This method should be used if it is required for typing reasons
       *   that the attributes have also type [node]. A common example
       *   are sets that may both contain elements and attributes, as they
       *   are used in the XPath language.
       *
       *   The attribute nodes are read-only; any call to a method
       *   modifying their contents will raise [Method_not_applicable].
       *   In order to get the value of such an attribute node [anode],
       *   one can invoke the method [attribute]:
       *
       *   [anode # attribute name]
       *
       *   where [name] is the name of the attribute represented by
       *   [anode]. This will return the attribute value as [att_value]. Of
       *   course, the other attribute observers can be applied as well. 
       *   Furthermore, the method [data] will return the attribute value as
       *   string. However, every attribute node only contains the value of the
       *   one attribute it represents, and it does not make sense to pass
       *   names of other attributes to the observer methods.
       *   
       *   The attribute nodes live outside of the regular XML tree, and
       *   they are not considered as children of the element node. However,
       *   the element node is the parent node of the attribute nodes 
       *   (i.e. the children/parent relationship is asymmetric).
       *
       *   The method [attributes_as_nodes] computes the list of attribute
       *   nodes when it is first invoked, and it will return the same list
       *   again in subsequent invocations.
       * <DOMAIN>  This method is only applicable to elements.
       * </ID>
       *)

    method set_comment : string option -> unit
      (* <ID:type-node-set-comment>
       * <CALL>   obj # [set_comment] c
       * <SIG>    AUTO
       * <DESCR>  Sets the comment string contained in comment nodes, if
       *    [c = Some s]. Otherwise, this method removes the comment string
       *    ([c = None]).
       *
       *    Note that the comment string must not include the delimiters
       *    [<--] and [-->]. Furthermore, it must not contain any character
       *    or character sequence that are forbidden in comments, such
       *    as ["--"]. However, this method does not check this condition.
       * <DOMAIN>  Comment nodes.
       * </ID>
       *)

    method comment : string option
      (* <ID:type-node-comment>
       * <CALL>   obj # [comment]
       * <SIG>    AUTO
       * <DESCR>  Returns [Some text] if the node is a comment node and if
       *    [text] is the comment string (without the delimiters [<--] and
       *    [-->]). Otherwise, [None] is passed back.
       *
       *    Note: The [data] method also returns the comment string, but it
       *    raises [Not_found] if the string is not available.
       * <DOMAIN> All node types. Note that the method will always return
       *    [None] for non-comment nodes.
       * </ID>
       *)

    method normprefix : string
      (* <ID:type-node-normprefix>
       * <CALL>   obj # [normprefix]
       * <SIG>    AUTO
       * <DESCR>  For namespace-aware implementations of the node class, this
       *     method returns the normalized prefix of the element or attribute.
       *     If the object does not have a prefix, "" will be passed back.
       *
       *     The normalized prefix is the part of the name before the 
       *     colon. It is normalized because the parser ensures that every
       *     prefix corresponds only to one namespace. Note that the
       *     prefix can be different than in the parsed XML source because
       *     the normalization step needs to change the prefix to avoid
       *     prefix conflicts.
       * <DOMAIN> Elements and attributes supporting namespaces.
       * </ID>
       *)

    method localname : string
      (* <ID:type-node-localname>
       * <CALL>   obj # [localname]
       * <SIG>    AUTO
       * <DESCR>  For namespace-aware implementations of the node class, this
       *     method returns the local part of the name of the element or
       *     attribute.
       *
       *     The local name is the part of the name after the colon, or
       *     the whole name if there is no colon.
       * <DOMAIN> Elements and attributes supporting namespaces.
       * </ID>
       *)

    method namespace_uri : string
      (* <ID:type-node-namespace-uri>
       * <CALL>   obj # [namespace_uri]
       * <SIG>    AUTO
       * <DESCR>  For namespace-aware implementations of the node class, this
       *     method returns the namespace URI of the element, attribute or
       *     namespace. It is required that a namespace manager is available.
       *
       *     If the node does not have a namespace prefix, and there is no
       *     default namespace, this method returns "".
       *
       *     The namespace URI is the unique name of the namespace.
       * <DOMAIN> Elements and attributes supporting namespaces; furthermore
       *     namespace nodes.
       * </ID>
       *)

    method namespace_manager : namespace_manager
      (* <ID:type-node-namespace-manager>
       * <CALL>   obj # [namespace_manager]
       * <SIG>    AUTO
       * <DESCR>  For namespace-aware implementations of the node class,
       *      this method returns the namespace manager. If the namespace
       *      manager has not been set, the exception [Not_found] is raised.
       *
       *      The namespace manager is an object that holds the mapping
       *      from namespace prefixes to namespace URIs, and vice versa.
       *      It is contained in the DTD.
       * <DOMAIN> Elements and attributes supporting namespaces; furthermore
       *     namespace nodes.
       * </ID>
       *)

    method namespace_info : 'ext namespace_info
      (* <ID:type-node-namespace-info>
       * <CALL>   obj # [namespace_info]
       * <SIG>    AUTO
       * <DESCR>  Returns additional information about the namespace prefixes
       *     in the parsed XML source. This method has been added for
       *     better XPath conformance. Note that it is still experimental
       *     and it is likely that it will be changed.
       *
       *     This record is only available if the parser has been configured
       * to support namespaces, and if the parser has been configured
       * to set this record (requires a lot of memory). Furthermore, only
       * the implementation namespace_element_impl supports this method.
       *
       * This method raises [Not_found] if the [namespace_info] field has not
       * been set.
       * <DOMAIN> Elements supporting namespaces.
       * </ID>
       *)

    method dtd : dtd
      (* <ID:type-node-dtd>
       * <CALL>   obj # [dtd]
       * <SIG>    AUTO
       * <DESCR>  Returns the DTD.
       * <DOMAIN> All node types. Note (1) that exemplars need not to have
       *   an associated DTD, in which case this method fails. (2) Even
       *   in well-formedness mode every node has a DTD object;
       *   this object specifies well-formedness mode.
       * </ID>
       *)

    method encoding : Pxp_core_types.rep_encoding
      (* <ID:type-node-encoding>
       * <CALL>   obj # [encoding]
       * <SIG>    AUTO
       * <DESCR>  Get the encoding which is always the same as the encoding of 
       *   the DTD. See also method [dtd]. (Note: This method fails, too, if
       *   no DTD is present.)
       * <DOMAIN> All node types. Note that exemplars need not to have
       *   an associated DTD, in which case this method fails.
       * </ID>
       *)

    method create_element :
             ?name_pool_for_attribute_values:Pxp_core_types.pool ->
             ?position:(string * int * int) ->
	     ?valcheck:bool ->      (* default: true *)
	     ?att_values:((string * Pxp_core_types.att_value) list) ->
             dtd -> node_type -> (string * string) list -> 'ext node
      (* <ID:type-node-create-element>
       * <CALL>   obj # [create_element] ~name_pool_for_attribute_values ~position ~valcheck ~att_values 
       *          dtd ntype att_list
       * <SIG>    AUTO
       * <DESCR>  Returns a flat copy of this element node with the following
       *    modifications: 
       *     - The DTD is set to [dtd]
       *     - The node type is set to [ntype] (which must be [T_element name])
       *     - The attribute list is set to the concatenation of 
       *       [att_list] and [att_values]; [att_list] passes attribute values
       *       as strings while [att_values] passes attribute values as
       *       type [att_value]
       *     - The copy does not have children nor a parent
       *     - The copy does not contain processing instructions.
       *     - The position triple is set to [position]
       *       --
       *   Note that the extension object is copied, too.
       *
       *   If [valcheck = true] (the default), it is checked whether the 
       *   element type exists and whether the passed attributes match the
       *   declared attribute list. Missing attributes are automatically
       *   added, if possible. If [valcheck = false], any element type
       *   and any attributes are accepted.
       *
       *   If a [name_pool_for_attribute_values] is passed, the attribute
       *   values in [att_list] are put into this pool.
       *
       *   The optional arguments have the following defaults:
       *    - [~name_pool_for_attribute_values]: No pool is used
       *    - [~position]: The position is not available in the copy
       *    - [~valcheck]: false
       *    - [~att_values]: empty
       *      --
       * <DOMAIN> Elements.
       * <SEE> type-node-ex-create-element
       * </ID>
       *)

    method create_data : dtd -> string -> 'ext node
      (* <ID:type-node-create-data>
       * <CALL>   obj # [create_data] dtd cdata
       * <SIG>    AUTO
       * <DESCR>  Returns a flat copy of this data node with the following
       *    modifications: 
       *     - The DTD is set to [dtd]
       *     - The character string is set to [cdata]
       *       --
       *   Note that the extension object is copied, too.
       * <DOMAIN> Data nodes.
       * <SEE> type-node-ex-create-data
       * </ID>
       *)

    method create_other : 
             ?position:(string * int * int) ->
             dtd -> node_type -> 'ext node
      (* <ID:type-node-create-other>
       * <CALL>   obj # [create_other] ~position dtd ntype
       * <SIG>    AUTO
       * <DESCR>  Returns a flat copy of this node with the following
       *   modification:
       *     - The DTD is set to [dtd]
       *     - The position triple is set to [position]
       *       --
       *   Note that the extension object is copied, too.
       *
       *   The passed node type [ntype] must match the node type
       *   of [obj].
       * <DOMAIN> Super root nodes, processing instruction nodes,
       *    comment nodes
       * </ID>
       *)


    method local_validate : 
              ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
      (* DEPRECATED NAME of validate_contents. *)

    method validate_contents : 
              ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
      (* <ID:type-node-validate-contents>
       * <CALL>   obj # [validate_contents] ?use_dfa ?check_data_nodes ()
       * <SIG>    AUTO
       * <DESCR>  Checks that the subnodes of this element match the declared
       *     content model of this element. The method returns [()] if
       *     the element is okay, and it raises an exception if an error
       *     is found (in most cases [Validation_error]).
       *
       *     This check is always performed by the parser, such that
       *     software that only reads parsed XML trees needs not call
       *     this method. However, if software modifies the tree itself,
       *     an invocation of this method ensures that the validation
       *     constraints about content models are fulfilled.
       *
       *     Note that the check is not performed recursively.
       *  
       *     - Option [~use_dfa]: If true, the deterministic finite automaton of
       *       regexp content models is used for validation, if available.
       *       Defaults to false.
       *     - Option [~check_data_nodes]: If true, it is checked whether data
       *       nodes only occur at valid positions. If false, these checks
       *       are left out. Defaults to true. (Usually, the parser turns
       *       this feature off because the parser already performs a similar
       *       check.)
       *
       *       See [classify_data_node] for details about what is checked.
       *       --
       *
       * In previous releases of PXP, this method was called [local_validate].
       * <DOMAIN> All node types. However, there are only real checks for
       *    elements; for other nodes, this method is a no-op.
       * </ID>
       *)

    method complement_attlist : unit -> unit
      (* <ID:type-node-complement-attlist>
       * <CALL>   obj # [complement_attlist] ()
       * <SIG>    AUTO
       * <DESCR>  Adds attributes that are declared in the DTD but are
       *     currently missing: [#IMPLIED] attributes are added with 
       *     [Implied_value], and if there is a default value for an attribute, 
       *     this value is added. [#REQUIRED] attributes are set to
       *     [Implied_value], too.
       * 
       *     It is only necessary to call this method if the element is created
       *     with ~valcheck:false, or the attribute list has been modified,
       *     and the element must be validated.
       * <DOMAIN> Elements.
       * </ID>
       *)

    method validate_attlist : unit -> unit
      (* <ID:type-node-validate-attlist>
       * <CALL>   obj # [validate_attlist] ()
       * <SIG>    AUTO
       * <DESCR>  Checks whether the attribute list of the element [obj] 
       *    matches the declared attribute list. The method returns [()]
       *    if the attribute list is formed correctly, and it raises an
       *    exception (usually a [Validation_error]) if there is an error.
       *
       *    This check is implicitly performed by [create_element] unless
       * the option [~valcheck:false] has been passed. This means that it
       * is usually not necessary to call this method; however, if the
       * attribute list has been changed by [set_attributes] or if 
       * [~valcheck:false] is in effect, the invocation of this method
       * ensures the validity of the attribute list.
       *
       * Note that the method complains about missing attributes even
       * if these attributes have been declared with a default value or as
       * being [#IMPLIED]; this method only checks the attributes but does
       * not modify the attribute list. If you know that attributes are
       * missing and you want to add them automatically just as 
       * [create_element] does, you can call [complement_attlist] before
       * doing this check.
       * <DOMAIN> All node types. However, for non-element nodes this
       *   check is a no-op.
       * </ID>
       *)

    method validate : unit -> unit
      (* <ID:type-node-validate>
       * <CALL>   obj # [validate] ()
       * <SIG>    AUTO
       * <DESCR>  Calls [validate_contents] and [validate_attlist], and
       *     ensures that this element is locally valid. The method 
       *     returns [()] if the element is valid, and raises an exception
       *     otherwise.
       * <DOMAIN> All node types. However, for non-element nodes this
       *   check is a no-op.
       * </ID>
       *)

    (* method keep_always_whitespace_mode : unit *)
      (* This method has been removed. You can now set the handling of
       * ignorable whitespace by a new Pxp_yacc.config option:
       * [drop_ignorable_whitespace]
       *)

    method write : 
             ?prefixes:string list ->
	     ?default:string ->
             Pxp_core_types.output_stream -> Pxp_core_types.encoding -> unit
      (* <ID:type-node-write>
       * <CALL>   obj # [write] ~prefixes stream enc
       * <SIG>    AUTO
       * <DESCR>  Write the contents of this node and the subtrees to the passed
       *    [stream] encoded as [enc]. The generated output is again XML.
       *    The output style is rather compact and should not be considered
       *    as "pretty printing".
       *
       *   Option [~prefixes]: The class [namespace_element_impl] interprets 
       *   this option and passes it recursively to subordinate invocations of
       *   [write]. The meaning is that the normprefixes enumerated by this list
       *   have already been declared by surrounding elements. The option
       *   defaults to [] forcing the method to output all necessary prefix
       *   declarations.
       *
       *   Option [~default]: Specifies the normprefix that becomes the
       *   default namespace in the output.
       *
       *   KNOWN BUG: comment nodes are not printed.
       * <DOMAIN> All regular node types (elements, data nodes, comments,
       *   processing instructions, super root nodes).
       * </ID>
       *)

    (* ---------------------------------------- *)
    (* internal methods: *)
    method internal_adopt : 'ext node option -> int -> unit
    method internal_set_pos : int -> unit
    method internal_delete : 'ext node -> unit
    method internal_init : (string * int * int) ->
                           Pxp_core_types.pool option ->
			   bool -> 
                           dtd -> string -> (string * string) list -> 
			   (string * Pxp_core_types.att_value) list -> unit
    method internal_init_other : (string * int * int) ->
                                 dtd -> node_type -> unit

    method set_namespace_info : 'ext namespace_info option -> unit
      (* Sets the namespace_info field.
       * Only the implementation namespace_element_impl supports this
       * method.
       *)

    method dump : Format.formatter -> unit

  end

and ['ext] namespace_info =
  (* IMPORTANT: namespace_info is very very very very experimental. It is
   * very likely that the signature will change in the future, or that
   * the class will be removed.
   *)
  object
    method srcprefix : string
      (* Returns the prefix before it is normalized *)

    method declaration : 'ext node list
      (* Returns the currently active namespace declaration. The list
       * enumerates all namespace objects with
       *   namespace # node_type = T_namespace "srcprefix"
       * meaning that the srcprefix is declared to correspond to the
       * namespace URI
       *   namespace # data.
       * This list always declares the prefix "xml". If there is a default
       * namespace, it is declared for the prefix "".
       *)
  end
;;


class [ 'ext ] data_impl : 'ext -> [ 'ext ] node
  (* <ID:class-data-impl>
   * <TYPE:class>
   * <CALL>   'ext [data_impl]
   * <SIG>    AUTO
   * <DESCR>  This class is an implementation of [node] which
   *   realizes data nodes. You can create a new object by
   *
   *   [let exemplar = new data_impl ext_obj]
   *
   *   which creates a special form of empty data node which already contains a
   *   reference to the [ext_obj], but is otherwise empty. This special form
   *   is called a data exemplar. In order to get a working data node
   *   that can be used in a node tree it is required to apply the method
   *   [create_data] on the exemplar object.
   * </ID>
   *)


class [ 'ext ] element_impl : 'ext -> [ 'ext ] node
  (* <ID:class-element-impl>
   * <TYPE:class>
   * <CALL>   'ext [element_impl]
   * <SIG>    AUTO
   * <DESCR>  This class is an implementation of [node] which
   *   realizes element nodes. You can create a new object by
   *
   *   [let exemplar = new element_impl ext_obj]
   *
   *   which creates a special form of empty element which already contains a
   *   reference to the [ext_obj], but is otherwise empty. This special form
   *   is called an element exemplar. In order to get a working element
   *   that can be used in a node tree it is required to apply the method
   *   [create_element] on the exemplar object.
   *
   *   Note that the class [element_impl] is not namespace-aware.
   * </ID>
   *)


class [ 'ext ] comment_impl : 'ext -> [ 'ext ] node ;;
  (* <ID:class-comment-impl>
   * <TYPE:class>
   * <CALL>   'ext [comment_impl]
   * <SIG>    AUTO
   * <DESCR>  This class is an implementation of [node] which
   *   realizes comment nodes. You can create a new object by
   *
   *   [let exemplar = new comment_impl ext_obj]
   *
   *   which creates a special form of empty element which already contains a
   *   reference to the [ext_obj], but is otherwise empty. This special form
   *   is called an comment exemplar. In order to get a working element
   *   that can be used in a node tree it is required to apply the method
   *   [create_other] on the exemplar object, e.g.
   *
   *   [let comment = exemplar # create_other dtd]
   * </ID>
   *)

class [ 'ext ] super_root_impl : 'ext -> [ 'ext ] node ;;
  (* <ID:class-super-root-impl>
   * <TYPE:class>
   * <CALL>   'ext [super_root_impl]
   * <SIG>    AUTO
   * <DESCR>  This class is an implementation of [node] which
   *   realizes super root nodes. You can create a new object by
   *
   *   [let exemplar = new super_root_impl ext_obj]
   *
   *   which creates a special form of empty super root which already contains a
   *   reference to the [ext_obj], but is otherwise empty. This special form
   *   is called a super root exemplar. In order to get a working node
   *   that can be used in a node tree it is required to apply the method
   *   [create_other] on the exemplar object, e.g.
   *
   *   [let root = exemplar # create_other dtd]
   * </ID>
   *)

class [ 'ext ] pinstr_impl : 'ext -> [ 'ext ] node ;;
  (* <ID:class-pinstr-impl>
   * <TYPE:class>
   * <CALL>   'ext [pinstr_impl]
   * <SIG>    AUTO
   * <DESCR>  This class is an implementation of [node] which
   *   realizes processing instruction nodes. You can create a new object by
   *
   *   [let exemplar = new pinstr_impl ext_obj]
   *
   *   which creates a special form of empty node which already contains a
   *   reference to the [ext_obj], but is otherwise empty. This special form
   *   is called a processing instruction exemplar. In order to get a working node
   *   that can be used in a node tree it is required to apply the method
   *   [create_other] on the exemplar object, e.g.
   *
   *   [let pi = exemplar # create_other dtd]
   * </ID>
   *)

val pinstr : 'ext node -> proc_instruction
  (* <ID:val-pinstr>
   * <TYPE:fun>
   * <CALL>   [pinstr] n
   * <SIG>    AUTO
   * <DESCR>  Returns the processing instruction contained in a
   *   processing instruction node.
   *   This function raises [Invalid_argument] if invoked for a different node
   *   type than T_pinstr.
   * </ID>
   *)

class [ 'ext ] attribute_impl :
  element:string -> 
  name:string -> 
  Pxp_core_types.att_value -> 
  dtd -> 
    [ 'ext ] node
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

val attribute_name  : 'ext node -> string
  (* <ID:val-attribute-name>
   * <TYPE:fun>
   * <CALL>   [attribute_name] n
   * <SIG>    AUTO
   * <DESCR>  Returns the name of the attribute contained in an attribute
   *    node. Raises [Invalid_argument] if [n] does not have node type
   *    [T_attribute].
   * </ID>
   *)

val attribute_value : 'ext node -> Pxp_core_types.att_value
  (* <ID:val-attribute-value>
   * <TYPE:fun>
   * <CALL>   [attribute_value] n
   * <SIG>    AUTO
   * <DESCR>  Returns the value of the attribute contained in an attribute
   *    node. Raises [Invalid_argument] if [n] does not have node type
   *    [T_attribute].
   * </ID>
   *)

val attribute_string_value : 'ext node -> string
  (* <ID:val-attribute-string-value>
   * <TYPE:fun>
   * <CALL>   [attribute_string_value] n
   * <SIG>    AUTO
   * <DESCR>  Returns the string value of the attribute contained in an attribute
   *    node. Raises [Invalid_argument] if [n] does not have node type
   *    [T_attribute].
   * </ID>
   *)

(* Very experimental namespace support: *)

class [ 'ext ] namespace_element_impl : 'ext -> [ 'ext ] node
  (* <ID:class-namespace-element-impl>
   * <TYPE:class>
   * <CALL>   'ext [namespace_element_impl]
   * <SIG>    AUTO
   * <DESCR>  This class is an implementation of [node] which
   *   realizes element nodes. In contrast to [element_impl], this class
   *   also implements the namespace methods.
   *   You can create a new object by
   *
   *   [let exemplar = new namespace_element_impl ext_obj]
   *
   *   which creates a special form of empty element which already contains a
   *   reference to the [ext_obj], but is otherwise empty. This special form
   *   is called an element exemplar. In order to get a working element
   *   that can be used in a node tree it is required to apply the method
   *   [create_element] on the exemplar object.
   * </ID>
   *)

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
   *)


class [ 'ext ] namespace_attribute_impl :
  element:string -> 
  name:string -> 
  Pxp_core_types.att_value -> 
  dtd -> 
    [ 'ext ] node
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

val namespace_normprefix : 'ext node -> string
val namespace_srcprefix : 'ext node -> string
val namespace_uri : 'ext node -> string
  (* These functions return the normprefix, the srcprefix, and the URI
   * stored in a namespace object. 
   * If invoked for a different node type, the functions raise Invalid_argument.
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
  (* <ID:type-spec>
   * <TYPE:type>
   * <CALL>   'ext [spec]
   * <SIG>    AUTO
   * <DESCR>  The abstract data type specifying which objects are actually
   *    created by the parser.
   * </ID>
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
  (* <ID:val-make-spec-from-mapping>
   * <TYPE:fun>
   * <CALL>   [make_spec_from_mapping]
   *            ~super_root_exemplar ~comment_exemplar ~default_pinstr_exemplar
   *            ~pinstr_mapping ~data_exemplar ~default_element_exemplar
   *            ~element_mapping
   *            ()
   * <SIG>    AUTO
   * <DESCR>  Creates a [spec] from the arguments. Some arguments are optional,
   *    some arguments are mandatory.
   *      - [~super_root_exemplar]: Specifies the exemplar to be used for
   *        new super root nodes. This exemplar is optional.
   *      - [~comment_exemplar]: Specifies the exemplar to be used for
   *        new comment nodes. This exemplar is optional.
   *      - [~pinstr_exemplar]: Specifies the exemplar to be used for
   *        new processing instruction nodes by a hashtable mapping target
   *        names to exemplars. This hashtable is optional.
   *      - [~default_pinstr_exemplar]: Specifies the exemplar to be used for
   *        new processing instruction nodes. This exemplar will be used
   *        for targets that are not contained in the [~pinstr_exemplar]
   *        hashtable. This exemplar is optional.
   *      - [~data_exemplar]: Specifies the exemplar to be used for
   *        new data nodes. This exemplar is mandatory.
   *      - [~element_mapping]: Specifies the exemplar to be used for
   *        new element nodes by a hashtable mapping element types to
   *        exemplars. This hashtable is mandatory (but may be empty).
   *      - [~default_element_exemplar]: Specifies the exemplar to be used for
   *        new element nodes. This exemplar will be used
   *        for element types that are not contained in the [~element_mapping]
   *        hashtable. This exemplar is mandatory.
   *        --
   * </ID>
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
  (* <ID:val-make-spec-from-alist>
   * <TYPE:fun>
   * <CALL>   [make_spec_from_alist]
   *            ~super_root_exemplar ~comment_exemplar ~default_pinstr_exemplar
   *            ~pinstr_alist ~data_exemplar ~default_element_exemplar
   *            ~element_alist
   *            ()
   * <SIG>    AUTO
   * <DESCR>  Creates a [spec] from the arguments. This is a convenience
   *    function for [make_spec_from_mapping]; instead of requiring hashtables
   *    the function allows it to pass associative lists.
   * </ID>
   *)

val create_data_node :
      'ext spec -> dtd -> string -> 'ext node
  (* <ID:val-create-data-node>
   * <TYPE:fun>
   * <CALL>   [create_data_node] spec dtd datastring
   * <SIG>    AUTO
   * <DESCR>  Creates a new data node from the exemplar contained in [spec].
   *    The new node contains [datastring] and is connected with the [dtd].
   * </ID>
   *)

val create_element_node :
      ?name_pool_for_attribute_values:Pxp_core_types.pool ->
      ?position:(string * int * int) ->
      ?valcheck:bool ->
      ?att_values:((string * Pxp_core_types.att_value) list) ->
      'ext spec -> dtd -> string -> (string * string) list -> 'ext node
  (* <ID:val-create-element-node>
   * <CALL>    [create_element_node] ~name_pool_for_attribute_values
   *              ~position ~valcheck ~att_values spec dtd eltype
   *              att_list
   * <SIG>     AUTO
   * <DESCR>   Creates a new element node from the exemplar(s) contained in
   *    [spec]:
   *      - The new node will be connected to the passed [dtd].
   *      - The new node will have the element type [eltype].
   *      - The attributes of the new node will be the concatenation of
   *        [att_list] and [att_values]; [att_list] passes attribute values
   *        as strings while [att_values] passes attribute values as
   *        type [att_value]
   *      - The source position is set to [~position] (if passed)
   *      - The [~name_pool_for_attribute_values] will be used, if passed.
   *      - If [~valcheck = true] (the default), the attribute list is 
   *        immediately validated. If [~valcheck = false], the validation
   *        is left out; in this case you can pass any element type and
   *        and any attributes, and it does not matter whether and how
   *        they are declared.
   *        --
   * </ID>
   *)

val create_super_root_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> 'ext node
  (* <ID:val-create-super-root-node>
   * <CALL>   [create_super_root_node] ~position spec dtd
   * <SIG>    AUTO
   * <DESCR>  Creates a new super root node from the exemplar contained in
   *    [spec]. The new node is connected to [dtd], and the position
   *    triple is set to [~position].
   *
   *    The function fails if there is no super root exemplar in [spec].
   * </ID>
   *)

val create_comment_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> string -> 'ext node
  (* <ID:val-create-comment-node>
   * <CALL>   [create_comment_node] ~position spec dtd commentstring
   * <SIG>    AUTO
   * <DESCR>  Creates a new comment node from the exemplar contained in
   *    [spec]. The new node is connected to [dtd], and the position
   *    triple is set to [~position]. The contents of the node are set
   *    to [commentstring].
   *
   *    The function fails if there is no comment exemplar in [spec].
   * </ID>
   *)


val create_pinstr_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> proc_instruction -> 'ext node
  (* <ID:val-create-pinstr-node>
   * <CALL>   [create_pinstr_node] ~position spec dtd pi
   * <SIG>    AUTO
   * <DESCR>  Creates a new processing instruction node from the exemplar 
   *    contained in [spec]. The new node is connected to [dtd], and the 
   *    position triple is set to [~position]. The contents of the node are set
   *    to [pi].
   *
   *    The function fails if there is no processing instruction exemplar in
   *    [spec].
   * </ID>
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
  (* <ID:val-compare>
   * <TYPE:fun>
   * <CALL>   [compare] n1 n2
   * <SIG>    AUTO
   * <DESCR>  Returns -1 if [n1] occurs before [n2], or +1 if [n1] occurs
   * after [n2], or 0 if both nodes are identical.
   * If the nodes are unrelated (do not have a common ancestor), the result
   * is undefined (Note: this case is different from [ord_compare]).
   * This test is rather slow, but it works even if the XML tree changes
   * dynamically (in contrast to [ord_compare] below).
   * </ID>
   *)

type 'ext ord_index
constraint 'ext = 'ext node #extension
  (* <ID:type-ord-index>
   * <TYPE:type>
   * <CALL>   'ext [ord_index]
   * <SIG>    AUTO
   * <DESCR>  The type of ordinal indexes.
   * </ID>
   *)

val create_ord_index : 'ext node -> 'ext ord_index
  (* <ID:val-create-ord-index>
   * <TYPE:fun>
   * <CALL>   [create_ord_index] startnode
   * <SIG>    AUTO
   * <DESCR>   
   * Creates an ordinal index for the subtree starting at [startnode].
   * This index assigns to every node an ordinal number (beginning with 0) such
   * that nodes are numbered upon the order of the first character in the XML
   * representation (document order).
   * Note that the index is not automatically updated when the tree is
   * modified.
   * </ID>
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
  (* <ID:val-ord-compare>
   * <TYPE:fun>
   * <CALL>   [ord_compare] idx n1 n2
   * <SIG>    AUTO
   * <DESCR>
   * Compares two nodes like [compare]:
   * Returns -1 if [n1] occurs before [n2], or +1 if [n1] occurs
   * after [n2], or 0 if both nodes are identical.
   * If one of the nodes does not occur in the ordinal index, [Not_found]
   * is raised. (Note that this is a different behaviour than what [compare]
   * would do.)
   *
   * This test is much faster than [compare].
   * </ID>
   *)


(***************************** Iterators ********************************)

(* General note: The iterators ignore attribute and namespace nodes *)

val find : ?deeply:bool ->
           ('ext node -> bool) -> 'ext node -> 'ext node
  (* <ID:val-find>
   * <TYPE:fun>
   * <CALL>    [find] ~deeply f startnode
   * <SIG>     AUTO
   * <DESCR>   Searches the first node in the tree below [startnode] for which 
   *     the predicate f is true, and returns it. Raises [Not_found]
   *     if there is no such node.
   *
   *     By default, [~deeply=false]. In this case, only the children of
   *     [startnode] are searched.
   *
   *     If passing [~deeply=true], the children are searched recursively
   *     (depth-first search). Note that even in this case [startnode] itself
   *     is not checked.
   *
   *     Attribute and namespace nodes are ignored.
   * </ID>
   *)

val find_all : ?deeply:bool ->
               ('ext node -> bool) -> 'ext node -> 'ext node list
  (* <ID:val-find-all>
   * <CALL>    [find_all] ~deeply f startnode
   * <SIG>     AUTO
   * <DESCR>   Searches all nodes in the tree below [startnode] for which 
   *     the predicate f is true, and returns them. 
   *
   *     By default, [~deeply=false]. In this case, only the children of
   *     [startnode] are searched.
   *
   *     If passing [~deeply=true], the children are searched recursively
   *     (depth-first search). Note that even in this case [startnode] itself
   *     is not checked.
   *
   *     Attribute and namespace nodes are ignored.
   * </ID>
   *)

val find_element : ?deeply:bool ->
                   string -> 'ext node -> 'ext node
  (* <ID:val-find-element>
   * <TYPE:fun>
   * <CALL>    [find_element] ~deeply eltype startnode
   * <SIG>     AUTO
   * <DESCR>   Searches the first element in the tree below [startnode] 
   *     that has the element type [eltype], and returns it. Raises [Not_found]
   *     if there is no such node.
   *
   *     By default, [~deeply=false]. In this case, only the children of
   *     [startnode] are searched.
   *
   *     If passing [~deeply=true], the children are searched recursively
   *     (depth-first search). Note that even in this case [startnode] itself
   *     is not checked.
   * </ID>
   *)

val find_all_elements : ?deeply:bool ->
                        string -> 'ext node -> 'ext node list
  (* <ID:val-find-all-elements>
   * <TYPE:fun>
   * <CALL>    [find_all_elements] ~deeply eltype startnode
   * <SIG>     AUTO
   * <DESCR>   Searches all elements in the tree below [startnode] 
   *     having the element type [eltype], and returns them.
   *
   *     By default, [~deeply=false]. In this case, only the children of
   *     [startnode] are searched.
   *
   *     If passing [~deeply=true], the children are searched recursively
   *     (depth-first search). Note that even in this case [startnode] itself
   *     is not checked.
   * </ID>
   *)

exception Skip
  (* <ID:exc-skip>
   * <TYPE:exception>
   * <CALL>   [Skip]
   * <SIG>    AUTO
   * <DESCR>  This exception can be used in the functions passed to
   *    [map_tree], [map_tree_sibl], [iter_tree], and [iter_tree_sibl]
   *    to skip the current node, and to proceed with the next node.
   *    See these function for details.
   * </ID>
   *)

val map_tree :  pre:('exta node -> 'extb node) ->
               ?post:('extb node -> 'extb node) ->
               'exta node ->
                   'extb node
  (* <ID:val-map-tree>
   * <TYPE:fun>
   * <CALL>   [map_tree] ~pre ~post startnode
   * <SIG>    AUTO
   * <DESCR>  Maps the tree beginning at [startnode] to a second tree
   *    using the following algorithm.
   *
   *    [startnode] and the whole tree below it are recursively traversed.
   *    After entering a node, the function ~pre is called. The result of
   *    this function must be a new node; it must not have children nor a
   *    parent. For example, you can pass
   *      [~pre:(fun n -> n # orphaned_flat_clone)] 
   *    to copy the original node. After that, the children are processed
   *    in the same way (from left to right) resulting in a list of
   *    mapped children. These are added to the mapped node as its 
   *    children.
   * 
   *    Now, the ~post function is invoked with the mapped node as argument, and
   *    the result is the result of the function (~post should return a root
   *    node, too; if not specified, the identity is the ~post function).
   *
   *    Both ~pre and ~post may raise [Skip] which causes that the node is
   *    left out (i.e. the mapped tree does neither contain the node nor
   *    any children of the node). 
   *    If the top node is skipped, the exception [Not_found] is
   *    raised.
   *
   *    For example, the following piece of code duplicates a tree, but
   *    removes all comment nodes:
   *
   *    [ map_tree ~pre:(fun n -> if n # node_type = T_comment then raise Skip else n # orphaned_flat_clone) startnode ]
   *
   *     Attribute and namespace nodes are ignored.
   * </ID>
   *)

val map_tree_sibl :
        pre: ('exta node option -> 'exta node -> 'exta node option ->
                  'extb node) ->
       ?post:('extb node option -> 'extb node -> 'extb node option ->
                  'extb node) ->
       'exta node ->
           'extb node
  (* <ID:val-map-tree-sibl>
   * <TYPE:fun>
   * <CALL>   [map_tree_sibl] ~pre ~post startnode
   * <SIG>    AUTO
   * <DESCR>  Maps the tree beginning at [startnode] to a second tree
   *    using the following algorithm.
   *
   *    [startnode] and the whole tree below it are recursively traversed.
   *    After entering a node, the function ~pre is called with three
   *    arguments: some previous node, the current node, and some next node.
   *    The previous and the next node may not exist because the current
   *    node is the first or the last in the current list of nodes.
   *    In this case, [None] is passed as previous or next node, resp.
   *    The result of this function invocation must be a new node; 
   *    it must not have children nor a parent. For example, you can pass
   *      [~pre:(fun prev n next -> n # orphaned_flat_clone)] 
   *    to copy the original node. After that, the children are processed
   *    in the same way (from left to right) resulting in a list of
   *    mapped children. 
   * 
   *    Now, the ~post function is applied to the list of mapped children
   *    resulting in a list of postprocessed children. (Note: this part
   *    works rather differently than [map_tree].) ~post has three arguments:
   *    some previous child, the current child, and some next child.
   *    The previous and the next child are [None] if non-existing.
   *    The postprocessed children are appended to the mapped node resulting
   *    in the mapped tree.
   *
   *    Both ~pre and ~post may raise [Skip] which causes that the node is
   *    left out (i.e. the mapped tree does neither contain the node nor
   *    any children of the node). 
   *    If the top node is skipped, the exception [Not_found] is
   *    raised.
   *
   *     Attribute and namespace nodes are ignored.
   * </ID>
   *)

val iter_tree : ?pre:('ext node -> unit) ->
                ?post:('ext node -> unit) ->
                'ext node ->
                    unit
  (* <ID:val-iter-tree>
   * <TYPE:fun>
   * <CALL>   [iter_tree] ~pre ~post startnode
   * <SIG>    AUTO
   * <DESCR>  Iterates over the tree beginning at [startnode] 
   *    using the following algorithm.
   *
   *    [startnode] and the whole tree below it are recursively traversed.
   *    After entering a node, the function ~pre is called. Now, the children
   *    are processed recursively. Finally, the ~post function is invoked.
   *
   *    The ~pre function may raise [Skip] causing that the children
   *    and the invocation of the ~post function are skipped.
   *    If the ~post function raises [Skip] nothing special happens.
   *
   *     Attribute and namespace nodes are ignored.
   * </ID>
   *)


val iter_tree_sibl :
       ?pre: ('ext node option -> 'ext node -> 'ext node option -> unit) ->
       ?post:('ext node option -> 'ext node -> 'ext node option -> unit) ->
       'ext node ->
           unit
  (* <ID:val-iter-tree-sibl>
   * <TYPE:fun>
   * <CALL>   [iter_tree_sibl] ~pre ~post startnode
   * <SIG>    AUTO
   * <DESCR>  Iterates over the tree beginning at [startnode] 
   *    using the following algorithm.
   *
   *    [startnode] and the whole tree below it are recursively traversed.
   *    After entering a node, the function ~pre is called with three
   *    arguments: some previous node, the current node, and some next node.
   *    The previous and the next node may be [None] if non-existing.
   *    Now, the children are processed recursively. 
   *    Finally, the ~post function is invoked with the same three
   *    arguments.
   *
   *    The ~pre function may raise [Skip] causing that the children
   *    and the invocation of the ~post function are skipped.
   *    If the ~post function raises [Skip] nothing special happens.
   *
   *     Attribute and namespace nodes are ignored.
   * </ID>
   *)


(************************ Whitespace handling ***************************)

type stripping_mode =
  [ `Strip_one_lf
  | `Strip_one
  | `Strip_seq
  | `Disabled
  ]
  (* <ID:type-stripping-mode>
   * <TYPE:type>
   * <CALL>   [stripping_mode]
   * <SIG>    AUTO
   * <DESCR>  The different ways how to strip whitespace from a single
   *    data node:
   * - [`Strip_one_lf]: If there is a linefeed character at the beginning/at
   *   the end, it will be removed. If there are more linefeed characters, 
   *   only the first/the last is removed.
   *   (This is the SGML rule to strip whitespace.)
   * - [`Strip_one]: If there is a whitespace character at the beginning/at
   *   the end, it will be removed. If there are more whitespace characters, 
   *   only the  first/the last is removed. Whitespace characters are space, 
   *   newline, carriage return, tab.
   * - [`Strip_seq]: All whitespace characters at the beginning/at the end are
   *   removed.
   * - [`Disabled]: Do not strip whitespace.
   *   --
   * </ID>
   *)


val strip_whitespace : 
      ?force:bool -> ?left:stripping_mode -> ?right:stripping_mode ->
      ?delete_empty_nodes:bool ->
      'ext node ->
      unit
  (* <ID:val-strip-whitespace>
   * <TYPE:fun>
   * <CALL>   [strip_whitespace] ~force ~left ~right ~delete_empty_nodes
   *             startnode
   * <SIG>    AUTO
   * <DESCR>
   * Modifies the passed tree in-place by the following rules:
   * - In general, whitespace stripping is not applied to nodes inside
   *   an [xml:space="preserve"] region, unless [~force:true] is passed
   *   to the function (default is [~force:false]). Only if whitespace
   *   stripping is allowed, the following rules are carried out.
   *   Note that the detection of regions with preserved whitespace takes
   *   the parent nodes of the passed [startnode] into account.
   * - If applied to a data node, whitespace at the beginning of the node
   *   is removed according to [~left], and whitespace at the end of the node
   *   is removed according to [~right].
   * - If applied to an element, whitespace at the beginning of the first
   *   data subnode is removed according to [~left], and whitespace at the end
   *   of the last data subnode is removed according to [~right]. Furthermore,
   *   these rules are recursively applied to all subelements (but not to
   *   other node types).
   * - If applied to the super root node, this node is treated as if it
   *   were an element.
   * - Whitespace of other node types is left as-is, as whitespace occuring
   *   in attributes.
   * - Option [~delete_empty_nodes] (default true):
   *   If data nodes become empty after removal of whitespace, they are
   *   deleted from the XML tree. 
   *   --
   * 
   * Defaults:
   *   - [~force:false]
   *   - [~left:`Disabled]
   *   - [~right:`Disabled]
   * </ID>
   *)


(****************************** normalization ****************************)

val normalize : 'ext node -> unit
  (* <ID:val-normalize>
   * <TYPE:fun>
   * <CALL>   [normalize] startnode
   * <SIG>    AUTO
   * <DESCR>  Normalizes the tree denoted by [startnode]  such that
   *    neither empty data nodes nor adjacent data nodes exist. Normalization
   *    works in-place.
   * </ID>
   *)

(******************************** validation *****************************)

val validate : 'ext node -> unit
  (* <ID:val-validate>
   * <TYPE:fun>
   * <CALL>   [validate] startnode
   * <SIG>    AUTO
   * <DESCR>  Validates the tree denoted by [startnode]. In contrast to
   *   [startnode # validate()] this function validates recursively.
   * </ID>
   *)

(******************************* document ********************************)

class [ 'ext ] document :
  ?swarner:Pxp_core_types.symbolic_warnings ->
  Pxp_core_types.collect_warnings -> Pxp_core_types.rep_encoding ->
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

    method init_root : 'ext node -> string -> unit
	(* Set the root element. It is expected that the root element has
	 * a DTD.
	 * The second argument is the original name of the root element
	 * (without namespace prefix processing).
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

    method encoding : Pxp_core_types.rep_encoding
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

    method write : ?default : string ->
                   ?prefer_dtd_reference : bool ->
                   Pxp_core_types.output_stream -> 
                   Pxp_core_types.encoding -> 
                     unit
      (* Write the document to the passed
       * output stream; the passed encoding used. The format
       * is compact (the opposite of "pretty printing").
       * If a DTD is present, the DTD is included into the internal subset.
       *
       * Option [~default]: Specifies the normprefix that becomes the
       * default namespace in the output.
       *
       * Option [~prefer_dtd_reference]: If true, it is tried to print
       * the DTD as reference, i.e. with SYSTEM or PUBLIC identifier.
       * This works only if the DTD has an [External] identifier. If
       * the DTD cannot printed as reference, it is included as text.
       * The default is not to try DTD references, i.e. to always include
       * the DTD as text.
       *)

    method dump : Format.formatter -> unit

  end
;;


(* Printers for toploop: *)

val print_node :
    'ext node -> unit ;;

val print_doc :
    'ext document -> unit ;;


(**********************************************************************)
(* Experimental: event streams and node trees                         *)
(**********************************************************************)

exception Empty_tree
  (* Nothing to return *)

exception Build_aborted
  (* The event stream contains an E_error event *)

val build_node_tree : 
    config -> dtd -> 'ext spec -> (unit -> event option) -> 'ext node

  (* Reads the event stream by calling the unit->event function, and
   * creates a node tree according to config, dtd, spec.
   *
   * The event stream may be either:
   * - A document event stream (as generated by `Entry_document)
   * - A content event stream (as generated by `Entry_content)
   *
   * The returned toplevel node is either the "super root node" (if
   * configured), or the topmost element. In the latter case, comments
   * and processing instructions at the top level are ignored, and it
   * may even happen that the function raises Empty_tree because there
   * is no topmost element.
   *
   * If the DTD allows validation, the returned tree is validated.
   *
   * The data nodes are not normalized unless the arriving data events
   * are already normalized.
   *
   * The uniqueness of ID attributes is not checked.
   *)

(* TODO:
 * build_document
 * decompose_node_tree
 * decompose_document
 *)


(* ======================================================================
 * History:
 *
 * $Log: pxp_document.mli,v $
 * Revision 1.26  2003/10/03 20:59:08  gerd
 * 	Added build_node_tree
 *
 * Revision 1.25  2003/06/20 15:14:13  gerd
 * 	Introducing symbolic warnings, expressed as polymorphic
 * variants
 *
 * Revision 1.24  2003/06/15 12:23:21  gerd
 * 	Moving core type definitions to Pxp_core_types
 *
 * Revision 1.23  2001/12/03 23:46:29  gerd
 * 	New option ~prefer_dtd_reference for [write].
 *
 * Revision 1.22  2001/06/30 00:05:12  gerd
 * 	Fix: When checking the type of the root element, namespace
 * rewritings are taken into account.
 *
 * Revision 1.21  2001/06/28 22:42:07  gerd
 * 	Fixed minor problems:
 * 	- Comments must be contained in one entity
 * 	- Pxp_document.document is now initialized with encoding.
 *           the DTD encoding may be initialized too late.
 *
 * Revision 1.20  2001/06/27 23:35:43  gerd
 * 	Minor fixes: create_other, write.
 *
 * Revision 1.19  2001/06/25 21:04:18  gerd
 * 	Updated documentation. Most docs are now structured comments
 * that can be extracted and included into the docbook manual.
 * 	New option ~default for method [write].
 * 	New method create_other.
 *
 * Revision 1.18  2001/06/09 22:33:14  gerd
 * 	Added 'dump' methods to 'node' and 'document'. Also print_node,
 * print_doc.
 * 	Fixed namespace_info.
 *
 * Revision 1.17  2001/06/08 01:15:46  gerd
 * 	Moved namespace_manager from Pxp_document to Pxp_dtd. This
 * makes it possible that the DTD can recognize the processing instructions
 * <?pxp:dtd namespace prefix="..." uri="..."?>, and add the namespace
 * declaration to the manager.
 *
 * Revision 1.16  2001/06/08 00:12:40  gerd
 * 	Numerous changes:
 * 	- Method add_node has been deprecated in favor of
 * classify_data_node and append_node
 * 	- keep_always_whitespace_mode has been dropped in favor
 * of the new Pxp_yacc.config option drop_ignorable_whitespace
 * 	- create_element_node: accepts the arguments ~att_values
 * and ~valcheck. The first of them contains the already preprocessed
 * and normalized attribute values as att_value (and not as string).
 * The latter may be used to switch off the validation of attribute
 * lists.
 * 	- validate_contents, validate_attlist, validate: these
 * are now the core validation methods
 * 	New methods:
 * 	- complement_attlist
 * 	- improved namespace_manager
 * 	- delete_nodes, insert_nodes
 * 	- set_data
 * 	- set_attributes now official
 * 	New functions:
 * 	- strip_whitespace
 * 	- normalize
 * 	- validate
 *
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
