(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(** Tree representation of XML documents *)

(** {1 The structure of document trees} *)

(**
  Please have a look at the separate text {!Intro_trees}.
 *)


(** {1 Interface} *)

open Pxp_types
open Pxp_dtd


(** {2 The node type} *)

type node_type =
    T_element of string (** An element node with this element type *)
  | T_data              (** A data node *)
  | T_super_root        (** The super root node *)
  | T_pinstr of string  (** A processing instruction with this target *)
  | T_comment           (** A comment *)
  | T_none              (** Sometimes used if the nodes are non-standard *)
  | T_attribute of string  (** An attribute node for this attribute name *)
  | T_namespace of string  (** A namespace node for this normalized prefix *)
  (** This type enumerates the possible node types:
   *   - [T_element name]: The node is an element and has element type [name]
   *   - [T_data]: The node is a data node
   *   - [T_super_root]: The node is a super root node
   *   - [T_pinstr name]: The node contains a processing instruction with
   *     target [name]
   *   - [T_comment]: The node is a comment
   *   - [T_attribute name]: The node contains an attribute called [name]
   *   - [T_namespace prefix]: The node identifies a namespace for the
   *     normalized [prefix]
   *   - [T_none]: This is a "bottom value" used if there is no reasonable
   *     type.
   *
   * Note that attribute and namespace nodes can only live outside the
   * regular tree, and are only returned by special methods.
   *)

(* The result type of the method classify_data_node: *)
type data_node_classification =
    CD_normal
  | CD_other
  | CD_empty
  | CD_ignorable
  | CD_error of exn (**)
  (** This type enumerates the result values of the method
   *   [classify_data_node]:
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
   *)


(** The [extension] is, as the name says, the extensible part of the
    nodes. See {!Intro_extensions} for an introduction into extensions.
 *)
class type [ 'node ] extension =
  object ('self)
    method clone : 'self
      (** [clone] is called when the node is to be duplicated, and as a
          followup action, also the extension must be cloned. This method
          must return a deep copy of the extension. By convention, the
          caller of this method must also invoke [set_node] on the copied
          extension to establish a new link to a main node.
       *)

    method node : 'node
      (** [node] is the link from the extension to the main node.
          Conventionally, this link is set by the node via [set_node]
          below after the extension has been created or cloned.
       *)

    method set_node : 'node -> unit
      (** [set_node n] sets the link to the main node to [n]. The link is
          returned by [node] after that.
       *)
  end
;;


(** The class type [node] defines the interface of the nodes that are part
    of XML document trees. For an introduction into trees, see
    {!Intro_trees}.

    The interface of the nodes as such cannot be extended by the user.
    There is, however, the possibility of defining a so-called extension
    which is reflected in the type parameter ['ext], and which can be
    any class type that is a subtype of {!Pxp_document.extension}.
    Note that also the extension has a type parameter pointing to the
    node class. Closed node types look thus like

    {[ type my_node = my_node extension node ]}

    which is a rare form of a recursive type.
 *)
class type [ 'ext ] node =
  object ('self)
    constraint 'ext = 'ext node #extension

  (** Domain

      Every node has a node type which is returned by the [node_type]
      method below. Depending on the node type, not all methods are
      defined. If a method is undefined for certain node types, this
      is documented below, and also any unusual reaction when the
      methods are called nevertheless. The standard rection is to raise
      either the exception
      {!Pxp_types.Method_not_applicable} or
      {!Pxp_types.Namespace_method_not_applicable} for namespace-specific
      methods.
   *)

    (** Validation

      Some methods modify the tree. This may violate the DTD. Because of this,
      it is documented for every mutating method which validation checks are
      performed.
     *)




    (** General Interface *)

    method extension : 'ext
      (** Returns the extension object of this node.
       *
       * {b Domain.} 
       *   Applicable to element, data, comment, processing instruction,
       *   and super root nodes.
       *)

    method node_type : node_type
      (**  Returns the type of the node:
       *   - [T_element t]: The node is an element with name [t]
       *   - [T_data]: The node is a data node
       *   - [T_comment]: The node is a comment node
       *   - [T_pinstr n]: The node is a processing instruction with
       *     target [n]
       *   - [T_super_root]: The node is a super root node
       *   - [T_attribute n]: The node is an attribute with name [n]
       *   - [T_namespace p]: The node is a namespace with normalized prefix [p]
       *
       * {b Domain.} All node types.
       *)

    method dtd : dtd
      (** Returns the DTD. Note that the DTD object is the same for all
       *  nodes of the same tree, and that the DTD object even exists when
       *  validation is turned off (well-formedness mode).
       *
       * {b Domein.} All node types. However, exemplars need not to have
       *   an associated DTD, in which case this method fails.
       *)

    method encoding : Pxp_core_types.rep_encoding
      (** Get the encoding which is always the same as the encoding of 
       *  the DTD. See also method [dtd]. (Note: This method fails, too, if
       *  no DTD is present.)
       *
       * {b Domain.} All node types. Note that exemplars need not to have
       *   an associated DTD, in which case this method fails.
       *)




    (** Navigation Interface *)

    method parent : 'ext node
      (** Get the parent node, or raise [Not_found] if this node is
       *  a root node. For attribute and namespace nodes, the parent is
       *  artificially defined as the element to which these nodes apply.
       * 
       * {b Domain.} All node types.
       *)

    method root : 'ext node
      (** Gets the root node of the tree.
       *  Every node is contained in a tree with a root, so this method always 
       *  succeeds. Note that this method searches for the root,
       *  which costs time proportional to the length of the path to the root.
       *
       * {b Domain.} All node types.
       *)

    method node_position : int
      (**  Returns the position of this node in the list of all children of
       *   the parent
       *   node. Positions are counted from 0 on. There are several cases:
       *    - The regular nodes get positions from [0] to [l-1] where [l] is the
       *      length of the list of regular children.
       *    - Attribute nodes and namespace nodes are irregular nodes, 
       *      which means here that their positions are counted seperately.
       *      All attribute nodes have positions from [0] to [m-1]; all namespace
       *      nodes have positions from [0] to [n-1].
       *    - If this node is a root, this method raises [Not_found].
       *
       * {b Domain.} All node types.
       *)

    method node_path : int list
      (** Returns the list of node positions describing
       *  the location of this node in the whole tree. The list describes 
       *  the path from the root node down to this node; the first path
       *  element is the index of the child of the root, the second path
       *  element is the index of the child of the child, and so on, and
       *  the last path element is the index of this node. The method returns 
       *  the empty list if this node is the root node.
       *
       *  Attribute and namespace nodes are not part of the regular tree, so 
       *  there is a special rule for them. Attribute nodes of an element 
       *  node [x] have the node path [x # node_path @ [-1; p]] where 
       *  [p] is the position of the attribute node. Namespace nodes of an 
       *  element node [x] have the node path [x # node_path @ [-2; p]] 
       *  where [p] is the position of the namespace node.
       *  Note that this definition respects the document order.
       *
       * {b Domain.} All node types.
       *)

    method sub_nodes : 'ext node list
      (**  Returns the regular children of the node as list. Only
       * elements, data nodes, comments, and processing instructions can
       * occur in this list; attributes and namespace nodes are not
       * considered as regular nodes, and super root nodes can only
       * be root nodes and will never be children of another node.
       * The returned list is always empty if this node is a data node,
       * comment, processing instruction, attribute, or namespace.
       *
       * {b Domain.} All node types.
       *)

    method iter_nodes : ('ext node -> unit) -> unit
      (** [obj#iter_nodes f] iterates over the regular children of [obj], and
       *  calls the function [f] for every child [ch] as [f ch]. The
       *  regular children are the nodes returned by [sub_nodes], see
       *  there for an explanation.
       *
       * See also {!Pxp_document.iterators} for more functions iterating over
       * trees.
       *
       * {b Domain.} All node types.
       *)

    method iter_nodes_sibl :
      ('ext node option -> 'ext node -> 'ext node option -> unit) -> unit
      (**  [obj#iter_nodes f] iterates over the regular children of [obj], and
       *  calls the function [f] for every child as [f pred ch succ]:
       *   - [ch] is the child
       *   - [pred] is [None] if the child is the first in the list,
       *     and [Some p] otherwise; [p] is the predecessor of [ch]
       *   - [succ] is [None] if the child is the last in the list,
       *     and [Some s] otherwise; [s] is the successor of [ch]
       *
       *   The
       *   regular children are the nodes returned by [sub_nodes], see
       *   there for an explanation.
       *
       * See also {!Pxp_document.iterators} for more functions iterating over
       * trees.
       *
       * {b Domain.} All node types.
       *)

    method nth_node : int -> 'ext node
      (** [nth_node n] returns the n-th regular child, [n >= 0].
       *  Raises [Not_found] if the index [n] is out of the valid range.
       *
       * {b Domain.} All node types.
       *)

    method previous_node : 'ext node
      (**  Returns the predecessor of this node
       *   in the list of regular children of the parent, or raise [Not_found]
       *   if this node is the first child. This is equivalent to
       *   [obj # parent # nth_node (obj # node_position - 1)] (when [obj]
       *   is this node).
       *
       * {b Domain.} All node types.
       *)

    method next_node : 'ext node
      (**  Returns the successor of this node
       *   in the list of regular children of the parent, or raise [Not_found]
       *   if this node is the last child. This is equivalent to
       *   [obj # parent # nth_node (obj # node_position + 1)] (when [obj]
       *   is this node).
       *
       * {b Domain.} All node types.
       *)



    (** Getting contents and attributes

        Also see the {!Pxp_document.node.node_type} method which returns
        the type of the node, and for elements their names.
     *)

    method data : string
      (**  This method returns what is considered as
       *   the data of the node which depends on the node type:
       *    - Data nodes: the method returns the character string the node 
       *      represents
       *    - Element nodes, super root nodes: the method returns the
       *      concatenated character strings of all (direct or indirect)
       *      data nodes below this node
       *    - Comment nodes: the method returns the
       *      comment string (without delimiters), or it raises [Not_found]
       *      if the comment string is not set (see also the [comment] method
       *      below for an alternate way of getting the comment string)
       *    - Processing instructions: the
       *      method returns the data part of the instruction, or [""] 
       *      if the data part is missing (see also the [pinstr] method
       *      below for an alternay way of accessing processing instructions)
       *    - Attribute nodes: the method returns the attribute
       *      value as string, or it raises [Not_found] if the attribute
       *      is implied.
       *    - Namespace nodes: the method returns the namespace
       *      URI
       *
       * {b Domain.} All node types.
       *)

    method attribute : string -> Pxp_core_types.att_value
      (** [attribute name] returns the value of the attribute [name].
       *
       * If the parser is in validating mode, the method returns
       * values for declared attributes, and it raises [Not_found] for any 
       * undeclared attribute. Note that it even returns a value if the
       * attribute is actually missing but is declared as [#IMPLIED] or 
       * has a default value. 
       *
       * If the parser (more precisely, the DTD object) is in 
       * well-formedness mode, the method returns only values for 
       * defined attributes that occur literally in the XML text, and it
       * raises [Not_found] for any
       * unknown attribute name.
       *
       * Possible return values are:
       * {ul
       *    {-  [Implied_value]: The attribute has been declared with the
       *        keyword [#IMPLIED], and the attribute definition is missing
       *        in the attribute list of the element.}
       *    {-  [Value s]: The attribute has been declared as type [CDATA], 
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
       *        object specifies well-formedness mode.}
       *    {-  [Valuelist l]: The attribute has been declared as type
       *        [IDREFS], as [ENTITIES], or [NMTOKENS], and one of the two
       *        conditions holds: (1) The attribute value is defined in the 
       *        attribute list in which case the space-separated tokens of
       *        the value are returned in the string list [l]. (2) The
       *        attribute has been omitted, and the DTD declares the attribute 
       *        with a default value. The default value is returned in [l]. 
       *
       *        Summarized, [Valuelist l] is returned for all list-type
       *        attribute values.}
       *  }
       *
       *   Note that before the attribute value is returned, the value is
       *   normalized. This means that newlines are converted to spaces, and
       *   that references to character entities (i.e. [&#n;]) and
       *   general entities (i.e. [&name;]) are expanded; if necessary, 
       *   the expansion is performed recursively.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       * will return values, all other node types always raise [Not_found].
       *)

    method attribute_names : string list
      (**  Returns the list of all attribute names of this element.
       *  In validating mode, this list is simply the list of declared
       *  attributes. In well-formedness mode, this list is the list of
       *  defined attributes that occur literally in the XML text.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return a non-empty list, all other node types always return
       *   the empty list.
       *)

    method attribute_type : string -> Pxp_core_types.att_type
      (** [attribute_type name]: returns the type of the attribute [name]. 
       *   If the attribute
       *   is declared, the declared type is returned. If the attribute is
       *   defined but undeclared, the type [A_cdata] will be returned.
       *   (The module [Pxp_types] contains the Caml type of attribute types.)
       *   This method raises [Not_found] if the attribute is unknown.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return values, all other node types always raise [Not_found].
       *)

    method attributes : (string * Pxp_core_types.att_value) list
      (** Returns the list of [(name,value)] pairs describing
       *  all attributes (declared attributes plus defined attributes).
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return non-empty values, all other node types always
       *   return the empty list.
       *)

    method required_string_attribute : string -> string
      (** [required_string_attribute name]:   
       *    Returns the value of the attribute [name] as string,
       *    i.e. if the value of the attribute is [Value s], this method
       *    will return simply [s], and if the value is [Valuelist l],
       *    this method will return the elements of [l] separated by
       *    spaces. If the attribute value is [Implied_value], the method
       *    will fail.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return values, all other node types always fail.
       *)

    method required_list_attribute : string -> string list
      (** [required_list_attribute name]:
       *    Returns the value of the attribute [name] as string list,
       *    i.e. if the value of the attribute is [Valuelist l], this method
       *    will return simply [l], and if the value is [Value s],
       *    this method will return the one-element list [[[s]]].
       *    If the attribute value is [Implied_value], the method
       *    will fail.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return values, all other node types always fail.
       *)

    method optional_string_attribute : string -> string option
      (** [optional_string_attribute name]:
       *  Returns the value of the attribute [name] as optional string,
       *    i.e. if the value of the attribute is [Value s], this method
       *    will return [Some s], and if the value is [Valuelist l],
       *    this method will return [Some s] where [s] consists of the
       *    concatenated elements of [l] separated by spaces. If the
       *    attribute value is [Implied_value], the method will return [None].
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return [Some] values, all other node types always return [None].
       *)

    method optional_list_attribute : string -> string list
      (**  [optional_list_attribute name]:
       *  Returns the value of the attribute [name] as string list,
       *    i.e. if the value of the attribute is [Valuelist l], this method
       *    will return simply [l], and if the value is [Value s],
       *    this method will return the one-element list [[[s]]].
       *    If the attribute value is [Implied_value], the method
       *    will return the empty list.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return non-empty values, all other node types always
       *   return the empty list.
       *)

    method id_attribute_name : string
      (** Returns the name of the (at most one) attribute being
       *    declared as type [ID]. The method raises [Not_found] if there 
       *    is no declared [ID] attribute for the element type.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return names, all other node types always raise [Not_found].
       *)

    method id_attribute_value : string
      (**  Returns the string value of the (at most one) attribute being
       *    declared as type [ID]. The method raises [Not_found] if there 
       *    is no declared [ID] attribute for the element type.
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return names, all other node types always raise [Not_found].
       *)

    method idref_attribute_names : string list
      (**  Returns the names of the attributes being
       *   declared as type [IDREF] or [IDREFS]. 
       *
       * {b Domain.} All node types. However, only elements and attribute nodes
       *   will return names, all other node types always return the empty
       *   list.
       *)

    method attributes_as_nodes : 'ext node list
      (** Returns all attributes (i.e. declared plus defined
       *  attributes) as a list of attribute nodes with node type 
       *  [T_attribute name]. 
       *
       *   This method can be used if it is required for typing reasons
       *   that the attributes have also type [node]. A common example
       *   are sets that may both contain elements and attributes, as they
       *   are used in the XPath language.
       *
       *   The attribute nodes are read-only; any call to a method
       *   modifying their contents will raise [Method_not_applicable].
       *   In order to get the value of such an attribute node [anode],
       *   one can invoke the method [attribute]:
       *
       *   {[anode # attribute name]}
       *
       *   where [name] is the name of the attribute represented by
       *   [anode]. This will return the attribute value as [att_value]. Of
       *   course, the other attribute accessors can be called as well. 
       *   Furthermore, the method [data] will return the attribute value as
       *   string. Of course, every attribute node only contains the value of the
       *   one attribute it represents, and so it does not make sense to pass
       *   names of other attributes to the access methods.
       *   
       *   The attribute nodes live outside of the regular XML tree, and
       *   they are not considered as children of the element node. However,
       *   the element node is the parent node of the attribute nodes 
       *   (i.e. the children/parent relationship is asymmetric).
       *
       *   The method [attributes_as_nodes] computes the list of attribute
       *   nodes when it is first invoked after object creation or any 
       *   modification of the attribute list, and it will return the same list
       *   again in subsequent invocations.
       *
       * {b Domain.} This method is only applicable to elements.
       *)

    method pinstr : string -> proc_instruction list
      (** [pinstr n] returns all processing instructions that are
       *  directly contained in this object and that have a target
       *  specification of [n].
       *
       * {b Domain.} All node types. However, it is only reasonable to 
       * call this method
       * for processing instruction nodes, and for elements; for all
       * other node types the method will return the empty list.
       *
       * It depends on the parser configuration whether the processing
       * instructions are gathered in special processing instruction nodes
       * or in their containing elements. The former case is enabled when
       * the [enable_pinstr_nodes] config option is in effect. When a
       * processing instruction is parsed, a new processing instruction
       * node is added to the tree, and the [proc_instruction] object
       * is added to this node, and can be queried by calling this method.
       *
       * If the mentioned config option is not active (which is the default),
       * the [proc_instruction] object is simply added to the containing
       * element, and the exact position of the instruction is not reflected
       * in the tree.
       *)

    method pinstr_names : string list
      (** Returns the targets of all processing instructions that are
       * directly contained in this object. The target is the first word
       * in the processing instruction. Use the [pinstr] method to get
       * the full data of a processing instruction.
       *
       * {b Domain.} All node types. However, as for [pinstr] only a few
       * types of nodes can be filled with processing instruction. See
       * the description at {!Pxp_document.node.pinstr} for details.
       *)

    method comment : string option
      (**  Returns [Some text] if the node is a comment node and if
       *    [text] is the comment string (without the delimiters [<!--] and
       *    [-->]). Otherwise, [None] is passed back.
       *
       *    Note: The [data] method also returns the comment string, but it
       *    raises [Not_found] if the string is not available. This is the
       *    only difference between these methods when called on comment
       *    nodes.
       *
       * {b Domain.} All node types. Note that the method will always return
       *    [None] for non-comment nodes.
       *)




    (** Meta data *)

    method position : (string * int * int)
      (**  Returns a triple [(entity,line,pos)] describing the 
       *   location of the element in the original XML text. This triple is
       *   only available for elements, and only if the parser has been
       *   configured to store positions (see parser option
       *   [store_element_positions]). If available, [entity] describes 
       *   the entity where the element occurred, [line] is the line number
       *   [>= 1], and [pos] is the byte position of the first character
       *   of the element in the line. 
       *
       *   If unavailable, the method will return the triple [("?",0,0)].
       *
       * {b Domain.} All node types. Note that the method will always return
       *   [("?",0,0)] for non-element nodes.
       *)

    method classify_data_node : 'ext node -> data_node_classification
      (** [classify_data_node n]:
       * Classifies the passed data node [n], and returns whether it
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
       *
       * Note that the method always returns and never raises an exception.
       *
       * {b Domain.} Elements.
       *)




    (** Modifying the tree structure *)

    method append_node : 'ext node -> unit
      (** [append_node n]:
       * Adds the node [n] at the end of the list of children. The 
       *   method expects that [n] is a root, and it requires that [n] and
       *   this node share the same DTD.
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.}  Elements, comments, processing instructions, data nodes,
       *   super root nodes.
       *)

    method add_node : ?force:bool -> 'ext node -> unit
      (** {b Deprecated}! Use [append_node] instead.
        *
        * [add_node n]:
       * Append new sub nodes -- mainly used by the parser itself, but
       * of course open for everybody. If an element is added, it must be
       * an orphan (i.e. does not have a parent node)
       *
       * {b Validation.} 
       * The method performs some basic validation checks if the current node
       * has a regular expression as content model, or is EMPTY. You can
       * turn these checks off by passing [~force:true] to the method.
       *
       * {b Domain.}  Elements, comments, processing instructions, data nodes,
       *   super root nodes.
       *)

    method insert_nodes : ?pos:int -> 'ext node list -> unit
      (** [insert_nodes ~pos nl]:
       *    Inserts the list of nodes [nl] in-place into the list of
       *    children. The insertion is performed at position [pos],
       *    i.e. in the modified list of children, the first element of
       *    [nl] will have position [pos]. If the optional argument [pos]
       *    is not passed to the method, the list [nl] is appended
       *    to the list of children. 
       *
       *    The method requires that all elements of
       *    the list [nl] are roots, and that all elements and [obj]
       *    share the same DTD.
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.}  Elements, comments, processing instructions, data nodes,
       *   super root nodes.
       *)

    method remove : unit -> unit
      (** Removes this node from the tree. After this
       *  operation, this node is no longer the child of the former father node,
       *  i.e. it does neither occur in the former parent's list of children
       *  nor is the former parent still the parent of this node. This node
       *  becomes orphaned, and is a singleton tree of its own.
       *
       *  If this node does not have a parent, [remove] does nothing.
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.}  Elements, comments, processing instructions, data nodes,
       *   super root nodes.
       *)

    method delete : unit
      (** {b Deprecated} alias for [remove] *)

    method remove_nodes : ?pos:int -> ?len:int -> unit -> unit
      (** [remove_nodes ~pos ~len ()]:
       * Removes the specified nodes from the list of children.
       * The method deletes the nodes from position [pos] to 
       * [pos+len-1]. The optional argument [pos] defaults to 0. The 
       * optional argument [len] defaults to the length of the children
       * list.
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.} Elements.
       *)

    method set_nodes : 'ext node list -> unit
      (** [set_nodes l]:
       *     Sets the list of children to [l]. It is required that
       *     every member of [l] is either a root or was already a children
       *     of this node before the method call, and it is required that 
       *     all members and the current object share the same DTD.
       *
       *     Former children which are not members of [l] are removed from
       *     the tree and get orphaned (see method [remove]).
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.} Elements.
       *)




    (** Modifying content *)

    method set_data : string -> unit
      (** [set_data s]:
       * This method sets the character string contained in 
       *  data nodes.
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.} Data nodes
       *)

    method set_attributes : (string * Pxp_core_types.att_value) list -> unit
      (** [set_attributes al]:
       * Sets the attributes of this element to [al].
       *
       * {b Validation.} 
       *    This method does not add missing attributes that are
       *    declared in the DTD. It also never rejects undeclared attributes.
       *    The passed values are not checked; they are simply taken as-are.
       *
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.} Elements.
       *)

    method set_attribute : ?force:bool -> string -> Pxp_core_types.att_value -> unit
      (** [set_attribute ~force n v]:
       *    Sets the attribute [n] of this element to the value [v].
       *    The attribute [n] must already exist, and gets a new value.
       *    If you pass [~force:true], however, the attribute is added
       *    to the attribute list if it is missing.
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.} Elements.
       *)

    method reset_attribute : string -> unit
      (** [reset_attribute n]:
       *   If the attribute [n] is a declared attribute, it is set
       *   to its default value, or to [Implied_value] if there is no default 
       *   (the latter is performed even if the attribute is [#REQUIRED]).
       *   If the attribute is an undeclared attribute, it is removed
       *   from the attribute list.
       *
       *   The idea of this method is to simulate what had happened if [n]
       *   had not been defined literally in the attribute list of the XML element.
       *   In validating mode, the parser would have chosen the default
       *   value if possible, or [Implied_value] otherwise, and in 
       *   well-formedness mode, the attribute would be simply missing
       *   in the attribute list.
       *
       *   It is intentionally not possible to remove a declared
       *   attribute with [reset_attribute]. 
       *
       * {b Validation.} 
       *    This method does not check whether the modified XML tree
       *    is still valid.
       *
       * {b Domain.} Elements.
       *)

    method quick_set_attributes : 
             (string * Pxp_core_types.att_value) list -> unit
      (** {b Deprecrated} alias for [set_attributes] *)

    method add_pinstr : proc_instruction -> unit
      (** [add_pinstr pi]:
       *   Adds the processing instruction [pi] to the set of
       *   processing instructions contained in this node. If this is an
       *   element node, you can add any number of processing instructions.
       *   If it is a processing instruction node, you can put at most
       *   one processing instruction into this node.
       *
       * {b Validation.}
       *   Processing instructions are outside the scope of validation.
       *
       * {b Domain.} Elements, processing instruction nodes.
       *)

    method set_comment : string option -> unit
      (** [set_comment c]:
       *     Sets the comment string contained in comment nodes if
       *    [c = Some s]. Otherwise, this method removes the comment string
       *    ([c = None]).
       *
       *    Note that the comment string must not include the delimiters
       *    [<!--] and [-->]. Furthermore, it must not contain any character
       *    or character sequence that are forbidden in comments, such
       *    as ["--"]. However, this method does not check this condition.
       *
       * {b Validation.}
       *   Comments are outside the scope of validation.
       *
       * {b Domain.} Comment nodes.
       *)




    (** Validation *)

    method validate : unit -> unit
      (** [validate ()]:
       *     Calls [validate_contents] and [validate_attlist], and
       *     ensures that this element is locally valid. The method 
       *     returns [()] if the element is valid, and raises an exception
       *     otherwise.
       *
       * {b Domain.} All node types. However, for non-element nodes this
       *   check is a no-op.
       *)

    method validate_contents : 
              ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
      (** [validate_contents ?use_dfa ?check_data_nodes ()]:
       *     Checks that the subnodes of this element match the declared
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
       *     Note that the check is not performed recursively, but only
       *     on this node.
       *  
       *  {ul
       *    {- Option [use_dfa]: If true, the deterministic finite automaton of
       *       regexp content models is used for validation, if available.
       *       Defaults to false.}
       *    {- Option [check_data_nodes]: If true, it is checked whether data
       *       nodes in the list of children only occur at valid positions. 
       *       If false, these checks
       *       are left out. Defaults to true. (Usually, the parser turns
       *       this feature off because the parser already performs a similar
       *       check.)
       *
       *       See [classify_data_node] for details about what is checked.
       *       Some elements do not allow that there are inner data nodes,
       *       or restrict data nodes to whitespace.
       *       }
       *  }
       *
       * {b Domain.} All node types. However, there are only real checks for
       *    elements; for other nodes, this method is a no-op.
       *)

    method local_validate : 
              ?use_dfa:bool -> ?check_data_nodes:bool -> unit -> unit
      (** {b Deprecated} alias for [validate_contents] *)


    method complement_attlist : unit -> unit
      (** [complement_attlist ()]:
       *     Adds attributes that are declared in the DTD but are
       *     currently missing: [#IMPLIED] attributes are added with 
       *     [Implied_value], and if there is a default value for an attribute, 
       *     this value is added. [#REQUIRED] attributes are set to
       *     [Implied_value], too.
       * 
       *     It is only necessary to call this method if the element is created
       *     with [~valcheck:false], or the attribute list has been modified,
       *     and the element must be again validated.
       *
       * {b Domain.} Elements.
       *)

    method validate_attlist : unit -> unit
      (** [validate_attlist ()]:
       * Checks whether the attribute list of this element
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
       *
       * {b Domain.} All node types. However, for non-element nodes this
       *   check is a no-op.
       *)




    (** Clones *)

    method orphaned_clone : 'self
      (**  Returns a clone of the node and the complete tree below
       *   this node (deep clone). The clone does not have a parent (i.e. the
       *    reference to the parent node is not cloned). While copying the
       *    subtree strings are skipped; normally the original tree and the
       *    copy tree share strings. Extension objects are cloned by invoking
       *    the [clone] method on the original objects; how much of
       *    the extension objects is cloned depends on the implemention of
       *    this method.
       * 
       * {Domain.} All node types.
       *)

    method orphaned_flat_clone : 'self
      (**  return a clone of this element where all subnodes are omitted.
       *     The type of the node, and the attributes are the same as in the
       *     original node. The clone has no parent.
       *
       * {b Domain.} All node types.
       *)




    (** Creating new nodes by cloning exemplars *)

    method create_element :
             ?name_pool_for_attribute_values:Pxp_core_types.pool ->
             ?position:(string * int * int) ->
	     ?valcheck:bool ->      (* default: true *)
	     ?att_values:((string * Pxp_core_types.att_value) list) ->
             dtd -> node_type -> (string * string) list -> 'ext node
      (** [create_element ~name_pool_for_attribute_values ~position ~valcheck ~att_values dtd ntype att_list]:
       *   This method is usually only called on exemplars to create
       *   fresh nodes of the same class as the examplars. This is done
       *   by copying the exemplars, and setting the properties of the
       *   (flat) copies as follows:
       *
       *     - The DTD is set to [dtd]
       *     - The node type is set to [ntype] (which must be [T_element name])
       *     - The attribute list is set to the concatenation of 
       *       [att_list] and [att_values]; [att_list] passes attribute values
       *       as strings while [att_values] passes attribute values as
       *       type [att_value]
       *     - The copy does not have children nor a parent
       *     - The copy does not contain processing instructions.
       *     - The position triple is set to [position]
       *
       *   Note that the extension object is copied, too.
       *
       *   If [valcheck=true] (the default), it is checked whether the 
       *   element type exists and whether the passed attributes match the
       *   declared attribute list. Missing attributes are automatically
       *   added, if possible. If [valcheck=false], any element type
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
       *
       * {b Domain.} Elements.
       *)

    method create_data : dtd -> string -> 'ext node
      (** [create_data dtd cdata]:
       *   This method is usually only called on exemplars to create
       *   fresh nodes of the same class as the examplars. This is done
       *   by copying the exemplars, and setting the properties of the
       *   (flat) copies as follows:
       *     - The DTD is set to [dtd]
       *     - The character string is set to [cdata]
       *
       *   Note that the extension object is copied, too.
       *
       * {b Domain.} Data nodes.
       *)

    method create_other : 
             ?position:(string * int * int) ->
             dtd -> node_type -> 'ext node
      (** [create_other ~position dtd ntype]:
       *   This method is usually only called on exemplars to create
       *   fresh nodes of the same class as the examplars. This is done
       *   by copying the exemplars, and setting the properties of the
       *   (flat) copies as follows:
       *     - The DTD is set to [dtd]
       *     - The position triple is set to [position]
       *
       *   Note that the extension object is copied, too.
       *
       *   The passed node type [ntype] must match the node type
       *   of this node.
       *
       * {b Domain.} Super root nodes, processing instruction nodes,
       *    comment nodes
       *)


    (** Namespaces

        Namespace methods are only available in namespace-aware implementations
        of [node]. For other implementations, the exception
        {!Pxp_types.Namespace_method_not_supported} is raised.
     *)

    method normprefix : string
      (** For namespace-aware implementations of the node class, this
       * method returns the normalized prefix of the element or attribute.
       * If the object does not have a prefix, "" will be passed back.
       *
       * The normalized prefix is the part of the name before the 
       * colon after prefix normalization has been applied to the node.
       *
       * {b Domain.} Elements and attributes supporting namespaces.
       *)

    method display_prefix : string
      (** For namespace-aware implementations of the node class, this
       *   method returns the display prefix of the element or attribute.
       *   If the object does not have a prefix, "" will be passed back.
       *
       * The display prefix is the prefix as it occurs literally in the XML
       * text.
       *
       *     Actually, this method does not return the real display prefix
       *     that was found in the XML text but the most recently declared
       *     display prefix bound to the namespace URI of this element or
       *     attribute, i.e. this method infers the display prefix. The
       *     result can be a different prefix than the original prefix
       *     if the same namespace URI is bound several times in the
       *     current namespace scope.
       *
       *     This method is quite slow.
       *
       * {b Domain.} Elements and attributes supporting namespaces.
       *)

    method localname : string
      (** For namespace-aware implementations of the node class, this
       * method returns the local part of the name of the element or
       * attribute.
       *
       *     The local name is the part of the name after the colon, or
       *     the whole name if there is no colon.
       *
       * {b Domain.} Elements and attributes supporting namespaces.
       *)

    method namespace_uri : string
      (** For namespace-aware implementations of the node class, this
       *  method returns the namespace URI of the element, attribute or
       *     namespace.
       *
       *     If the node does not have a namespace prefix, and there is no
       *     default namespace, this method returns "".
       *
       *     The namespace URI is the unique name of the namespace.
       *
       * {b Domain.} Elements and attributes supporting namespaces; furthermore
       *     namespace nodes.
       *)

    method namespace_manager : namespace_manager
      (** For namespace-aware implementations of the node class,
       *  this method returns the namespace manager. If the namespace
       *  manager has not yet been set, the exception [Not_found] is raised.
       *
       *      The namespace manager is an object that holds the mapping
       *      from namespace prefixes to namespace URIs, and vice versa.
       *      It is contained in the DTD, and must be configured there.
       *
       * {b Domain.} Elements and attributes supporting namespaces; furthermore
       *     namespace nodes.
       *)

    method namespace_scope : namespace_scope
      (** Returns additional information about the namespace
       * structure in the parsed XML text. In particular, the namespace
       * scope describes the literal (unprocessed) namespace prefixes
       * in the XML text, and how they are mapped to the namespace URIs.
       *
       * When printing XML text, the namespace scope may be used
       * to give the printer hints where to introduce namespaces, and
       * which namespace prefixes are preferred.
       *
       * {b Domain.} Elements and attributes supporting namespaces
       *)

    method set_namespace_scope : namespace_scope -> unit
      (** [set_namespace_scope scope]:
       * Sets the namespace scope object. It is required that
       * this object is connected to the same namespace manager as
       * the rest of the document tree.
       *
       * {b Domain.} Elements and attributes supporting namespaces
       *)

    method namespaces_as_nodes : 'ext node list
      (**  Returns the namespaces found in the [namespace_scope]
       * object and all parent scope objects (except declarations that
       * are hidden by more recent declarations). The namespaces are
       * returned as node objects with type [T_namespace name] where
       * [name] is the normalized prefix.
       *
       * This method should be used if it is required for typing reasons
       * that the namespaces have also type [node]. A common example
       * are sets that may both contain elements and namespaces, as they
       * are used in the XPath language.
       *
       * The namespace nodes are read-only; any call to a method
       * modifying their contents will raise [Method_not_applicable].
       * See the class [namespace_impl] below for more information 
       * about the returned nodes.
       *
       * The namespace nodes live outside of the regular XML tree, and
       * they are not considered as children of the element node. However,
       * the element node is the parent node of the namespace nodes 
       * (i.e. the children/parent relationship is asymmetric).
       *
       * The method [namespaces_as_nodes] computes the list of namespace
       * nodes when it is first invoked, and it will return the same list
       * again in subsequent invocations.
       *
       * {b Domain.}  This method is only applicable to elements that
       *   support namespaces.
       *)


    (** Writing trees as XML text

        The [write] and [display] methods are very similar. The main difference
        is how namespaces are handled. When generating XML text, the 
        namespaces need to be again represented as prefixes. The [write]
        method uses the normalized prefixes for this purpose. The [display]
        method uses the display prefixes, i.e. the prefixes as they orginally
        have been in the parsed XML text. This means for parsed XML text
        [display] produces an more exact copy of the text, whereas 
        [write] shows the prefixes as they are seen by the program.
     *)

    method write : 
             ?prefixes:string list ->
	     ?default:string ->
             ?minimization:[`AllEmpty | `DeclaredEmpty | `None] ->
             Pxp_core_types.output_stream -> Pxp_core_types.encoding -> unit
      (** [write stream enc]:
       *    Write the contents of this node and the subtrees to the passed
       *    [stream] encoded as [enc]. The generated output is again XML.
       *    The output style is rather compact and should not be considered
       *    as "pretty printing".
       *
       *    The namespace-aware nodes use a notation with normalized
       *    prefixes. The namespace scope is ignored.
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
       *   Option [~minimization]: How to write out empty elements. [`AllEmpty]
       *   means that all empty elements are minimized (using the <name/>
       *   form). [`DeclaredEmpty] minimizes only empty elements that are
       *   declared as empty in the DTD. [`None] does not minimize at all
       *   and is the default.
       *
       * {b Domain.} All regular node types (elements, data nodes, comments,
       *   processing instructions, super root nodes).
       *)

    method display :
             ?prefixes:string StringMap.t ->
             ?minimization:[`AllEmpty | `DeclaredEmpty | `None] ->
	      Pxp_core_types.output_stream -> Pxp_core_types.encoding -> unit
      (** [display stream enc]:
       *    Write the contents of this node and the subtrees to the passed
       *    [stream] encoded as [enc]. The generated output is again XML.
       *    The output style is rather compact and should not be considered
       *    as "pretty printing".
       *
       *    The namespace-aware nodes try to follow the namespace scoping
       *    found in the nodes. The generated namespace prefixes are
       *    display prefixes. Missing prefixes are complemented, but this
       *    is slow.
       *
       *   Option [~prefixes]: The class [namespace_element_impl] interprets 
       *   this option and passes it recursively to subordinate invocations of
       *   [display]. The mapping contains the declarations currently in
       *   effect as pairs of [(prefix,uri)]. The option
       *   defaults to [] forcing the method to output all necessary prefix
       *   declarations.
       *
       *   Option [~minimization]: How to write out empty elements. [`AllEmpty]
       *   means that all empty elements are minimized (using the <name/>
       *   form). [`DeclaredEmpty] minimizes only empty elements that are
       *   declared as empty in the DTD. [`None] does not minimize at all
       *   and is the default.
       *
       * {b Domain.} All regular node types (elements, data nodes, comments,
       *   processing instructions, super root nodes).
       *)


    (** Internals

        These methods are considered as private of the implementation.
     *)

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

    method dump : Format.formatter -> unit

  end
;;


(** {2 Implementations of nodes} *)

class [ 'ext ] data_impl : 'ext -> [ 'ext ] node
  (** This class is an implementation of [node] which
   *  realizes data nodes. You can create a new exemplar by
   *
   *  [let exemplar = new data_impl ext_obj]
   *
   *   which creates an empty data node which already contains a
   *   reference to the extension [ext_obj], but is otherwise empty.
   *   Use the method {!Pxp_document.node.create_data} on the exemplar 
   *   to get a filled copy
   *   of the examplar (i.e. we need two steps to create nodes).
   *)


class [ 'ext ] element_impl : 'ext -> [ 'ext ] node
  (**  This class is an implementation of [node] which
   *   realizes element nodes. You can create a new exemplar by
   *
   *   [let exemplar = new element_impl ext_obj]
   *
   *   which creates an empty element which already contains a
   *   reference to the extension [ext_obj], but is otherwise empty.
   *   Use the method {!Pxp_document.node.create_element} on the exemplar 
   *   to get a filled copy
   *   of the examplar (i.e. we need two steps to create nodes).
   *
   *   Note that the class [element_impl] is not namespace-aware.
   *)


class [ 'ext ] comment_impl : 'ext -> [ 'ext ] node ;;
  (**  This class is an implementation of [node] which
   *   realizes comment nodes. You can create a new exemplar by
   *
   *   [let exemplar = new comment_impl ext_obj]
   *
   *   which creates an empty comment which already contains a
   *   reference to the extension [ext_obj], but is otherwise empty.
   *   Use the method {!Pxp_document.node.create_other} on the exemplar 
   *   to get a filled copy
   *   of the examplar (i.e. we need two steps to create nodes).
   *)

class [ 'ext ] pinstr_impl : 'ext -> [ 'ext ] node ;;
  (** This class is an implementation of [node] which
   *   realizes processing instruction nodes. You can create a new exemplar by
   *
   *   [let exemplar = new pinstr_impl ext_obj]
   *
   *   which creates an empty node which already contains a
   *   reference to the extension [ext_obj], but is otherwise empty.
   *   Use the method {!Pxp_document.node.create_other} on the exemplar 
   *   to get a filled copy
   *   of the examplar (i.e. we need two steps to create nodes).
   *)

class [ 'ext ] super_root_impl : 'ext -> [ 'ext ] node ;;
  (**  This class is an implementation of [node] which
   *   realizes super root nodes. You can create a new exemplar by
   *
   *   [let exemplar = new super_root_impl ext_obj]
   *
   *   which creates an empty super root which already contains a
   *   reference to the extension [ext_obj], but is otherwise empty.
   *   Use the method {!Pxp_document.node.create_other} on the exemplar 
   *   to get a filled copy
   *   of the examplar (i.e. we need two steps to create nodes).
   *)

class [ 'ext ] attribute_impl :
  element:string -> 
  name:string -> 
  Pxp_core_types.att_value -> 
  dtd -> 
    [ 'ext ] node
    (** This class is an implementation of [node] which
     *   realizes attribute nodes.
     * Create a new node by
     *
     * {[new attribute_impl element_name attribute_name attribute_value dtd]}
     *
     * Note that attribute nodes do intentionally not have extensions.
     *
     * Attribute nodes are created on demand by the first invocation of
     * [attributes_as_nodes] of the element node. Attribute nodes are
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


class [ 'ext ] namespace_impl :
  (* dspprefix: *) string -> (* normprefix: *) string -> dtd -> [ 'ext ] node
  (** Namespace objects are only used to represent the namespace declarations
   * occurring in the attribute lists of elements.
   *)

class [ 'ext ] namespace_element_impl : 'ext -> [ 'ext ] node
  (** This class is an implementation of [node] which
   *   realizes element nodes. In contrast to [element_impl], this class
   *   also implements the namespace methods.
   *
   * This class has an extended definition of the [create_element] method.
   * It accepts element names of the form "normprefix:localname" where
   * normprefix must be a prefix managed by the [namespace_manager]. Note
   * that [create_element] does not itself normalize prefixes; it is expected
   * that the prefixes are already normalized. 
   *
   * In addition to calling [create_element], one can set the namespace scope
   * after creation ([set_namespace_scope]) to save the mapping of unprocessed
   * namespace prefixes to normalized prefixes. This is voluntary.
   *
   * Such nodes have the node type [T_element "normprefix:localname"].
   *
   * This class implements the namespace methods.
   *)

class [ 'ext ] namespace_attribute_impl :
  element:string -> 
  name:string -> 
  Pxp_core_types.att_value -> 
  dtd -> 
    [ 'ext ] node
  (** the namespace-aware implementation of attribute nodes. *)


(** {2 Useful accessor functions} *)

val pinstr : 'ext node -> proc_instruction
  (** [pinstr n]:
   *   Returns the processing instruction contained in a
   *   processing instruction node.
   *   This function raises [Invalid_argument] if invoked for a different node
   *   type than [T_pinstr].
   *)


val attribute_name  : 'ext node -> string
  (** [attribute_name n]
   * Returns the name of the attribute contained in an attribute
   * node. Raises [Invalid_argument] if [n] does not have node type
   * [T_attribute].
   *)

val attribute_value : 'ext node -> Pxp_core_types.att_value
  (** [attribute_value n]:
   *     Returns the value of the attribute contained in an attribute
   *    node. Raises [Invalid_argument] if [n] does not have node type
   *    [T_attribute].
   *)

val attribute_string_value : 'ext node -> string
  (** [attribute_string_value n]:
   *    Returns the string value of the attribute contained in an attribute
   *    node. Raises [Invalid_argument] if [n] does not have node type
   *    [T_attribute].
   *)

val namespace_normprefix : 'ext node -> string
  (** Returns the normprefix of a namespace node.
      Raises [Invalid_argument] if [n] does not have node type
      [T_namespace].
   *)

val namespace_display_prefix : 'ext node -> string
  (** Returns the display prefix of a namespace node .
      Raises [Invalid_argument] if [n] does not have node type
      [T_namespace].
  *)

val namespace_uri : 'ext node -> string
  (** Retruns the namespace URI of a namespace node .
      Raises [Invalid_argument] if [n] does not have node type
      [T_namespace].
  *)



(** {2 Document model specifications} *)

type 'ext spec
constraint 'ext = 'ext node #extension
  (** The abstract data type of the document model specification.
      These values define objects of which classes are actually created
      for the various types of nodes.
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
  (** [make_spec_from_mapping
   *     ~super_root_exemplar ~comment_exemplar ~default_pinstr_exemplar
   *     ~pinstr_mapping ~data_exemplar ~default_element_exemplar
   *     ~element_mapping
   *     ()]:
   *   Creates a [spec] from the arguments. Some arguments are optional,
   *    some arguments are mandatory.
   *      - [super_root_exemplar]: Specifies the exemplar to be used for
   *        new super root nodes. This exemplar is optional.
   *      - [comment_exemplar]: Specifies the exemplar to be used for
   *        new comment nodes. This exemplar is optional.
   *      - [pinstr_exemplar]: Specifies the exemplar to be used for
   *        new processing instruction nodes by a hashtable mapping target
   *        names to exemplars. This hashtable is optional.
   *      - [default_pinstr_exemplar]: Specifies the exemplar to be used for
   *        new processing instruction nodes. This exemplar will be used
   *        for targets that are not contained in the [~pinstr_exemplar]
   *        hashtable. This exemplar is optional.
   *      - [data_exemplar]: Specifies the exemplar to be used for
   *        new data nodes. This exemplar is mandatory.
   *      - [element_mapping]: Specifies the exemplar to be used for
   *        new element nodes by a hashtable mapping element types to
   *        exemplars. This hashtable is mandatory (but may be empty).
   *      - [default_element_exemplar]: Specifies the exemplar to be used for
   *        new element nodes. This exemplar will be used
   *        for element types that are not contained in the [~element_mapping]
   *        hashtable. This exemplar is mandatory.
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
  (**  [make_spec_from_alist
   *      ~super_root_exemplar ~comment_exemplar ~default_pinstr_exemplar
   *      ~pinstr_alist ~data_exemplar ~default_element_exemplar
   *      ~element_alist
   *      ()]:
   *    Creates a [spec] from the arguments. This is a convenience
   *    function for [make_spec_from_mapping]; instead of requiring hashtables
   *    the function allows it to pass associative lists.
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
  (** These functions just return the exemplars (or raise [Not_found]). *)

  (*
   * Notes:
   * (1) In future versions, it may be possible that the element exemplar
   *     depends on attributes, too, so the attlist must be passed
   *     to get_element_exemplar
   * (2) In future versions, it may be possible that the pinstr exemplar
   *     depends on the full value of the processing instruction and
   *     not only on the target, so the full proc_instruction must be
   *     passed to get_pinstr_exemplar.
   *)


(** {2 Creating nodes from specifications} *)

val create_data_node :
      'ext spec -> dtd -> string -> 'ext node
  (** [create_data_node spec dtd datastring]:
   *     Creates a new data node from the exemplar contained in [spec].
   *    The new node contains [datastring] and is connected with the [dtd].
   *)

val create_element_node :
      ?name_pool_for_attribute_values:Pxp_core_types.pool ->
      ?position:(string * int * int) ->
      ?valcheck:bool ->
      ?att_values:((string * Pxp_core_types.att_value) list) ->
      'ext spec -> dtd -> string -> (string * string) list -> 'ext node
  (** [create_element_node ~name_pool_for_attribute_values
   *              ~position ~valcheck ~att_values spec dtd eltype
   *              att_list]:
   * Creates a new element node from the exemplar(s) contained in
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
   *)

val create_super_root_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> 'ext node
  (** [create_super_root_node ~position spec dtd]:
   *    Creates a new super root node from the exemplar contained in
   *    [spec]. The new node is connected to [dtd], and the position
   *    triple is set to [~position].
   *
   *    The function fails if there is no super root exemplar in [spec].
   *)

val create_comment_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> string -> 'ext node
  (** [create_comment_node ~position spec dtd commentstring]:
   *    Creates a new comment node from the exemplar contained in
   *    [spec]. The new node is connected to [dtd], and the position
   *    triple is set to [~position]. The contents of the node are set
   *    to [commentstring].
   *
   *    The function fails if there is no comment exemplar in [spec].
   *)


val create_pinstr_node :
      ?position:(string * int * int) ->
      'ext spec -> dtd -> proc_instruction -> 'ext node
  (** [create_pinstr_node ~position spec dtd pi]:
   *    Creates a new processing instruction node from the exemplar 
   *    contained in [spec]. The new node is connected to [dtd], and the 
   *    position triple is set to [~position]. The contents of the node are set
   *    to [pi].
   *
   *    The function fails if there is no processing instruction exemplar in
   *    [spec].
   *)

val create_no_node :
       ?position:(string * int * int) -> 'ext spec -> dtd -> 'ext node
  (** Creates a T_none node with limited functionality.
   * {b Important:} This function is conceptually broken and may be dropped in the
   * future.
   *)



(** {2 Document order} *)


(** The functions [compare] and [ord_compare] implement the so-called
 * "document order". The basic principle is that the nodes are linearly
 * ordered by their occurence in the textual XML representation of the
 * tree. While this is clear for element nodes, data nodes, comments, and
 * processing instructions, a more detailed definition is necessary for the
 * other node types. In particular, attribute nodes of an element node
 * occur before any regular subnode of the element, and namespace nodes
 * of that element occur even before the attribute nodes. So the order
 * of nodes of
 *  {[ <sample a1="5" a2="6"><subnode/></sample>  ]}
 * is
 * {ol
 *   {- element "sample"}
 *   {- attribute "a1"}
 *   {- attribute "a2"}
 *   {- element "subnode"}}
 * Note that the order of the attributes of the same element is unspecified,
 * so "a2" may alternatively be ordered before "a1". If there were namespace
 * nodes, they would occur between 1 and 2.
 *
 *   If there is a super root node, it will be handled as the very first
 * node.
 *)

val compare : 'ext node -> 'ext node -> int
  (** [compare n1 n2]:
   *  Returns -1 if [n1] occurs before [n2], or +1 if [n1] occurs
   * after [n2], or 0 if both nodes are identical.
   * If the nodes are unrelated (do not have a common ancestor), the result
   * is undefined (Note: this case is different from [ord_compare]).
   * This test is rather slow, but it works even if the XML tree changes
   * dynamically (in contrast to [ord_compare] below).
   *)

type 'ext ord_index
constraint 'ext = 'ext node #extension
  (** The type of ordinal indexes. *)

val create_ord_index : 'ext node -> 'ext ord_index
  (** [create_ord_index startnode]:
   * Creates an ordinal index for the subtree starting at [startnode].
   * This index assigns to every node an ordinal number (beginning with 0) such
   * that nodes are numbered upon the order of the first character in the XML
   * representation (document order).
   * Note that the index is not automatically updated when the tree is
   * modified.
   *)

val ord_number : 'ext ord_index -> 'ext node -> int
  (** Returns the ordinal number of the node, or raises [Not_found].
   * Note that attribute nodes and namespace nodes are treated specially:
   * All attribute nodes for a certain element node have the _same_
   * ordinal index. All namespace nodes for a certain element node
   * have the _same_ ordinal index.
   *
   * (So ord_number x = ord_number y does not imply x == y for these
   * nodes. However, this is true for the other node types.)
   * It is not recommended to work with the ordinal number directly but
   * to call ord_compare which already handles the special cases.
   *)

val ord_compare : 'ext ord_index -> 'ext node -> 'ext node -> int
  (** [ord_compare idx n1 n2]:
   * Compares two nodes like [compare]:
   * Returns -1 if [n1] occurs before [n2], or +1 if [n1] occurs
   * after [n2], or 0 if both nodes are identical.
   * If one of the nodes does not occur in the ordinal index, [Not_found]
   * is raised. (Note that this is a different behaviour than what [compare]
   * would do.)
   *
   * This test is much faster than [compare].
   *)



(** {2:iterators Document iterators} *)

(** General note: The iterators ignore attribute and namespace nodes *)



val find : ?deeply:bool ->
           ('ext node -> bool) -> 'ext node -> 'ext node
  (** [find ~deeply f startnode]
   *     Searches the first node in the tree below [startnode] for which 
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
   *)

val find_all : ?deeply:bool ->
               ('ext node -> bool) -> 'ext node -> 'ext node list
  (** [find_all ~deeply f startnode]:
   *     Searches all nodes in the tree below [startnode] for which 
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
   *)

val find_element : ?deeply:bool ->
                   string -> 'ext node -> 'ext node
  (** [find_element ~deeply eltype startnode]:
   *      Searches the first element in the tree below [startnode] 
   *     that has the element type [eltype], and returns it. Raises [Not_found]
   *     if there is no such node.
   *
   *     By default, [~deeply=false]. In this case, only the children of
   *     [startnode] are searched.
   *
   *     If passing [~deeply=true], the children are searched recursively
   *     (depth-first search). Note that even in this case [startnode] itself
   *     is not checked.
   *)

val find_all_elements : ?deeply:bool ->
                        string -> 'ext node -> 'ext node list
  (** [find_all_elements ~deeply eltype startnode]:
   *     Searches all elements in the tree below [startnode] 
   *     having the element type [eltype], and returns them.
   *
   *     By default, [~deeply=false]. In this case, only the children of
   *     [startnode] are searched.
   *
   *     If passing [~deeply=true], the children are searched recursively
   *     (depth-first search). Note that even in this case [startnode] itself
   *     is not checked.
   *)

exception Skip
  (**  This exception can be used in the functions passed to
   *    [map_tree], [map_tree_sibl], [iter_tree], and [iter_tree_sibl]
   *    to skip the current node, and to proceed with the next node.
   *    See these function for details.
   *)

val map_tree :  pre:('exta node -> 'extb node) ->
               ?post:('extb node -> 'extb node) ->
               'exta node ->
                   'extb node
  (** [map_tree ~pre ~post startnode]
   *     Maps the tree beginning at [startnode] to a second tree
   *    using the following algorithm.
   *
   *    [startnode] and the whole tree below it are recursively traversed.
   *    After entering a node, the function ~pre is called. The result of
   *    this function must be a new node; it must not have children nor a
   *    parent. For example, you can pass
   *      {[~pre:(fun n -> n # orphaned_flat_clone)]}
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
   *    {[ map_tree ~pre:(fun n -> if n # node_type = T_comment then raise Skip else n # orphaned_flat_clone) startnode ]}
   *
   *     Attribute and namespace nodes are ignored.
   *)

val map_tree_sibl :
        pre: ('exta node option -> 'exta node -> 'exta node option ->
                  'extb node) ->
       ?post:('extb node option -> 'extb node -> 'extb node option ->
                  'extb node) ->
       'exta node ->
           'extb node
  (** [map_tree_sibl ~pre ~post startnode]:
   *    Maps the tree beginning at [startnode] to a second tree
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
   *      {[~pre:(fun prev n next -> n # orphaned_flat_clone)]}
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
  (** [iter_tree ~pre ~post startnode]:
   *    Iterates over the tree beginning at [startnode] 
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
   *)


val iter_tree_sibl :
       ?pre: ('ext node option -> 'ext node -> 'ext node option -> unit) ->
       ?post:('ext node option -> 'ext node -> 'ext node option -> unit) ->
       'ext node ->
           unit
  (** [iter_tree_sibl ~pre ~post startnode]:
   *   Iterates over the tree beginning at [startnode] 
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
   *)


(************************ Whitespace handling ***************************)

(** {2 Whitespace} *)

type stripping_mode =
  [ `Strip_one_lf
  | `Strip_one
  | `Strip_seq
  | `Disabled
  ]
  (** The different ways how to strip whitespace from a single
   *    data node:
   * - [`Strip_one_lf]: If there is a linefeed character at the beginning/at
   *   the end, it will be removed. If there are more linefeed characters, 
   *   only the first/the last is removed.
   *   (This is the SGML rule to strip whitespace.)
   * - [`Strip_one]: If there is a whitespace character at the beginning/at
   *   the end, it will be removed. If there are more whitespace characters, 
   *   only the  first/the last is removed. Whitespace characters are space, 
   *   newline, carriage return, and tab.
   * - [`Strip_seq]: All whitespace characters at the beginning/at the end are
   *   removed.
   * - [`Disabled]: Do not strip whitespace.
   *)


val strip_whitespace : 
      ?force:bool -> ?left:stripping_mode -> ?right:stripping_mode ->
      ?delete_empty_nodes:bool ->
      'ext node ->
      unit
  (** [strip_whitespace ~force ~left ~right ~delete_empty_nodes startnode]:
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
   * 
   * Defaults:
   *   - [~force:false]
   *   - [~left:`Disabled]
   *   - [~right:`Disabled]
   *)


(****************************** normalization ****************************)

(** {2 Normalization} *)

val normalize : 'ext node -> unit
  (** Normalizes the tree denoted by [startnode]  such that
   *  neither empty data nodes nor adjacent data nodes exist. Normalization
   *    works in-place.
   *)

(******************************** validation *****************************)

(** {2 Validation} *)

val validate : 'ext node -> unit
  (** [validate startnode]:
   *   Validates the tree denoted by [startnode]. In contrast to
   *   [startnode # validate()] this function validates recursively.
   *)

(******************************* document ********************************)

(** {2 The document type} *)

    (** Documents are used to represent closed documents that may
     * consist of an XML declaration, a DTD, and a node tree.
     *
     * Important invariant: A document is either empty (no root element,
     * no DTD), or it has both a root element and a DTD.
     *
     * A fresh document created by [new] is empty.
     *)
class [ 'ext ] document :
  ?swarner:Pxp_core_types.symbolic_warnings ->
  Pxp_core_types.collect_warnings -> Pxp_core_types.rep_encoding ->
  object

    method init_xml_version : string -> unit
	(** Set the XML version string of the XML declaration. *)

    method init_root : 'ext node -> string -> unit
	(** Set the root element. It is expected that the root element has
	 * a DTD.
	 * The second argument is the original name of the root element
	 * (without namespace prefix processing).
	 * Note that [init_root] checks whether the passed root element
	 * has the type expected by the DTD. The check takes into account
	 * that the root element might be a virtual root node.
	 *)

    method xml_version : string
      (** Returns the XML version from the XML declaration. Returns "1.0"
       * if the declaration is missing.
       *)

    method xml_standalone : bool
      (** Returns whether this document is declared as being standalone.
       * This method returns the same value as 'standalone_declaration'
       * of the DTD (if there is a DTD).
       * Returns [false] if there is no DTD.
       *)

    method dtd : dtd
      (** Returns the DTD of the root element.
       * Fails if there is no root element.
       *)

    method encoding : Pxp_core_types.rep_encoding
      (** Returns the string encoding of the document = the encoding of
       * the root element = the encoding of the element tree = the
       * encoding of the DTD.
       * Fails if there is no root element.
       *)

    method root : 'ext node
      (** Returns the root element, or fails if there is not any. *)

    method raw_root_name : string
      (** The unprocessed name of the root element (second arg of
       * [init_root])
       *)

    method add_pinstr : proc_instruction -> unit
      (** Adds a processing instruction to the document container.
       * The parser does this for PIs occurring outside the DTD and outside
       * the root element.
       *)

    method pinstr : string -> proc_instruction list
      (** Return all PIs for a passed target string. *)

    method pinstr_names : string list
      (** Return all target strings of all PIs. *)

    (** Writing documents as XML text

        The [write] and [display] methods are very similar. The main difference
        is how namespaces are handled. When generating XML text, the 
        namespaces need to be again represented as prefixes. The [write]
        method uses the normalized prefixes for this purpose. The [display]
        method uses the display prefixes, i.e. the prefixes as they orginally
        have been in the parsed XML text. This means for parsed XML text
        [display] produces an more exact copy of the text, whereas 
        [write] shows the prefixes as they are seen by the program.
     *)

    method write : ?default : string ->
                   ?prefer_dtd_reference : bool ->
                   ?dtd_style:[`Omit|`Reference|`Included|`Auto] ->
                   ?minimization:[`AllEmpty | `DeclaredEmpty | `None] ->
                   Pxp_core_types.output_stream -> 
                   Pxp_core_types.encoding -> 
                     unit
      (** Write the document to the passed
       * output stream; the passed encoding used. The format
       * is compact (the opposite of "pretty printing").
       * If a DTD is present, the DTD is included as internal subset.
       *
       * Option [~default]: Specifies the normprefix that becomes the
       * default namespace in the output.
       *
       * Option [~dtd_style]: Selects how to print the DTD. [`Omit] means
       * to omit the DTD at all (no DOCTYPE clause). 
       * [`Reference] prints the DTD reference to an
       * external entity (using SYSTEM or PUBLIC identifier), if possible,
       * and falls back to [`Included] otherwise. [`Included] means to
       * always include the DTD as internal subset. [`Auto] tries to find
       * the best way: If there is a DTD, try [`Reference] then [`Included].
       * Otherwise, [`Omit]. The default is [`Included].
       *
       * Option [~prefer_dtd_reference]: Same as [~dtd_style:`Reference]
       * (backward-compatible).
       *
       * Option [~minimization]: How to write out empty elements. [`AllEmpty]
       * means that all empty elements are minimized (using the <name/>
       * form). [`DeclaredEmpty] minimizes only empty elements that are
       * declared as empty in the DTD. [`None] does not minimize at all
       * and is the default.
       *)


    method display : ?prefer_dtd_reference : bool ->
                     ?dtd_style:[`Omit|`Reference|`Included|`Auto] ->
                     ?minimization:[`AllEmpty | `DeclaredEmpty | `None] ->
                     Pxp_core_types.output_stream -> 
                     Pxp_core_types.encoding -> 
                       unit
      (** Write the document to the passed
       * output stream; the passed encoding used. The format
       * is compact (the opposite of "pretty printing").
       * If a DTD is present, the DTD is included as internal subset.
       * In contrast to [write], this method uses the display namespace
       * prefixes instead of the normprefixes.
       *
       * Option [~dtd_style]: Same meaning as in [write].
       *
       * Option [~prefer_dtd_reference]: Same meaning as in [write].
       *
       * Option [~minimization]: Same meaning as in [write].
       *)

    method dump : Format.formatter -> unit

  end
;;


(** {2 Printers for the toploop} *)

(** These functions are intented to be used with the [#install_printer]
    directive of the toploop
 *)

(* Printers for toploop: *)

val print_node :
    'ext node -> unit ;;

val print_doc :
    'ext document -> unit ;;


(** {2 Conversion between trees and event streams} *)

(** We use the metaphor of "solid" XML for trees and other randomly accessible
    data structures representing XML, and the metaphor of "liquid" XML
    for event streams describing XML
 *)

exception Error_event of exn
  (** The event stream contains an [E_error] event *)

type 'ext solid_xml =
    [ `Node of 'ext node
    | `Document of 'ext document
    ]
  (** Solid XML can be a (sub)tree [`Node n], or a closed [`Document] *)

val solidify :
    ?dtd:dtd ->
    config -> 'ext spec -> (unit -> event option) -> 'ext solid_xml
  (** Reads the event stream by calling the [unit->event] function, and
   * creates a node tree according to config, dtd, spec.
   *
   * The event stream may be either:
   * - A document event stream (as generated by [`Entry_document]). 
   *   In this case [`Document d] is returned.
   * - A content event stream (as generated by [`Entry_content]). 
   *   In this case [`Node n] is returned.
   *
   * Document streams contain a DTD. The found DTD is used for the
   * node tree. Content streams, on the contrary, do not contain DTDs.
   * In this case, an empty DTD is created (in well-formedness mode).
   *
   * The [dtd] argument overrides any DTD, no matter whether found
   * in the stream or freshly created.
   *
   * If the DTD allows validation, the returned tree is validated.
   *
   * The data nodes are not normalized unless the arriving data events
   * are already normalized. To get this effect, filter the stream
   * with {!Pxp_ev_parser.norm_cdata_filter} before calling solidify.
   *
   * Ignorable whitespace is not automatically removed. To get this
   * effect, filter the stream with 
   * {!Pxp_ev_parser.drop_ignorable_whitespace_filter} before calling solidify.
   *
   * The uniqueness of ID attributes is not checked.
   *)

val liquefy :
     ?omit_end: bool -> ?omit_positions:bool -> 'ext solid_xml -> 
     ('a -> event option)
  (** The converse of [solidify]: The passed node or document is transformed
   * into an event stream.
   *
   * - [omit_end]: If true, the [E_end_of_stream] event is omitted at the end.
   *   Useful to concatenate several streams. Default: false.
   * - [omit_positions]: If true, no [E_position] events are generated.
   *   Default:false.
   *)
