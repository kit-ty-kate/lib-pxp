(* $Id$ *)

open Pxp_types
open Pxp_document
open Pxp_dtd

(* - Define Pxp_set: Sets over 'ext node
 * - How to specify the ordering function?
 *
 * - What to do when encoding of nodes and encoding of xpath differ?
 *
 * - Better emulation of missing super root nodes
 *)


module type NODE = sig
  type t = 'n extension node as 'n
  val compare : t -> t -> int
end

module Node = struct
  type t = 'n extension node as 'n
  let compare m n =
    Pxp_document.compare m n
end

module Make(XNode : NODE) = struct

  module XSet =
    Set.Make(XNode)

  type xpath_value =
      [ `Nodeset of XSet.t
      | `Boolean of bool
      | `Number of float
      | `String of string
      ]

  type 'value open_context =
      { ctx_node : XNode.t;
	ctx_position : int;
	ctx_size : int;
	ctx_var_binding : string -> 'value;
	ctx_fun_library : string -> 'value open_context -> 'value list -> 'value;
	ctx_scope : namespace_scope;
	ctx_lookup_by_id : string -> XSet.t;

	(* Note: character encoding is taken from ctx_node *)
      }

  and axis =
      [ `Child
      | `Child_rev             (* Child axis in reverse order *)
      | `Descendant
      | `Descendant_rev        (* Descendant axis in reverse order *)
      | `Parent
      | `Ancestor
      | `Following_sibling
      | `Preceding_sibling
      | `Following
      | `Preceding
      | `Attribute
      | `Namespace
      | `Self
      | `Descendant_or_self
      | `Descendant_or_self_rev (* Descendant-or-self axis in reverse order *)
      | `Ancestor_or_self
      ]

  and node_test =
      [ `Principal           (* e.g. child::* *)
      | `Ns_name of string   (* Contains only the normprefix *)
      | `Name of string      (* Full normalized name *)
      | `Comment             (* comment() *)
      | `Text                (* text() *)
      | `PI                  (* processing-instruction() *)
      | `PI_name of string   (* processing-instruction(name) *)
      | `Any                 (* node() *)
      ]

  and 'expr open_expr =
      [ `String of string              (* String literal *)
      | `Float of float                (* Floating-point literal *)
      | `Int of int                    (* Integer literal (convenience) *)
      | `Call of (string * 'expr list) (* Function call: name, args *)
      | `Or of 'expr list              (* Boolean "or" *)
      | `And of 'expr list             (* Boolean "and" *)
      | `Cmp of (cmp * 'expr * 'expr)  (* Comparison *)
      | `Add of 'expr * 'expr          (* Addition *)
      | `Sub of 'expr * 'expr          (* Subtraction *)
      | `Mul of 'expr * 'expr          (* Multiplication *)
      | `Div of 'expr * 'expr          (* Division *)
      | `Mod of 'expr * 'expr          (* Modulus *)
      | `Neg of 'expr                  (* Numeric negation *)
      | `Union of 'expr list           (* Union of node sets *)
      | `Root                          (* The location path "/" *)
      | `Path of 'expr * 'expr step list (* Take the expression as set of
					  * context nodes, and perform steps
					  * relative to it
					  *)
      | `Predicate of 'expr * 'expr    (* 2nd expr is a predicate *)
      | `Current                       (* The ctx_node *)
      | `Var of string                 (* Variable reference *)
      ]
  (* Missing:
   * - `Literal generic literal
   * - `Assert_string
   * - `Assert_number
   * - `Assert_boolean
   * - `Assert_nodeset
   *
   * - `Store: Put intermediate result into a variable
   *
   * Typechecking!  --> Type-annotated expr
   *)


  (* Ideas for optimisations:
   * - Creation of attribute and namespace nodes is expensive. Try to
   *   avoid that:
   *    + Node sets consisting _only_ of attributes or _only_ of namespaces
   *      could have a different representation
   *      ==> `Nodeset_attopt, `Nodeset_nsopt
   *      ==> new axes `Attribute_opt, `Namespace_opt creating these sets
   *      ==> It is forbidden to mix an optimised set with another set
   *)

  and expr = ('expr open_expr as 'expr)

  and cmp =
      [ `Eq            (* Test "=" *)
      | `Neq           (* Test "!=" *)
      | `Lt            (* Test "<" *)
      | `Le            (* Test "<=" *)
      | `Gt            (* Test ">" *)
      | `Ge            (* Test ">=" *)
      ] 

  and 'expr step =
      ( axis * node_test * (* predicates: *) 'expr list )


  let string_value n =
    n # data    (* CHECK *)
      

  let rec fold_axis axis (f : XNode.t -> 'a -> 'a) (n : XNode.t) (acc0 : 'a) : 'a =
    (* Call [f] in axis order relative to start node [n], and
     * accumulate.
     *
     * Axis order is forward or reverse document order, depending
     * on the natural direction of the axis.
     *)
    match axis with
      | `Child ->
	  (* In the PXP model and in the XPath model nodes have the
	   * same children, so we can simply iterate using iter_nodes.
	   *)
	  (* This iteration is in document order: *)
	  let acc = ref acc0 in
	  n # iter_nodes
	    (fun n' -> acc := f n' !acc);
	  !acc
      | `Child_rev ->
	  (* This iteration is in reverse document order: *)
	  let acc = ref acc0 in
	  let nodes = List.rev (n # sub_nodes) in
	  List.iter
	    (fun n' -> acc := f n' !acc)
	    nodes;
	  !acc
      | `Descendant ->
	  (* Reduce this to a `Child folding: *)
	  (* This iteration is in document order: *)
	  ( fold_axis 
	      `Child 
	      (fun n' acc ->
		 let acc' = f n' acc in
		 fold_axis `Descendant f n' acc'
	      )
	      n
	      acc0 )
      | `Descendant_rev ->
	  (* Reduce this to a `Child_rev folding: *)
	  (* This iteration is in reverse document order: *)
	  ( fold_axis 
	      `Child_rev
	      (fun n' acc ->
		 let acc' = fold_axis `Descendant_rev f n' acc in
		 f n' acc'
	      )
	      n
	      acc0 )
      | `Parent ->
	  (* In the PXP model and in the XPath model nodes have the
	   * same parents, so we can simply use the [parent] method here.
	   *)
	  (* This iteration is in reverse document order: *)
	  let p_opt = try Some(n # parent) with Not_found -> None in
	  ( match p_opt with
		Some p -> f p acc0
	      | None -> (* This is the root *) acc0
	  )
      | `Ancestor ->
	  (* This iteration is in reverse document order: *)
	  let p_opt = try Some(n # parent) with Not_found -> None in
	  ( match p_opt with
		Some p ->
		  fold_axis `Ancestor f p (f p acc0)
	      | None -> (* This is the root *) acc0
	  )
      | `Following_sibling ->
	  (* In the PXP model, attributes and namespaces may have
	   * siblings, but in the XPath model, they do not have them.
	   *)
	  (* This iteration is in document order: *)
	  ( match n#node_type with
		T_attribute _ | T_namespace _ -> acc0
	      | _ ->
		  let rec follow n' acc =
		    match 
		      ( try Some(n' # next_node) with Not_found -> None) 
		    with
			Some n'' ->
			  follow n'' (f n'' acc)
		      | None ->
			  acc
		  in
		  follow n acc0
	  )
      | `Preceding_sibling ->
	  (* In the PXP model, attributes and namespaces may have
	   * siblings, but in the XPath model, they do not have them.
	   *)
	  (* This iteration is in reverse document order: *)
	  ( match n#node_type with
		T_attribute _ | T_namespace _ -> acc0
	      | _ ->
		  let rec follow n' acc =
		    match 
		      ( try Some(n' # previous_node) with Not_found -> None) 
		    with
			Some n'' ->
			  follow n'' (f n'' acc)
		      | None ->
			  acc
		  in
		  follow n acc0
	  )
      | `Following ->
	  (* Reduce to `Following_sibling, `Descendant, `Parent *)
	  (* This iteration is in document order: *)
	  let acc'' =
	    (* First the direct following siblings plus descendants: *)
	    fold_axis
	      `Following_sibling
	      (fun n' acc ->
		 let acc' = f n' acc in
		 fold_axis `Descendant f n' acc'
	      )
	      n
	      acc0 in
	  (* Continue with the following nodes of the parent: *)
	  fold_axis
	    `Parent
	    (fun n' acc ->
	       fold_axis `Following f n' acc
	    )
	    n
	    acc''
      | `Preceding ->
	  (* Reduce to `Preceding_sibling, `Descendant_rev, `Parent *)
	  (* This iteration is in reverse document order: *)
	  let acc'' =
	    fold_axis
	      `Preceding_sibling
	      (fun n' acc ->
		 let acc' = fold_axis `Descendant_rev f n' acc in
		 f n' acc'
	      )
	      n
	      acc0 in
	  (* Continue with the preceding nodes of the parent: *)
	  fold_axis
	    `Parent
	    (fun n' acc ->
	       fold_axis `Preceding f n' acc
	    )
	    n
	    acc''
      | `Attribute ->
	  let atts = n # attributes_as_nodes in
	  List.fold_left (fun acc n' -> f n' acc) acc0 atts 
      | `Namespace ->
	  let nsnodes = n # namespaces_as_nodes in
	  List.fold_left (fun acc n' -> f n' acc) acc0 nsnodes
      | `Self ->
	  f n acc0
      | `Descendant_or_self ->
	  (* This iteration is in document order: *)
	  fold_axis `Descendant f n (f n acc0)
      | `Descendant_or_self_rev ->
	  (* This iteration is in reverse document order: *)
	  f n (fold_axis `Descendant_rev f n acc0)
      | `Ancestor_or_self ->
	  (* This iteration is in reverse document order: *)
	  fold_axis `Ancestor f n (f n acc0)
  ;;


  let eval_open_expr 
        (eval_sub_expr : 'value open_context -> 'expr -> 'value)
        (ctx : 'value open_context)
        (expr : 'expr open_expr) : 'value =
    
    let dest_boolean =
      function 
	  `Boolean (b : bool) -> b
	| _ -> failwith "Boolean expected" in

    let dest_string =
      function 
	  `String (b : string) -> b
	| _ -> failwith "String expected" in

    let dest_number =
      function 
	  `Number (b : float) -> b
	| _ -> failwith "Number expected" in

    let dest_nodeset =
      function 
	  `Nodeset b -> b
	| _ -> failwith "Node set expected" in

    let f_boolean arg = 
      dest_boolean (ctx.ctx_fun_library "boolean" ctx [arg]) in

    let f_string arg = 
      dest_string (ctx.ctx_fun_library "string" ctx [arg]) in

    let f_number arg = 
      dest_number (ctx.ctx_fun_library "number" ctx [arg]) in

    let cmp op s1 s2 =
      match op with
	  `Eq -> s1 = s2
	| `Neq -> s1 <> s2
	| `Lt -> s1 < s2
	| `Le -> s1 <= s2
	| `Gt -> s1 > s2
	| `Ge -> s1 >= s2
    in

    let rec follow_path (ctx : 'value open_context)
                        (s : XSet.t)
                        (l : 'expr step list) : XSet.t =
      (* Follow path [l] beginning at the nodes in [s] *)
      XSet.fold
	(fun n acc ->
	   let ctx' = { ctx with ctx_node = n } in
	   (* CHECK: position, size *)
	   let s' = follow_steps ctx' l in
	   XSet.union acc s'
	)
	s
	XSet.empty

    and follow_steps ctx (l : 'expr step list) : XSet.t =
      (* Follow path [l] beginning at the node [ctx.ctx_node] *)
      match l with
	  [] -> XSet.singleton ctx.ctx_node
	| (axis, nt, p) :: l' ->
	    let n = ctx.ctx_node in
 	    if p = [] then (
	      (* Without predicates, we can do an optimization *)
	      follow_path
		ctx
		(fold_axis
		   axis
		   (fun n' acc ->
		      if eval_node_test axis n' nt then
			XSet.add n' acc
		      else
			acc
		   )
		   n
		   XSet.empty
		)
		l'
	    )
	    else (
	      (* First collect the elements of the axis in a _list_ *)
	      let size1, nodes1 =
		fold_axis 
		  axis
		  (fun n' (counter,acc) ->
		     if eval_node_test axis n' nt then
		       (counter+1), ((counter,n') :: acc)
		     else
		       counter, acc
		  )
		  n
		  (0, []) in
	      
	      (* Iterate through the predicates, and apply them: *)
	      let final_size, final_nodes =
		List.fold_left
		  (fun (size,nodes) pred_expr ->
		     let nodes' =
		       List.filter
			 (fun (pos,n') ->
			    let ctx' = { ctx with
					   ctx_node = n';
					   ctx_position = pos + 1;
					   ctx_size = size } in
			    let r = eval_sub_expr ctx' pred_expr in
			    match r with
				`Number rn -> truncate rn = pos+1 (* CHECK *)
			      | _          -> f_boolean r
			 )
			 nodes in
		     (* The positions are wrong in nodes', and we don't
		      * know the new size yet. So do another iteration:
		      *)
		     List.fold_left
		       (fun (counter,acc) (_,n') ->
			  (counter+1), ((counter,n') :: acc)
		       )
		       (0, [])
		       nodes'
		  )
		  (size1, nodes1)
		  p
	      in

	      (* Create again a nodeset: *)
	      List.fold_left
		(fun acc (_,n') -> XSet.add n' acc)
		XSet.empty
		final_nodes
	    )

    and eval_node_test axis n =
      function
	  `Principal ->
	    ( match n # node_type with
		  T_attribute _ ->
		    axis = `Attribute
		| T_namespace _ ->
		    axis = `Namespace
		| T_element _ ->
		    axis <> `Attribute && axis <> `Namespace
		| _ ->
		    false
	    )
	| `Ns_name normprefix ->
	    ( match n # node_type with
		  T_attribute _ ->
		    axis = `Attribute && n # normprefix = normprefix
		| T_namespace _ ->
		    axis = `Namespace && n # normprefix = normprefix
		| _ ->
		    axis <> `Attribute && axis <> `Namespace &&
		    n # normprefix = normprefix
	    )
	| `Name name     ->
	    ( match axis with
		  `Attribute ->
		    (* Principal node type is "attribute" *)
		    n # node_type = T_attribute name
		| `Namespace ->
		    (* Principal node type is "namespace" *)
		    n # node_type = T_namespace name (* CHECK *)
		| _ ->
		    (* Principal node type is "element" *)
		    n # node_type = T_element name
	    )
	| `Comment       -> n # node_type = T_comment
	| `Text          -> n # node_type = T_data
	| `PI            -> ( match n # node_type with 
				  T_pinstr _ -> true
				| _ -> false)
	| `PI_name name  -> n # node_type = T_pinstr name
	| `Any           -> true

    in

    match expr with
	`String s -> 
	  `String s
      | `Float n -> 
	  `Number n
      | `Int n -> 
	  `Number(float_of_int n)
      | `Call(name,arg_exprs) ->
	  let f = ctx.ctx_fun_library name in
	  let args = List.map (eval_sub_expr ctx) arg_exprs in
	  f ctx args
      | `Or l ->
	  `Boolean 
	    ( List.fold_left
		(fun acc expr ->
		   acc || f_boolean(eval_sub_expr ctx expr))
		false
		l 
	    )
      | `And l ->
	  `Boolean 
	    ( List.fold_left
		(fun acc expr ->
		   acc && f_boolean(eval_sub_expr ctx expr))
		true
		l 
	    )
      | `Cmp(op,expr1,expr2) ->
	  let x1 = eval_sub_expr ctx expr1 in
	  let x2 = eval_sub_expr ctx expr2 in
	  ( match (op,x1,x2) with
		(_, `Nodeset s1, `Nodeset s2) ->
		  `Boolean(XSet.exists
			     (fun e1 ->
				XSet.exists
				  (fun e2 ->
				     cmp op (string_value e1) (string_value e2)
				  )
				  s2
			     )
			     s1)
	      | (_, `Nodeset s1, `Number n2) ->
		  `Boolean(XSet.exists
			     (fun e1 ->
				cmp 
				  op 
				  (f_number (`String (string_value e1)))
				  n2
			     )
			     s1)
	      | (_, `Number n1, `Nodeset s2) ->
		  `Boolean(XSet.exists
			     (fun e2 ->
				cmp 
				  op 
				  n1
				  (f_number (`String (string_value e2)))
			     )
			     s2)
	      | (_, `Nodeset s1, `String t2) ->
		  `Boolean(XSet.exists
			     (fun e1 -> cmp op (string_value e1) t2)
			     s1)
	      | (_, `String t1, `Nodeset s2) ->
		  `Boolean(XSet.exists
			     (fun e2 -> cmp op t1 (string_value e2))
			     s2)
	      | (_, `Nodeset s1, `Boolean b2) ->
		  `Boolean(cmp op (f_boolean x1) b2)
	      | (_, `Boolean b1, `Nodeset s2) ->
		  `Boolean(cmp op b1 (f_boolean x2))
	      | ((`Eq|`Neq), `Boolean b1, _) ->
		  `Boolean(cmp op b1 (f_boolean x2))
	      | ((`Eq|`Neq), _, `Boolean b2) ->
		  `Boolean(cmp op (f_boolean x1) b2)
	      | ((`Eq|`Neq), `Number n1, _) ->
		  `Boolean(cmp op n1 (f_number x2))
	      | ((`Eq|`Neq), _, `Number n2) ->
		  `Boolean(cmp op (f_number x1) n2)
	      | ((`Eq|`Neq), _, _) ->
		  `Boolean(cmp op (f_string x1) (f_string x2))
	      | (_, _, _) ->
		  `Boolean(cmp op (f_number x1) (f_number x2))
	  )
      | `Add(expr1,expr2) ->
	  let x1 = eval_sub_expr ctx expr1 in
	  let x2 = eval_sub_expr ctx expr2 in
	  `Number(f_number x1 +. f_number x2)
      | `Sub(expr1,expr2) ->
	  let x1 = eval_sub_expr ctx expr1 in
	  let x2 = eval_sub_expr ctx expr2 in
	  `Number(f_number x1 -. f_number x2)
      | `Mul(expr1,expr2) ->
	  let x1 = eval_sub_expr ctx expr1 in
	  let x2 = eval_sub_expr ctx expr2 in
	  `Number(f_number x1 *. f_number x2)
      | `Div(expr1,expr2) ->
	  let x1 = eval_sub_expr ctx expr1 in
	  let x2 = eval_sub_expr ctx expr2 in
	  `Number(f_number x1 /. f_number x2)
      | `Mod(expr1,expr2) ->
	  let x1 = eval_sub_expr ctx expr1 in
	  let x2 = eval_sub_expr ctx expr2 in
	  `Number(
	    float(truncate(f_number x1) mod truncate(f_number x2)))
      | `Neg expr ->
	  let x = eval_sub_expr ctx expr in
	  `Number(-. (f_number x))
      | `Union l ->
	  `Nodeset
	    (List.fold_left
	       (fun acc expr ->
		  let x = eval_sub_expr ctx expr in
		  match x with
		      `Nodeset s ->
			XSet.union acc s
		    | _ ->
			failwith "Arguments of 'union' must be node sets"
	       )
	       XSet.empty
	       l
	    )
      | `Root ->
	  (* This is a bit tricky:
	   * - If the root node is of type T_super_root, we take this node
	   * - Otherwise, we cannot return the node, and fail. However,
	   *   there is the special case that `Root occurs inside `Path,
	   *   see below. This case can be handled.
	   *)
	  let root = ctx.ctx_node # root in
	  ( match root # node_type with
		T_super_root ->
		  `Nodeset(XSet.singleton root)
	      | _ ->
		  failwith "Cannot select root node as such if it is not of type T_super_root"
	  )
      | `Path(`Root,l) when l <> [] ->
	  let root = ctx.ctx_node # root in
	  ( match root # node_type with
		T_super_root ->
		  (* Simple case *)
		  `Nodeset(follow_path ctx (XSet.singleton root) l)
	      | _ ->
		  (* Maybe we can handle this! *)
		  ( match l with
			(* These work: *)
			((`Child|`Child_rev), nt, p) :: l' ->
			  (* The only child of the non-existing super root is
			   * the real root:
			   *)
			  `Nodeset (follow_path 
				      ctx 
				      (XSet.singleton root)
				      ((`Self, nt, p) :: l'))
		      | (`Descendant, nt, p) :: l' ->
			  (* The descendants of the non-existing super root
			   * are the descendants of the real root plus self: 
			   *)
			  `Nodeset (follow_path 
				      ctx 
				      (XSet.singleton root)
				      ((`Descendant_or_self, nt, p) :: l'))
		      | (`Descendant_rev, nt, p) :: l' ->
			  `Nodeset (follow_path 
				      ctx 
				      (XSet.singleton root)
				      ((`Descendant_or_self_rev, nt, p) :: l'))
		      | (`Parent, _, _) :: _ ->
			  (* The non-existing super root node does not have
			   * parents:
			   *)
			  `Nodeset(XSet.empty)
		      | (`Ancestor, _, _) :: _ ->
			  (* Same for ancestors: *)
			  `Nodeset(XSet.empty)
		      | (`Following_sibling, _, _) :: _ ->
			  (* Same for following siblings: *)
			  `Nodeset(XSet.empty)
		      | (`Preceding_sibling, _, _) :: _ ->
			  (* Same for preceding siblings: *)
			  `Nodeset(XSet.empty)
		      | (`Following, nt, p) :: l' ->
			  (* The following nodes of the non-existing super root
			   * are the descendants of the real root: 
			   *)
			  `Nodeset(follow_path 
				     ctx 
				     (XSet.singleton root)
				     ((`Descendant, nt, p) :: l'))
		      | (`Preceding, _, _) :: _ ->
			  (* The non-existing super root node does not have
			   * preceding nodes:
			   *)
			  `Nodeset(XSet.empty)
		      | (`Attribute, _, _) :: _ ->
			  (* A super root node never has attributes: *)
			  `Nodeset(XSet.empty)
		      | (`Namespace, _, _) :: _ ->
			  (* A super root node never has namespace nodes: *)
			  `Nodeset(XSet.empty)

			(* These do not work: *)
		      | ((`Self|`Descendant_or_self|`Ancestor_or_self|`Descendant_or_self_rev),_,_)::_
		      | [] ->
			  failwith "Cannot select root node as such if it is not of type T_super_root"
		  )
	  )	  
      | `Path(expr,l) ->
	  let x = eval_sub_expr ctx expr in
	  `Nodeset(follow_path ctx (dest_nodeset x) l)
      | `Predicate(expr1,expr2) ->
	  let x1 = eval_sub_expr ctx expr1 in
	  ( match x1 with
		`Nodeset s1 ->
		  (* Output the nodes in document order: *)
		  let l1 = XSet.elements s1 in
		  let size = List.length l1 in
		  let s2 = ref XSet.empty in
		  let pos = ref 0 in
		  List.iter
		    (fun n' ->
		       let selected =
		       	 let ctx' = { ctx with
					ctx_node = n';
					ctx_position = !pos + 1;
					ctx_size = size } in
			 let r = eval_sub_expr ctx' expr2 in
			 match r with
			     `Number rn -> truncate rn = !pos+1  (* CHECK *)
			   | _          -> f_boolean r
		       in
		       if selected then s2 := XSet.add n' !s2;
		       incr pos;
		    )
		    l1;
		  `Nodeset !s2
	      | _ ->
		  failwith "Only node sets can be filtered"
	  )
      | `Var name ->
	  ctx.ctx_var_binding name
      | `Current ->
	  `Nodeset(XSet.singleton(ctx.ctx_node))
  ;;

  let rec eval_expr ctx (expr : expr) =
    eval_open_expr eval_expr ctx expr


  (**********************************************************************)
  (* XPath Library:                                                     *)
  (**********************************************************************)

  exception Function_type_mismatch of string

  exception Function_not_defined of string


  let f_last lib ctx args =
    if args <> [] then raise(Function_type_mismatch "last");
    `Number(float_of_int(ctx.ctx_size))

  let f_position lib ctx args =
    if args <> [] then raise(Function_type_mismatch "position");
    `Number(float_of_int(ctx.ctx_position))

  let f_count lib ctx args =
    match args with
	[ `Nodeset s ] -> `Number(float_of_int(XSet.cardinal s))
      | _ ->  raise(Function_type_mismatch "count")

  let ws_re =
    Netstring_pcre.regexp "[ \t\r\n]+"


  let f_id lib =
    let f_string = lazy(lib "string") in

    fun ctx args ->

      let get_id_nodes id_str : XSet.t =
	let words = Netstring_pcre.split ws_re id_str in
	List.fold_left
	  (fun acc id ->
	     XSet.union (ctx.ctx_lookup_by_id id) acc)
	  XSet.empty
	  words
      in

      match args with
	  [ `Nodeset s ] ->
	    `Nodeset(
	      XSet.fold (fun node acc ->
			   let id_str = string_value node in
			   XSet.union acc (get_id_nodes id_str)
			)
		       s
		       XSet.empty)
	| [ arg1 ] ->
	    let arg1' = (Lazy.force f_string) ctx [ arg1 ] in
	    ( match arg1' with
		  `String arg1_str ->
		    `Nodeset(get_id_nodes arg1_str)
		| _ -> assert false
	    )
	    
	| _ -> raise(Function_type_mismatch "id")
	    

  let f_local_name lib ctx args =
    let set = 
      match args with
	  [ `Nodeset s ] -> s
      | [] -> XSet.singleton ctx.ctx_node
      | _ -> raise(Function_type_mismatch "local-name")
    in
    try
      let first_node = XSet.min_elt set in  (* or Not_found *)
      `String(first_node # localname)
	(* (Namespace_)Method_not_applicable *)
    with
	Not_found 
      | Method_not_applicable _
      | Namespace_method_not_applicable _ ->
	  `String ""

  let f_namespace_uri lib ctx args =
    let set = 
      match args with
	  [ `Nodeset s ] -> s
      | [] -> XSet.singleton ctx.ctx_node
      | _ -> raise(Function_type_mismatch "namespace-uri")
    in
    try
      let first_node = XSet.min_elt set in  (* or Not_found *)
      `String(first_node # namespace_uri)
	(* (Namespace_)Method_not_applicable *)
    with
	Not_found 
      | Method_not_applicable _
      | Namespace_method_not_applicable _ ->
	  `String ""

  let f_name ~prefer_display_prefix lib ctx args =
    let set = 
      match args with
	  [ `Nodeset s ] -> s
      | [] -> XSet.singleton ctx.ctx_node
      | _ -> raise(Function_type_mismatch "name")
    in
    try
      let first_node = XSet.min_elt set in  (* or Not_found *)
      let prefix =
	if prefer_display_prefix then
	  first_node # display_prefix
	else
	  first_node # normprefix in
      let localname = first_node # localname in
      `String(if prefix = "" then localname else prefix ^ ":" ^ localname)
    with
	Not_found 
      | Method_not_applicable _
      | Namespace_method_not_applicable _ ->
	  `String ""

  let rec f_string lib ctx args =
    match args with
	[ `Nodeset s ] ->
	  ( try 
	      let first_node = XSet.min_elt s in  (* or Not_found *)
	      `String(string_value first_node) 
	    with
		Not_found -> `String "" 
	  )
      | [ `Number f ] ->
	  ( match classify_float f with
		FP_normal 
	      | FP_subnormal ->
		  (* Is [f] integral? We use the criterion
		   * floor f = f, but this is bogus for floats bigger than
		   * the maximum representable integer
		   *)
		  if floor f = f then
		    `String(Printf.sprintf "%1.0f" f)
		  else
		    let s = Printf.sprintf "%1.1F" f in
		    if s<>"" && s.[ String.length s - 1] = '.' then
		      `String(s ^ "0")  (* Should not happen, just to be sure *)
		    else
		      `String s
	      | FP_zero -> 
		  `String "0"
	      | FP_infinite -> 
		  `String(if f > 0.0 then "Infinity" else "-Infinity")
	      | FP_nan ->
		  `String "NaN"
	  )
      | [ `Boolean false ] -> `String "false"
      | [ `Boolean true ] -> `String "true"
      | [ `String s as arg ] -> arg
      | [] -> f_string lib ctx [ `Nodeset(XSet.singleton ctx.ctx_node) ]
      | _ -> raise(Function_type_mismatch "string")

  let f_concat lib ctx args =
    `String
      (String.concat ""
	 (List.map
	    (function 
		 `String s -> s
	       | _ -> raise(Function_type_mismatch "concat")
	    )
	    args))

  let f_starts_with lib ctx args =
    match args with
	[ `String s1; `String s2 ] ->
	  let l1 = String.length s1 in
	  let l2 = String.length s2 in
	  `Boolean(l1 >= l2 && String.sub s1 0 l2 = s2)
      | _ -> raise(Function_type_mismatch "starts-with")

  let rec contains_at k s1 s2 =
    (* Only called for s2 <> "" *)
    if k <= String.length s1 - String.length s2 then
      ( (s1.[k] = s2.[0]) &&
	(String.sub s1 k (String.length s2) = s2) ) || contains_at (k+1) s1 s2
      (* TODO: String.sub may be quite slow, esp. for large strings *)
    else
      false

  let f_contains lib ctx args =
    match args with
	[ `String s1; `String s2 ] ->
	  `Boolean(s2 = "" || contains_at 0 s1 s2)
      | _ -> raise(Function_type_mismatch "contains")

  let rec substr_before_at k s1 s2 =
    (* Only called for s2 <> "" *)
    if k <= String.length s1 - String.length s2 then (
      (* TODO: String.sub may be quite slow, esp. for large strings *)
      if ( (s1.[k] = s2.[0]) &&
	   (String.sub s1 k (String.length s2) = s2) ) then 
	String.sub s1 0 k
      else
	substr_before_at (k+1) s1 s2
    )
    else
      ""

  let f_substring_before lib ctx args =
    match args with
	[ `String s1; `String s2 ] ->
	  if s2 = "" then
	    `String ""
	  else
	    `String(substr_before_at 0 s1 s2)
      | _ -> raise(Function_type_mismatch "substring-before")


  let rec substr_after_at k s1 s2 =
    (* Only called for s2 <> "" *)
    if k <= String.length s1 - String.length s2 then (
      (* TODO: String.sub may be quite slow, esp. for large strings *)
      if ( (s1.[k] = s2.[0]) &&
	   (String.sub s1 k (String.length s2) = s2) ) then 
	let p = k+String.length s2 in
	String.sub s1 p (String.length s1 - p)
      else
	substr_after_at (k+1) s1 s2
    )
    else
      ""

  let f_substring_after lib ctx args =
    match args with
	[ `String s1; `String s2 ] ->
	  if s2 = "" then
	    `String ""
	  else
	    `String(substr_after_at 0 s1 s2)
      | _ -> raise(Function_type_mismatch "substring-after")


  exception Nan

  let f_substring lib =
    let f_round = lazy(lib "round") in

    fun ctx args ->

      let enc = (ctx.ctx_node # encoding :> Netconversion.encoding) in
      let to_int f =
	match classify_float f with
	    FP_nan -> raise Nan
	  | FP_infinite -> if f < 0.0 then min_int else max_int
	  | _ -> int_of_float f in
      let round f =
	match (Lazy.force f_round) ctx [ `Number f ] with
	    `Number f' -> f'
	  | _ -> assert false in
      try
	let s, l, n_start, n_end =
	  match args with
	      [ `String s; `Number n1 ] ->
		let l = Netconversion.ustring_length enc s in
		s, l, to_int(round n1) - 1, l
	    | [ `String s; `Number n1; `Number n2 ] ->
		let l = Netconversion.ustring_length enc s in
		s, l, to_int(round n1) - 1, to_int(round n1 +. round n2) - 1
	    | _ -> raise(Function_type_mismatch "substring")
	in
	let n_start = min (max n_start 0) l in
	let n_end   = min (max n_end 0) l in
	let s' = Netconversion.ustring_sub enc n_start (n_end - n_start) s in
	`String s'
      with
	  Nan -> `String ""

  let f_string_length lib ctx args =
    let enc = (ctx.ctx_node # encoding :> Netconversion.encoding) in
    let s = 
    match args with
	[ `String s ] -> s
      | [] -> string_value ctx.ctx_node
      | _ ->  raise(Function_type_mismatch "string-length")
    in
    `Number(float_of_int(Netconversion.ustring_length enc s))

  let f_normalize_space lib ctx args =
    let enc = ctx.ctx_node # encoding in
    let s = 
    match args with
	[ `String s ] -> s
      | [] -> string_value ctx.ctx_node
      | _ ->  raise(Function_type_mismatch "normalize-space")
    in
    let words = Netstring_pcre.split ws_re s in
    `String(String.concat " " words)

  let f_translate lib ctx args =
    let enc = (ctx.ctx_node # encoding :> Netconversion.encoding) in
    match args with
	[ `String s; `String pat; `String repl ] ->
	  let subst = Hashtbl.create 10 in
	  let pat_a = Netconversion.uarray_of_ustring enc pat in
	  let repl_a = Netconversion.uarray_of_ustring enc repl in
	  for k = 0 to Array.length pat_a - 1 do
	    let p_char = pat_a.(k) in
	    let r_char_list =
	      if k < Array.length repl_a then
		[ repl_a.(k) ]
	      else
		[] in
	    if not(Hashtbl.mem subst p_char) then
	      Hashtbl.add subst p_char r_char_list
	  done;
	  let s' =
	    Netconversion.ustring_map 
	      enc 
	      (fun char -> 
		 try Hashtbl.find subst char with Not_found -> [char])
	      s in
	  `String s'
      | _ -> raise(Function_type_mismatch "translate")

  let f_boolean lib ctx args =
    match args with
	[ `Number f ] ->
	  `Boolean
	  ( match classify_float f with
		FP_zero | FP_nan -> false
	      | _ -> true
	  )
      | [ `Nodeset s ] ->
	  `Boolean(s <> XSet.empty)
      | [ `String s ] ->
	  `Boolean(s <> "")
      | [ `Boolean _ as b ] -> b
      | _ -> raise(Function_type_mismatch "boolean")

  let f_not lib ctx args =
    match args with
	[ `Boolean b ] -> `Boolean(not b)
      | _ -> raise(Function_type_mismatch "not")

  let f_true lib ctx args =
    if args <> [] then raise(Function_type_mismatch "true");
    `Boolean true
	    
  let f_false lib ctx args =
    if args <> [] then raise(Function_type_mismatch "false");
    `Boolean false

  let rec find_xml_lang n =
    match n # optional_string_attribute "xml:lang" with
	Some v -> v
      | None   -> find_xml_lang n#parent (* or Not_found *)


  let lang_sufx_re =
    Netstring_pcre.regexp "-.*$"

  let f_lang lib ctx args =
    match args with
	[ `String s ] ->
	  ( try
	      let lang = find_xml_lang ctx.ctx_node in (* or Not_found *)
	      let lang' = Netstring_pcre.replace_first lang_sufx_re "" lang in
	      `Boolean(String.lowercase s = String.lowercase lang')
	      (* BUG: String.lowercase assumes ISO-8859-1. This may be
	       * wrong. However, we don't have a Unicode-aware function
	       * for case-insensitive comparisons.
	       * Implications are limited, as language codes are usually ASCII.
	       *)
	    with
		Not_found -> `Boolean false
	  )
      | _ -> raise(Function_type_mismatch "lang")

  let rec f_number lib =
    let f_string = lazy(lib "string") in

    fun ctx args ->
      match args with
	  [ `String s ] ->
	    (* TODO: Check syntax of s with regexp *)
	    `Number(float_of_string s)
	| [ `Nodeset _ as arg ] ->
	    let arg' = (Lazy.force f_string) ctx [ arg ] in
	    f_number lib ctx [arg']
	| [ `Boolean b ] ->
	    `Number(if b then 1.0 else 0.0)
	| [ `Number _ as n ] -> n
	| [] ->
	    let arg = `Nodeset(XSet.singleton ctx.ctx_node) in
	    f_number lib ctx [ arg ]
	| _ -> raise(Function_type_mismatch "number")
	    
  let f_sum lib =
    let f_number = lazy(lib "number") in

    fun ctx args ->
      let number s =
	match (Lazy.force f_number) ctx [ `String s ] with
	    `Number f' -> f'
	  | _ -> assert false in
      match args with
	  [ `Nodeset s ] ->
	    `Number(
	      XSet.fold (fun n acc ->
			   acc +. number(string_value n)) s 0.0)
	| _ ->  raise(Function_type_mismatch "sum")

  let f_floor lib ctx args =
    match args with
	[ `Number f ] -> `Number(floor f)
      | _ ->  raise(Function_type_mismatch "floor")

  let f_ceiling lib ctx args =
    match args with
	[ `Number f ] -> `Number(ceil f)
      | _ ->  raise(Function_type_mismatch "ceiling")

  let f_round lib ctx args =
    match args with
	[ `Number f as n ] -> 
	  ( match classify_float f with
		FP_zero -> n
	      | FP_infinite -> n
	      | FP_nan -> n
	      | _ ->
		  if f < 0.0 && f >= (-0.5) then
		    `Number(-. 0.0)
		  else
		    `Number(floor(f +. 0.5))
	  )
      | _ ->  raise(Function_type_mismatch "round")

  let lookup h name =
    try
      Hashtbl.find h name
    with
	Not_found -> raise(Function_not_defined name)


  let mk_std_library ?(prefer_display_prefix = true) () =
    let h = Hashtbl.create 30 in
    let lib = Hashtbl.find h in
    Hashtbl.add h "last" (f_last lib);
    Hashtbl.add h "position" (f_position lib);
    Hashtbl.add h "count" (f_count lib);
    Hashtbl.add h "id" (f_id lib);
    Hashtbl.add h "local-name" (f_local_name lib);
    Hashtbl.add h "namespace-uri" (f_namespace_uri lib);
    Hashtbl.add h "name" (f_name ~prefer_display_prefix lib);
    Hashtbl.add h "string" (f_string lib);
    Hashtbl.add h "concat" (f_concat lib);
    Hashtbl.add h "starts-with" (f_starts_with lib);
    Hashtbl.add h "contains" (f_contains lib);
    Hashtbl.add h "substring-before" (f_substring_before lib);
    Hashtbl.add h "substring-after" (f_substring_after lib);
    Hashtbl.add h "substring" (f_substring lib);
    Hashtbl.add h "string-length" (f_string_length lib);
    Hashtbl.add h "normalize-space" (f_normalize_space lib);
    Hashtbl.add h "translate" (f_translate lib);
    Hashtbl.add h "boolean" (f_boolean lib);
    Hashtbl.add h "not" (f_not lib);
    Hashtbl.add h "true" (f_true lib);
    Hashtbl.add h "false" (f_false lib);
    Hashtbl.add h "lang" (f_lang lib);
    Hashtbl.add h "number" (f_number lib); 
    Hashtbl.add h "sum" (f_sum lib);
    Hashtbl.add h "floor" (f_floor lib);
    Hashtbl.add h "ceiling" (f_ceiling lib);
    Hashtbl.add h "round" (f_round lib);
    h

  let mk_context ?scope h n =
    { ctx_node = n;
      ctx_position = 0;
      ctx_size = 0;
      ctx_var_binding = (fun _ -> assert false);   (* TODO *)
      ctx_fun_library = lookup h;
      ctx_scope = ( match scope with
			None -> n # namespace_scope
		      | Some s -> s);
      ctx_lookup_by_id = (fun _ -> assert false);  (* TODO *)
    }

	

end
