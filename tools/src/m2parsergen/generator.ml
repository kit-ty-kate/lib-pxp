(* $Id: generator.ml,v 1.4 2000/05/09 00:03:22 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Parser
open Ast

(* Overall scheme:
 *
 * The rules are translated to:
 *
 * let rec parse_<rule1> ... = ...
 *     and parse_<rule2> ... = ...
 *     and ...
 *     and parse_<ruleN> ... = ...
 * in
 *
 * Every rule has at least two arguments: 'current' and 'get_next'.
 * 'current()' is the token that should match the first symbol of the
 * rule. 'get_next()' returns the next token.
 *
 * The rules may have further user arguments; these are the next arguments
 * in turn.
 *
 * The rules return the user value. After they have returned to the caller 
 * the current token is the token that follows the sequence of tokens 
 * matching the rule.
 *
 * The rules will raise:
 *  - Not_found if the first token does not match
 *  - Parsing.Parse_error if the rest does not match.
 *
 * Rule scheme:
 *
 * rule(arg1,arg2,...):
 *   (l1:x1)
 *   {{ let-CODE }}
 *   (l2:y2(name1,...)) y3 ... 
 *   {{ CODE }}
 *   ? {{ ?-CODE }}
 * | x2 ...
 * | ...
 * | xN
 *
 * let parse_<rule> current get_next arg1 arg2 ... =
 *   match current() with
 *     S(x1) -> ...
 *   | S(x2) -> ...
 *   | ...
 *   | S(xN) -> ...
*    | _ -> raise Not_found
 *
 * Here, S(xi) denotes the set of tokens matched by xi without all tokens
 * already matched by x1 to x(i-1). (If S(xi) = empty, a warning is printed,
 * and this branch of the rule is omitted.)
 *
 * S(xi) may be a set because xi may be a reference to another rule. In this
 * case, S(xi) bases on the set of tokens that match the first symbol of 
 * the other rule. (In general, S(xi) must be computed recursively.)
 *
 * If the "?" clause is present, every branch is embraced by the following:
 *
 * let position = ref "<Label of x1>" in
 * ( try ... 
 *   with Parsing.Parse_error -> ( <<?-CODE>> )
 * )
 * 
 * Next: The "..." is
 *
 * OPTIONAL: let <l1> = parse_<rule(x1)> in
 * <<let-CODE>>
 * M(y1)
 * M(y2)
 * ...
 * M(yN)
 * <<CODE>>
 *
 * If x1 is a rule invocation, it is now parsed, and the result is bound
 * to a variable.
 *
 * Note: After x1 has matched, the Caml variable <l1> must be either
 * bound to the result of the sub parsing, or to the value associated
 * with the token (if any). The latter is already done in the main
 * "match" statement, i.e. "match ... with S(x1) -> ..." is actually
 * "match ... with Token1 <l1> -> ...".
 *
 * Note: After calling parse_<rule(x1)> the exception Not_found is NEVER
 * converted to Parsing.Parse_error. It is simply not possible that this
 * happens.

 * For every remaining symbol yi of the rule, a matching statement M(yi)
 * is produced. These statements have the form:
 *
 * OPTIONAL: position := "<Label of yi>";
 * CASE: yi is a token without associated value
 *     let yy_i = get_next()  OR  current() in
 *     if yy_i <> Token(yi) then raise Parsing.Parse_error;
 * CASE: yi is a token with value
 *     let yy_i = get_next()  OR  current() in
 *     let <li> = match yy_i with Token x -> x | _ -> raise Parsing.Parse_error 
 *     in
 * CASE: yi is a rule invocation
 *     OPTIONAL: let _ = get_next() in
 *     let <li> = try parse_<rule(yi)> 
 *                with Not_found -> raise Parsing.Parse_error in
 *
 * yy_i is get_next() if y(i-1) was a token, and yy_i is current() if
 * y(i-1) was a rule invocation.
 *
 * Repetitions:
 *
 * If yi = (yi')*:
 *
 * CASE no label given:
 *
 * ( try 
 *     while true do 
 *       M(yi') with the modification that top-level mismatches raise
 *              Not_found instead of Parsing.Parse_error
 *     done
 *   with Not_found -> ()
 * )
 *
 * CASE a label <li> is given: The list of results must be bound to <li>!
 *
 * let yy_list = ref [] in
 * ( try 
 *     while true do
 *       let yy_first = M(yi') (with some modifications) in
 *       yy_list := yy_first :: !yy_list;
 *     done
 *   with Not_found -> ()
 * );
 * let <li> = List.rev !yy_list in
 *
 * Note that this scheme minimizes stack and heap allocations.
 *
 * Options:
 *
 * If yi = (yi')?:
 *
 * CASE no label given:
 *
 * ( try 
 *     M(yi') with the modification that top-level mismatches raise
 *            Not_found instead of Parsing.Parse_error
 *   with Not_found -> ()
 * )
 *
 * CASE a label <li> is given: The optional result must be bound to <li>!
 *
 * let <li> =
 *   try 
 *     Some( M(yi') (with some modifications) )
 *   with Not_found -> None
 * );
 *)


let lookup_rule tree name =
  try
    List.find (fun r -> r.rule_name = name) tree.text_rules
  with
      Not_found ->
	failwith ("Rule `" ^ name ^ "' not found")
;;


let is_typed tree name =
  (* Find out whether the token 'name' is typed or not *)
  let decl =
    try
      List.find (fun d -> match d with
		     D_token n -> n = name
		   | D_typed_token n -> n = name
		)
	tree.text_decls
    with
	Not_found -> 
	  failwith ("Token `" ^ name ^ "' not found")
  in
  match decl with
      D_token _ -> false
    | D_typed_token _ -> true
;;


let label_of_symbol tree sym =
  match sym with
      U_symbol (tok, lab) -> 
	if is_typed tree tok then lab else None
    | L_symbol (_, _, lab) -> lab
    | L_indirect (_, _, lab) -> lab
;;


let rec set_of_list l =
  (* Removes duplicate members of l *)
  match l with
      [] -> []
    | x :: l' -> if List.mem x l' then set_of_list l' else x :: (set_of_list l')
;;


let selector_set_of_rule tree name =
  (* Determines the set of tokens that match the first symbol of a rule *)
  
  let rec collect visited_rules name =
    if List.mem name visited_rules then
      []
    else
      let r = lookup_rule tree name in
      List.flatten
	(List.map
	   (fun branch ->
	      match branch.branch_selector with
		  U_symbol (tok_name,_) ->
		    [ tok_name ]
		| L_symbol (rule_name, _, _) ->
		    collect (name :: visited_rules) rule_name
		| L_indirect (_, _, _) ->
		    failwith("The first symbol in rule `" ^ name ^ 
			     "' is an indirect call; this is not allowed")
	   )
	   r.rule_branches
	)
  in
  set_of_list (collect [] name)
;;


let output_code_location b file_name (_, line, column) = 
  Buffer.add_string b "\n";
  Buffer.add_string b ("# " ^ string_of_int line ^ " \"" ^
		       file_name ^ "\"\n");
  Buffer.add_string b (String.make column ' ')
;;


let output_code b file_name ((code, line, column) as triple) = 
  if code <> "" then begin
    output_code_location b file_name triple;
    Buffer.add_string b code
  end
;;


let process_branch b file_name tree branch =

  let make_rule_invocation called_rule args lab allow_not_found =
    (* Produces: let <label> = parse_<called_rule> ... args in 
     * If not allow_not_found, the exception Not_found is caught and
     * changed into Parsing.Parse_error.
     *)
    let r = lookup_rule tree called_rule in
    if List.length r.rule_arguments <> List.length args then
      failwith("Calling rule `" ^ called_rule ^ "' with the wrong number of arguments!");

    Buffer.add_string b "let ";
    begin match lab with
	None   -> Buffer.add_string b "_"
      | Some l -> Buffer.add_string b l
    end;
    Buffer.add_string b " = ";
    if not allow_not_found then
      Buffer.add_string b "try ";
    Buffer.add_string b "parse_";
    Buffer.add_string b called_rule;
    Buffer.add_string b " yy_current yy_get_next";
    List.iter
      (fun a -> Buffer.add_string b " ";
	        Buffer.add_string b a;
      )
      args;
    if not allow_not_found then
      Buffer.add_string b " with Not_found -> raise Parsing.Parse_error";
    Buffer.add_string b " in\n"
  in

  let make_indirect_rule_invocation ml_name args lab allow_not_found =
    (* Produces: let <label> = ml_name ... args in 
     * If not allow_not_found, the exception Not_found is caught and
     * changed into Parsing.Parse_error.
     *)
    Buffer.add_string b "let ";
    begin match lab with
	None   -> Buffer.add_string b "_"
      | Some l -> Buffer.add_string b l
    end;
    Buffer.add_string b " = ";
    if not allow_not_found then
      Buffer.add_string b "try ";
    Buffer.add_string b ml_name;
    Buffer.add_string b " yy_current yy_get_next";
    List.iter
      (fun a -> Buffer.add_string b " ";
	        Buffer.add_string b a;
      )
      args;
    if not allow_not_found then
      Buffer.add_string b " with Not_found -> raise Parsing.Parse_error";
    Buffer.add_string b " in\n"
  in

  let process_symbol sym previous_was_token allow_not_found =
    match sym with
	U_symbol(tok, lab) ->
	  (* Distinguish between simple tokens and typed tokens *)
	  if is_typed tree tok then begin
	    (* Typed token *)
	    Buffer.add_string b "let ";
	    begin match lab with
		None   -> Buffer.add_string b "_"
	      | Some l -> Buffer.add_string b l
	    end;
	    Buffer.add_string b " = match ";
	    if previous_was_token then
	      Buffer.add_string b "yy_get_next()"
	    else
	      Buffer.add_string b "yy_current()";
	    Buffer.add_string b " with ";
	    Buffer.add_string b tok;
	    Buffer.add_string b " x -> x | _ -> raise ";
	    if allow_not_found then
	      Buffer.add_string b "Not_found"
	    else
	      Buffer.add_string b "Parsing.Parse_error";
	    Buffer.add_string b " in\n";
	  end
	  else begin
	    (* Simple token *)
	    Buffer.add_string b "if (";
	    if previous_was_token then
	      Buffer.add_string b "yy_get_next()"
	    else
	      Buffer.add_string b "yy_current()";
	    Buffer.add_string b ") <> ";
	    Buffer.add_string b tok;
	    Buffer.add_string b " then raise ";
	    if allow_not_found then
	      Buffer.add_string b "Not_found;\n"
	    else
	      Buffer.add_string b "Parsing.Parse_error;\n"
	  end
      | L_symbol(called_rule, args, lab) ->
	  if previous_was_token then
	    Buffer.add_string b "ignore(yy_get_next());\n";
	  make_rule_invocation called_rule args lab allow_not_found
      | L_indirect(ml_name, args, lab) ->
	  if previous_was_token then
	    Buffer.add_string b "ignore(yy_get_next());\n";
	  make_indirect_rule_invocation ml_name args lab allow_not_found
  in

  let process_pattern (current_position, previous_was_token) pat =
    (* Assign "position" if necessary. *)
    let new_position =
      if branch.branch_error_code <> None then begin
	match pat.pat_symbol with
	    U_symbol(_,Some l)   -> l
	  | L_symbol(_,_,Some l) -> l
	  | L_indirect(_,_,Some l) -> l
	  | _ -> ""
      end
      else ""
    in
    if new_position <> current_position then begin
      Buffer.add_string b "yy_position := \"";
      Buffer.add_string b new_position;
      Buffer.add_string b "\";\n";
    end;

    let this_is_token =
      match pat.pat_symbol with
	  U_symbol(_,_)   -> pat.pat_modifier = Exact
	| L_symbol(_,_,_) -> false
	| L_indirect(_,_,_) -> false
    in

    (* First distinguish between Exact, Option, and Repetition: *)
    begin match pat.pat_modifier with
	Exact ->
	  process_symbol pat.pat_symbol previous_was_token false
      | Option ->
	  begin match label_of_symbol tree pat.pat_symbol with
	      None ->
		(* CASE: optional symbol without label *)
		(* OPTIMIZATION: If the symbol is
		 * a token, the loop becomes very simple.
		 *)
		if (match pat.pat_symbol with 
			U_symbol(_,_) -> true | _ -> false) 
		then begin
		  let tok = match pat.pat_symbol with 
		               U_symbol(t,_) -> t | _ -> assert false in
		  (* Optimized case *)
		  Buffer.add_string b "if ";
		  if previous_was_token then
		    Buffer.add_string b "yy_get_next()"
		  else
		    Buffer.add_string b "yy_current()";
		  Buffer.add_string b " = ";
		  Buffer.add_string b tok;
		  Buffer.add_string b " then ignore(yy_get_next());\n";
		end
		else begin
		  (* General, non-optimized case: *)
		  Buffer.add_string b "( try (";
		  process_symbol pat.pat_symbol previous_was_token true;
		  Buffer.add_string b "ignore(yy_get_next());\n";
		  Buffer.add_string b ") with Not_found -> ());\n";
		end
	    | Some l ->
		(* CASE: optional symbol with label *)
		Buffer.add_string b "let ";
		Buffer.add_string b l;
		Buffer.add_string b " = try let yy_tok = Some(";
		process_symbol pat.pat_symbol previous_was_token true;
		Buffer.add_string b l;
		Buffer.add_string b ") in\n";

		if (match pat.pat_symbol with
			U_symbol(_,_) -> true | _ -> false) then
		  Buffer.add_string b "ignore(yy_get_next());\n";

		Buffer.add_string b "yy_tok with Not_found -> None in\n";
	  end
      | Repetition ->
	  begin match label_of_symbol tree pat.pat_symbol with
	      None ->
		(* CASE: repeated symbol without label *)
		(* OPTIMIZATION: If the symbol is
		 * a token, the loop becomes very simple.
		 *)
		if (match pat.pat_symbol with 
			U_symbol(_,_) -> true | _ -> false) 
		then begin
		  let tok = match pat.pat_symbol with 
		               U_symbol(t,_) -> t | _ -> assert false in
		  if previous_was_token then begin
		    (* Optimized case I *)
		    Buffer.add_string b "while yy_get_next() = ";
		    Buffer.add_string b tok;
		    Buffer.add_string b " do () done;\n";
		  end
		  else begin
		    (* Optimized case II *)
		    Buffer.add_string b "if yy_current() = ";
		    Buffer.add_string b tok;
		    Buffer.add_string b " then (";
		    Buffer.add_string b "while yy_get_next() = ";
		    Buffer.add_string b tok;
		    Buffer.add_string b " do () done);\n";
		  end
		end
		else begin
		  (* General, non-optimized case: *)
		  if previous_was_token then
		    Buffer.add_string b "ignore(yy_get_next());\n";
		  Buffer.add_string b "( try while true do (";
		  process_symbol pat.pat_symbol false true;

		  if (match pat.pat_symbol with
			 U_symbol(_,_) -> true | _ -> false) then
		    Buffer.add_string b "ignore(yy_get_next());\n"
		  else
		    Buffer.add_string b "();\n";

		  Buffer.add_string b ") done with Not_found -> ());\n";
		end
	    | Some l ->
		(* CASE: repeated symbol with label *)
		if previous_was_token then
		  Buffer.add_string b "ignore(yy_get_next());\n";
		Buffer.add_string b "let yy_list = ref [] in\n";
		Buffer.add_string b "( try while true do \n";
		process_symbol pat.pat_symbol false true;
		Buffer.add_string b "yy_list := ";
		Buffer.add_string b l;
		Buffer.add_string b " :: !yy_list;\n";

		if (match pat.pat_symbol with
			U_symbol(_,_) -> true | _ -> false) then
		  Buffer.add_string b "ignore(yy_get_next());\n";

		Buffer.add_string b "done with Not_found -> ());\n";
		Buffer.add_string b "let ";
		Buffer.add_string b l;
		Buffer.add_string b " = List.rev !yy_list in\n";
	  end
    end;

    (* Continue: *)
    (new_position, this_is_token)
  in


  let process_inner_branch current_position =
    (* If there is "early code", run this now: *)
    output_code b file_name branch.branch_early_code;
    Buffer.add_string b "\n";

    (* If the first symbol is a rule invocation, call the corresponding
     * parser function now.
     *)
    let previous_was_token =
      begin match branch.branch_selector with
	  U_symbol(_,_) -> 
	    true
	| L_symbol(called_rule, args, lab) ->
	    make_rule_invocation called_rule args lab true;
	    false
	| L_indirect(_,_,_) -> 
	    failwith("The first symbol in some rule is an indirect call; this is not allowed")
      end
    in

    (* Now output the "let-CODE". *)
    output_code b file_name branch.branch_binding_code;
    Buffer.add_string b "\n";

    (* Process the other symbols in turn: *)
    let (_, previous_was_token') =
      (List.fold_left
	 process_pattern
	 (current_position, previous_was_token)
	 branch.branch_pattern
      )
    in

    (* Special case: 
     *
     * If previous_was_token', we must invoke yy_get_next one more time.
     * This is deferred until "CODE" is executed to give this code 
     * the chance to make the next token available (in XML, the next token
     * might come from a different entity, and "CODE" must switch to this
     * entity).
     *)

    (* Now output "CODE": *)
    Buffer.add_string b "let result = \n";
    output_code b file_name branch.branch_result_code;
    Buffer.add_string b "\nin\n";

    if previous_was_token' then
      Buffer.add_string b "ignore(yy_get_next());\nresult\n"
    else
      Buffer.add_string b "result\n"
  in

  (* If we have a ? clause, generate now the "try" statement *)
  match branch.branch_error_code with
      None ->
	Buffer.add_string b "( ";
	process_inner_branch "";
	Buffer.add_string b " )";
    | Some code ->

	(* let position = ref "<label>" in *)

	Buffer.add_string b "let yy_position = ref \"";
	let current_position =
	  match branch.branch_selector with
	      U_symbol(_,_) -> ""
	    | L_symbol(_,_,None) -> ""
	    | L_symbol(_,_,Some l) -> l
	    | L_indirect(_,_,None) -> ""
	    | L_indirect(_,_,Some l) -> l
	in
	Buffer.add_string b current_position;
	Buffer.add_string b "\" in\n";
	
	(* The "try" statement: *)

	Buffer.add_string b "( try (\n";

	process_inner_branch current_position;

	Buffer.add_string b "\n) with Parsing.Parse_error -> (\n";
	output_code b file_name code;
	Buffer.add_string b "\n))\n"
;;


let process b file_name tree =
  (* Iterate over the rules and output the parser functions: *)
  let is_first = ref true in
  List.iter
    (fun r ->

       (* Generate the function header: *)

       if !is_first then
	 Buffer.add_string b "let rec "
       else
	 Buffer.add_string b "and ";
       is_first := false;
       Buffer.add_string b "parse_";
       Buffer.add_string b r.rule_name;
       Buffer.add_string b " yy_current yy_get_next";
       List.iter
	 (fun arg -> Buffer.add_string b " ";
	             Buffer.add_string b arg)
	 r.rule_arguments;
       Buffer.add_string b " =\n";

       (* Generate the "match" statement: *)

       Buffer.add_string b "match yy_current() with\n";
       let s_done = ref [] in
       (* s_done: The set of already matched tokens *)

       List.iter
	 (fun branch ->
	    match branch.branch_selector with
		U_symbol(tok, lab) ->
		  (* A simple token *)
		  if List.mem tok !s_done then begin
		    prerr_endline("WARNING: In rule `" ^ r.rule_name ^ 
				  "': Match for token `" ^
				  tok ^ "' hidden by previous match");
		  end
		  else
		    if is_typed tree tok then begin
		      match lab with
			  None ->
			    Buffer.add_string b "| ";
			    Buffer.add_string b tok;
			    Buffer.add_string b " _ -> ";
			    process_branch b file_name tree branch;
			    Buffer.add_string b "\n";
			    s_done := tok :: !s_done;
			| Some l ->
			    Buffer.add_string b "| ";
			    Buffer.add_string b tok;
			    Buffer.add_string b " ";
			    Buffer.add_string b l;
			    Buffer.add_string b " -> ";
			    process_branch b file_name tree branch;
			    Buffer.add_string b "\n";
			    s_done := tok :: !s_done;
		  end
		  else begin
		    Buffer.add_string b "| ";
		    Buffer.add_string b tok;
		    Buffer.add_string b " -> ";
		    process_branch b file_name tree branch;
		    Buffer.add_string b "\n";
		    s_done := tok :: !s_done;
		  end
	      | L_symbol(called_rule, args, lab) ->
		  (* An invocation of a rule *)
		  let s_rule = selector_set_of_rule tree called_rule in
		  let s_rule' =
		    List.filter
		      (fun tok ->
			 if List.mem tok !s_done then begin
			   prerr_endline("WARNING: In rule `" ^ r.rule_name ^ 
					 "': Match for token `" ^
					 tok ^ "' hidden by previous match");
			   false
			 end
			 else true)
		      s_rule in
		  if s_rule' <> [] then begin
		    Buffer.add_string b "| ( ";
		    let is_first = ref true in
		    List.iter
		      (fun tok ->
			 if not !is_first then
			   Buffer.add_string b " | ";
			 is_first := false;
			 Buffer.add_string b tok;
			 if is_typed tree tok then
			   Buffer.add_string b " _";
		      )
		      s_rule';
		    Buffer.add_string b ") -> ";
		    process_branch b file_name tree branch;
		    Buffer.add_string b "\n";
		    s_done := s_rule' @ !s_done;
		  end
	      | L_indirect(ml_name, args, lab) ->
		  (* An invocation of an indirect rule *)
		  failwith("The first symbol in rule `" ^ r.rule_name ^ 
			   "' is an indirect call; this is not allowed")
	 )
	 r.rule_branches;

       Buffer.add_string b "\n| _ -> raise Not_found\n";
    )
    tree.text_rules;

  Buffer.add_string b " in\n"
;;


let count_lines s =
  (* returns number of lines in s, number of columns of the last line *)
  let l = String.length s in

  let rec count n k no_cr no_lf =
    let next_cr = 
      if no_cr then
        (-1)
      else
        try String.index_from s k '\013' with Not_found -> (-1) in
    let next_lf = 
      if no_lf then
        (-1)
      else
        try String.index_from s k '\010' with Not_found -> (-1) in
    if next_cr >= 0 & (next_lf < 0 or next_cr < next_lf) then begin
      if next_cr+1 < l & s.[next_cr+1] = '\010' then
        count (n+1) (next_cr+2) false (next_lf < 0)
      else
        count (n+1) (next_cr+1) false (next_lf < 0)
    end
    else if next_lf >= 0 then begin
      count (n+1) (next_lf+1) (next_cr < 0) false
    end
    else
      n, (l - k)

  in
  count 0 0 false false
;;


type scan_context =
    { mutable old_line : int;
      mutable old_column : int;
      mutable line : int;
      mutable column : int;
    }
;;


let rec next_token context lexbuf =
  let t = Lexer.scan_file lexbuf in
  let line = context.line in
  let column = context.column in
  context.old_line <- line;
  context.old_column <- column;
  let n_lines, n_columns = count_lines (Lexing.lexeme lexbuf) in
  if n_lines > 0 then begin
    context.line <- line + n_lines;
    context.column <- n_columns;
  end 
  else 
    context.column <- column + n_columns;
  match t with
      Space -> next_token context lexbuf
    | Code(s,_,_) -> Code(s,line,column + 2)
    | Eof   -> failwith "Unexpected end of file"
    | _     -> t
;;


let parse_and_generate ch =
  let b = Buffer.create 20000 in

  let rec find_sep context lexbuf =
    let t = Lexer.scan_header lexbuf in
    let line = context.line in
    let column = context.column in
    context.old_line <- line;
    context.old_column <- column;
    let n_lines, n_columns = count_lines (Lexing.lexeme lexbuf) in
    if n_lines > 0 then begin
      context.line <- line + n_lines;
      context.column <- n_columns;
    end 
    else 
      context.column <- column + n_columns;
    match t with
	Code(s,_,_) -> 
	  Buffer.add_string b s;
          find_sep context lexbuf
      | Eof    -> failwith "Unexpected end of file"
      | Separator -> ()
      | _         -> assert false
  in

  let rec find_rest context lexbuf =
    let t = Lexer.scan_header lexbuf in
    let line = context.line in
    let column = context.column in
    context.old_line <- line;
    context.old_column <- column;
    let n_lines, n_columns = count_lines (Lexing.lexeme lexbuf) in
    if n_lines > 0 then begin
      context.line <- line + n_lines;
      context.column <- n_columns;
    end 
    else 
      context.column <- column + n_columns;
    match t with
	Code(s,_,_) -> 
	  Buffer.add_string b s;
          find_rest context lexbuf
      | Eof    -> ()
      | _      -> assert false
  in

  (* First read until '%%' *)
  let lexbuf = Lexing.from_channel ch in
  let context = { old_line = 0; old_column = 0; line = 1; column = 0 } in
  let file_name = "stdin" in
  try
    output_code_location b file_name ("", 1, 0);
    find_sep context lexbuf;
    (* Parse the following text *)
    let text = (Parser.text (next_token context) lexbuf : Ast.text) in
    (* Process it: *)
    process b file_name text;
    (* Read rest *)
    output_code_location b file_name ("", context.line, context.column);
    find_rest context lexbuf;
    (* Output everything: *)
    print_string (Buffer.contents b)
  with
      any ->
	Printf.eprintf 
	  "Error at line %d column %d: %s\n"
	  context.old_line
	  context.old_column
	  (Printexc.to_string any);
	exit 1
;;


parse_and_generate stdin;;
exit 0;;

(* ======================================================================
 * History:
 * 
 * $Log: generator.ml,v $
 * Revision 1.4  2000/05/09 00:03:22  gerd
 * 	Added [ ml_name ] symbols, where ml_name is an arbitrary
 * OCaml identifier.
 *
 * Revision 1.3  2000/05/08 22:03:01  gerd
 * 	It is now possible to have a $ {{ }} sequence right BEFORE
 * the first token. This code is executed just after the first token
 * has been recognized.
 *
 * Revision 1.2  2000/05/06 21:51:08  gerd
 * 	Numerous bugfixes.
 *
 * Revision 1.1  2000/05/06 17:36:17  gerd
 * 	Initial revision.
 *
 * 
 *)
