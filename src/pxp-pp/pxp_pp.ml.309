(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* Syntax extension to construct XML trees *)

open Netulex.Ulexing
module Ulexing = Netulex.Ulexing

(**********************************************************************)
(* pa_macro:
 *
 * OCAML_NEW_LOC: If defined, camlp4 locations have type
 *                (Lexing.position * Lexing.position) (O'Caml >= 3.08)
 *                Otherwise, locations have type int*int (O'Caml <= 3.07).
 *)

(**********************************************************************)
(* Lexer bases on ulex *)

type pos = int * int * int
  (* Triple (line, line start, char pos):
   * - line: line number. First line has number 1
   * - line start: Character count where the current line started
   * - char pos: Character count of the position
   *
   * When OCAML_NEW_LOC, the triple is fully used. Otherwise,
   * we always have line=1, line_start=0, and only char_pos contains
   * useful information.
   *)

exception Lex_error of (string * pos)
  (* Message, position *)

type token =
    [ `Langle | `Rangle | `Rangle_empty | `Lbracket | `Rbracket
    | `Equal | `Lparen | `Rparen | `List_concat | `String_concat
    | `Comment | `PI | `Super | `End_ocaml_comment | `Other | `EOF
    | `Data | `Langle_colon
    | `Literal of string | `Name of string | `Anti of string ]

  (* Tokens are always encoded in UTF-8! *)

let regexp int = [ '0'-'9' ]+ | "0" ['X' 'x'] ['0'-'9' 'a'-'f' 'A'-'F']+ ;;

let regexp int_entity = "&" "#" int ";" ;;

let regexp name = (xml_letter | '_' | ':' | int_entity) 
                  (xml_letter | xml_digit | '.' | ':' | '-' | '_' | 
		   xml_combining_char | xml_extender | int_entity) *
;;

let rec scan line line_start =
  let pos1 lexbuf = 
    (!line, !line_start, lexeme_start lexbuf) in
  let pos2 lexbuf = 
    (!line, !line_start, lexeme_end lexbuf) in
  lexer
    "<"    -> `Langle, (pos1 lexbuf), (pos2 lexbuf)
  | "<:"   -> `Langle_colon, (pos1 lexbuf), (pos2 lexbuf)
  | ">"    -> `Rangle, (pos1 lexbuf), (pos2 lexbuf)
  | "/>"   -> `Rangle_empty, (pos1 lexbuf), (pos2 lexbuf)
  | "["    -> `Lbracket, (pos1 lexbuf), (pos2 lexbuf)
  | "]"    -> `Rbracket, (pos1 lexbuf), (pos2 lexbuf)
  | "="    -> `Equal, (pos1 lexbuf), (pos2 lexbuf)
  | "("    -> `Lparen, (pos1 lexbuf), (pos2 lexbuf)
  | ")"    -> `Rparen, (pos1 lexbuf), (pos2 lexbuf)
  | "@"    -> `List_concat, (pos1 lexbuf), (pos2 lexbuf)
  | "^"    -> `String_concat, (pos1 lexbuf), (pos2 lexbuf)
  | "<!>"  -> `Comment, (pos1 lexbuf), (pos2 lexbuf)
  | "<?>"  -> `PI, (pos1 lexbuf), (pos2 lexbuf)
  | "<^>"  -> `Super, (pos1 lexbuf), (pos2 lexbuf)
  | "<*>"  -> `Data, (pos1 lexbuf), (pos2 lexbuf)
  | "(:"   -> let p1 = pos1 lexbuf in
              let text = 
		String.concat "" (scan_antiquot p1 line line_start lexbuf) in
	      let p2 = pos2 lexbuf in
	      (`Anti text, p1, p2)
  | "(*"   -> let p1 = pos1 lexbuf in
              skip_comment p1 line line_start lexbuf;
              scan line line_start lexbuf
  | "*)"   -> `End_ocaml_comment, (pos1 lexbuf), (pos2 lexbuf)
  | '"' [^ '"']* '"' -> 
              let n = lexeme_length lexbuf in
              let s = utf8_sub_lexeme lexbuf 1 (n-2) in
              `Literal s, (pos1 lexbuf), (pos2 lexbuf)
  | '"'      -> raise(Lex_error("Unterminated string literal",
				(pos1 lexbuf)))
  | name     -> `Name (utf8_lexeme lexbuf), (pos1 lexbuf), (pos2 lexbuf)
  | "\r\n" 
  | "\r"
  | "\n"     -> IFDEF OCAML_NEW_LOC THEN 
                  incr line; 
                  line_start := lexeme_end lexbuf
		ELSE
		  ()		  
		END;
		scan line line_start lexbuf
  | [ ' ' '\t' ]+ -> scan line line_start lexbuf
  | _        -> `Other, (pos1 lexbuf), (pos2 lexbuf) (* May occur in comments *)
  | eof      -> `EOF, (pos1 lexbuf), (pos2 lexbuf)

and scan_antiquot start_pos line line_start = 
  lexer
    ":)"     -> []
  | [^ ':' '\r' '\n' ]+ -> 
                let s = utf8_lexeme lexbuf in
                s :: scan_antiquot start_pos line line_start lexbuf
  | ":"      -> ":" :: scan_antiquot start_pos line line_start lexbuf
  | "\r\n"
  | "\r"
  | "\n"     -> IFDEF OCAML_NEW_LOC THEN
                  incr line; 
                  line_start := lexeme_end lexbuf
                ELSE
                  ()
		END;
		let s = utf8_lexeme lexbuf in
		s :: scan_antiquot start_pos line line_start lexbuf
  | eof      -> raise(Lex_error("Unterminated antiquotation",
				start_pos))

and skip_comment start_pos line line_start lexbuf =
  match scan line line_start lexbuf with
      (`EOF,_ ,_) ->  raise(Lex_error("Unterminated comment", start_pos))
    | (`End_ocaml_comment,_,_) -> ()
    | _ -> skip_comment start_pos line line_start lexbuf
;;


type charset_decl =
    { source_enc : Netconversion.encoding;
      rep_enc : Netconversion.encoding;
    }
;;


let default_decl =
  { source_enc = `Enc_iso88591;
    rep_enc = `Enc_iso88591
  } ;;

let current_decl = ref default_decl ;;

let reset_decl() =
  current_decl := default_decl ;;


let current_file = ref "" ;;

let check_file() =
  if !Pcaml.input_file <> !current_file then (
    reset_decl();
    current_file := !Pcaml.input_file
  )
;;


let unichar = Netconversion.ustring_of_uchar `Enc_utf8;;


let scan_entities line line_start offset =
  lexer
    "&#" int ";" ->
      let l = lexeme_length lexbuf in
      let s = utf8_sub_lexeme lexbuf 2 (l-3) in
      unichar (int_of_string s)
  | "&lt;" ->
      "<"
  | "&gt;" ->
      ">"
  | "&apos;" ->
      "'"
  | "&amp;" ->
      "&"
  | "&quot;" ->
      "\""
  | "&" ->
      raise(Lex_error("'&' must be written as '&amp;'",
		      (!line,
		       !line_start,
		       lexeme_start lexbuf + offset)))
  | "\r\n"
  | "\r"
  | "\n"     -> IFDEF OCAML_NEW_LOC THEN
                  incr line; 
                  line_start := lexeme_end lexbuf + offset
                ELSE
		  ()
		END;
		utf8_lexeme lexbuf
  | _ ->
      utf8_lexeme lexbuf
  | eof ->
      raise End_of_file
;;


let convert_entities (line,line_start,pos) offset s =
  let lexbuf = from_ulb_lexbuf (Netulex.ULB.from_string `Enc_utf8 s) in
  let rline = ref line in
  let rline_start = ref line_start in
  let b = Buffer.create 200 in
  try
    while true do
      Buffer.add_string b (scan_entities rline rline_start (pos+offset) lexbuf)
    done;
    assert false;
  with
      End_of_file ->
	Buffer.contents b
;;


let scan_string s : (token * pos * pos) Stream.t =
  let src_enc = !current_decl.source_enc in
  let line = ref 1 in
  let line_start = ref 0 in
  let lexbuf = from_ulb_lexbuf (Netulex.ULB.from_string src_enc s) in
  Stream.from
    (fun count ->
       try
	 ( match scan line line_start lexbuf with
	       (`Name s, p1, p2) ->
		 Some (`Name (convert_entities p1 0 s), p1, p2)
	     | (`Literal s, p1, p2) ->
		 Some (`Literal (convert_entities p1 1 s), p1, p2)
	     | other ->
		 Some other
	 )
       with
	   Error ->
	     raise(Lex_error("Lexical error",
			     (!line,
			      !line_start,
			      lexeme_start lexbuf)))
    )
;;


(**********************************************************************)
(* Stream Parser *)

exception Syntax_error of pos * pos;;


type ast_node0 =
    [ `Element of (ast_string * ast_attr list * ast_node_list)
    | `Data of ast_string
    | `Comment of ast_string
    | `PI of (ast_string * ast_string)
    | `Super of ast_node_list
    | `Meta of (string * ast_attr list * ast_node)
    | `Ident of string
    | `Anti of string
       (* The following are the same as ast_string0. They are interpreted
	* as data node:
	*)
    | `Literal of string
    | `Concat of ast_node list
    ]
and ast_node = (ast_node0 * pos * pos)

and ast_node_list0 =
    [ `Nodes of ast_node list
    | `Concat of ast_node_list list
    | `Ident of string
    | `Anti of string
    ]
and ast_node_list = (ast_node_list0 * pos * pos)

and ast_string0 =
    [ `Literal of string
    | `Concat of ast_string list
    | `Ident of string
    | `Anti of string
    ]
and ast_string = (ast_string0 * pos * pos)

and ast_attr0 =
    [ `Attr of ast_string * ast_string
    | `Anti of string
    ]
and ast_attr = (ast_attr0 * pos * pos)

and ast_any_node =
    [ `Node of ast_node
    | `Nodelist of ast_node_list
    ]
;;


(* Note that the syntax allows that strings are interpreted as data
 * nodes, e.g.
 *
 * <a>[ "x" ]: Element "a" with data sub node "x"
 *
 * ==> Every string can also be interpreted as data node.
 *
 * At some locations we do not allow that because _only_ strings
 * are reasonable. Then string_restr=true ("string restriction").
 * E.g. 
 *
 * <a x="x"/>  is ok, but
 * <a x=<b/>/> is nonsense
 *
 * Not every expression composed of strings and nodes is sound. E.g.
 * "x" ^ "y" is ok (string concatenation), but "x" ^ <a/> is not.
 * The latter is accepted by the grammar, but must be rejected by
 * an additional type check.
 *)


let ensure_at_end s =
  match Stream.peek s with
      Some(`EOF, _, _) -> ()
    | _ -> raise Stream.Failure
;;


let last_pos s =
  match Stream.peek s with
      Some(_, _, pos) -> pos
    | _ -> assert false
;;
  

let check_meta name atts =
  match name with
      "scope" ->
	()
    | "autoscope" ->
	if atts <> [] then raise Stream.Failure;
    | "emptyscope" ->
	if atts <> [] then raise Stream.Failure;
    | _ ->
	raise Stream.Failure
;;


let rec parse_any_expr (s : (token * pos * pos) Stream.t) : ast_any_node =
  match Stream.peek s with
      Some(`Lbracket, _, _) ->
	let v = `Nodelist(parse_nodelist_expr s) in
	ensure_at_end s;
	v
    | Some _ ->
	let v = `Node(parse_expr false s) in
	ensure_at_end s;
	v
    | None ->
	raise Stream.Failure

and parse_expr string_restr : (token * pos * pos) Stream.t -> ast_node =
  parser
      [< (nl1, p1, p2) as nl = parse_factor string_restr;
	 c1 = parse_cont string_restr;
      >] ->
	match c1 with
	    None ->
	      nl
	  | Some(`Concat l, p1', p2') ->
	      (`Concat(nl :: l), p1, p2')
	  | Some(other, p1', p2') ->
	      (`Concat([nl; (other, p1', p2')]), p1, p2')

and parse_cont string_restr : (token * pos * pos) Stream.t -> ast_node option =
  parser
      [< '(`String_concat, p1, p2);
	 e = parse_expr string_restr;
      >] ->
	Some e
    | [< >] ->
	None

and parse_factor string_restr : (token * pos * pos) Stream.t -> ast_node =
  parser
      [< '(`Langle, p1, p2) when not string_restr; 
	 name = parse_element_name;
	 attrs, flag, p' = parse_attrs;
	 (subnodes0, p1', p2') as subnodes = 
	   if flag then
	     (fun _ -> `Nodes [], p', p')
	   else
	     parse_nodelist_expr;
      >] ->
	( `Element(name, attrs, subnodes), p1, p2'  )
    | [< '(`Langle_colon, p1, p2) when not string_restr; 
	 '(`Name name, _, _);
	 attrs, flag, p' = parse_attrs;
	 (subnode0, p1', p2') as subnode = parse_expr string_restr;
      >] ->
	( check_meta name attrs;
	  `Meta(name, attrs, subnode), p1, p2'  )
    | [< '(`Comment, p1, p2) when not string_restr;
	 (contents0, p1', p2') as contents = parse_string_expr
      >] ->
	( `Comment contents, p1, p2' )
    | [< '(`PI, p1, p2) when not string_restr;
	 (contents0, p1', p2') as contents = parse_string_expr;
	 (contents0', p1'', p2'') as contents' = parse_string_expr
      >] ->
	( `PI(contents,contents'), p1, p2'' )
    | [< '(`Super, p1, p2) when not string_restr;
	 (subnodes0, p1', p2') as subnodes = parse_nodelist_expr;
      >] -> 
	( `Super subnodes, p1, p2' )
    | [< '(`Data, p1, p2) when not string_restr;
         (contents0, p1', p2') as contents = parse_string_expr
      >] ->
         ( `Data contents, p1, p2' )
    | [< '(`Lparen, p1, p2);
	 (inner, p1', p2') = parse_expr string_restr;
	 '(`Rparen, p1'', p2'') 
      >] ->
	(inner, p1, p2'')
    | [< '(`Anti text, p1, p2) >] ->
	( `Anti text, p1, p2 )
    | [< '(`Literal s, p1, p2) >] ->
	(`Literal s, p1, p2)
    | [< '(`Name n, p1, p2) >] ->
	( `Ident n, p1, p2)

and parse_element_name : (token * pos * pos) Stream.t -> ast_string =
  parser
      [< '(`Name n, p1, p2) >] ->
	( `Literal n, p1, p2 )
    | [< '(`Anti text, p1, p2) >] ->
	( `Anti text, p1, p2 )
    | [< '(`Lparen, p1, p2);
	 s = parse_string_expr;
	 '(`Rparen, p1', p2') 
      >] ->
	let (str, _, _) = s in
	(str, p1, p2')

and parse_attrs : (token * pos * pos) Stream.t -> ast_attr list * bool * pos =
  parser
      [< '(`Name n, p1, p2);
	 '(`Equal, p1', p2');
	 (s, p1'', p2'') = parse_string_expr;
	 (rest, flag, p) = parse_attrs
      >] ->
	let name_str = (`Literal n, p1, p2) in
	let val_str  = (s, p1'', p2'') in
	( `Attr(name_str, val_str), p1, p2'' ) :: rest, flag, p
    | [< '(`Lparen, p1, p2);
	 (s1, p1', p2') = parse_string_expr;
	 '(`Rparen, p1'', p2'');
	 '(`Equal, _, _);
	 (s2, p1''', p2''') = parse_string_expr;
	 (rest, flag, p) = parse_attrs
      >] ->
	let name_str = (s1, p1', p2') in
	let val_str  = (s2, p1''', p2''') in
	( `Attr(name_str, val_str), p1, p2''' ) :: rest, flag, p
    | [< '(`Anti text, p1, p2);
	 (rest, flag, p) = parse_attrs
      >] ->
	( `Anti text, p1, p2 ) :: rest, flag, p
    | [< '(`Rangle, p1, p2) >] ->
	[], false, p2
    | [< '(`Rangle_empty, p1, p2) >] ->
	[], true, p2

and parse_nodelist_expr : (token * pos * pos) Stream.t -> ast_node_list =
  parser
      [< (nl1, p1, p2) as nl = parse_nodelist_factor;
	 c1 = parse_nodelist_cont;
      >] ->
	match c1 with
	    None ->
	      nl
	  | Some(`Concat l, p1', p2') ->
	      (`Concat(nl :: l), p1, p2')
	  | Some(other, p1', p2') ->
	      (`Concat([nl; (other, p1', p2')]), p1, p2')

and parse_nodelist_cont : (token * pos * pos) Stream.t -> ast_node_list option =
  parser
      [< '(`List_concat, p1, p2);
	 e = parse_nodelist_expr;
      >] ->
	Some e
    | [< >] ->
	None

and parse_nodelist_factor : (token * pos * pos) Stream.t -> ast_node_list =
  parser
      [< '(`Lbracket, p1, p2);
	 (l, p1', p2') = parse_bracket_expr
      >] ->
	( `Nodes l, p1, p2' )
    | [< '(`Lparen, p1, p2);
	 (e, p1', p2') = parse_nodelist_expr;
	 '(`Rparen, p1'', p2'')
      >] ->
	( e, p1, p2'' )
    | [< '(`Anti text, p1, p2) >] ->
	( `Anti text, p1, p2 )
    | [< '(`Name n, p1, p2) >] ->
	( `Ident n, p1, p2)
    | [< (n, p1, p2) = parse_expr false >] ->
	`Nodes [n, p1, p2], p1, p2

and parse_bracket_expr : (token * pos * pos) Stream.t -> ast_node list * pos * pos =
  parser
      [< '(`Rbracket, p1, p2) >] ->
	( [], p1, p2 )
    | [< (n, p1, p2) = parse_expr false;
	 (b, p1', p2') = parse_bracket_expr >] ->
	((n, p1, p2) :: b, p1, p2')

and parse_string_expr (s : (token * pos * pos) Stream.t) : ast_string =
  let rec coerce (e : ast_node) : ast_string =
    match e with
	(`Concat l, p1, p2) ->
	  `Concat (List.map coerce l), p1, p2
      | `Anti _, _, _ as e' -> e'
      | `Ident _, _, _ as e' -> e'
      | `Literal _, _, _ as e' -> e'
      | _ -> assert false
  in
  coerce(parse_expr true s)
;;


let rec parse_charset_decl0 : (token * pos * pos) Stream.t -> charset_decl =
  parser
      [< '(`Name n, p1, p2);
	 '(`Equal, _, _);
	 '(`Literal s, p1', p2');
	 cur = parse_charset_decl0 >] ->
	
	( match n with
	      "source" ->
		let e = Netconversion.encoding_of_string s in
		{ cur with source_enc = e }
	    | "representation" ->
		let e = Netconversion.encoding_of_string s in
		{ cur with rep_enc = e }
	    | _ ->
		raise Stream.Failure
	)

    | [< >] ->
	(* default: *)
	!current_decl
;;


let parse_charset_decl s =
  let v = parse_charset_decl0 s in
  ensure_at_end s;
  v
;;


let call_parser f s =
  try
    f s 
  with
      Stream.Failure
    | Stream.Error _ ->
	( match Stream.peek s with
	      Some(_, p1, p2) ->
		raise(Syntax_error(p1,p2))
	    | None ->
		assert false  (* must not happen *)
	)
;;


(**********************************************************************)
(* Type checker *)

(* Typing checking also transforms the ast such that 
 * `Literal and `Concat are never used in the context of a
 * node expression.
 *)

exception Typing_error of (string * pos * pos);;

let rec check_any_expr : ast_any_node -> ast_any_node =
  function
      `Node n -> `Node(check_node_expr n)
    | `Nodelist l -> `Nodelist(check_nodelist_expr l)

and check_nodelist_expr : ast_node_list -> ast_node_list =
  function
      (`Nodes l,p1,p2) -> (`Nodes(List.map check_node_expr l),p1,p2)
    | (`Concat l,p1,p2) -> (`Concat(List.map check_nodelist_expr l),p1,p2)
    | ((`Ident _|`Anti _),_,_) as nl -> nl

and check_node_expr : ast_node -> ast_node =
  function
      ((`Data _ | `Comment _ | `PI _ | `Ident _ | `Anti _),_,_) as n -> n
    | (`Element(name,attrs,children),p1,p2) ->
	(`Element(name,attrs,check_nodelist_expr children),p1,p2)
    | (`Super children,p1,p2) ->
	(`Super(check_nodelist_expr children),p1,p2)
    | (`Literal s,p1,p2) ->
	(`Data(`Literal s,p1,p2),p1,p2)
    | (`Concat l,p1,p2) ->
	(`Data(`Concat(List.map check_node_expr_as_string l),p1,p2),p1,p2)
    | (`Meta(n,a,child),p1,p2) ->
	(`Meta(n,a,check_node_expr child),p1,p2)

and check_node_expr_as_string : ast_node -> ast_string =
  function
      ((`Data _ | `Comment _ | `PI _ | `Element _ | `Super _),p1,p2) ->
	raise(Typing_error("Nodes cannot be used as strings", p1, p2))
    | ((`Ident _| `Anti _ | `Literal _),p1,p2) as n ->
	n
    | (`Concat l,p1,p2) ->
	(`Concat(List.map check_node_expr_as_string l),p1,p2)
    | (`Meta(n,a,child),p1,p2) ->
	raise(Typing_error("Meta node not allowed in string context", p1, p2))
;;

(**********************************************************************)
(* Code generator for tree expressions *)

let mkloc ((_p1_line,_p1_line_start,p1_pos)) 
          ((_p2_line,_p2_line_start,p2_pos)) =
  (* Differs in O'Caml 3.07 and 3.08 *)
  IFDEF OCAML_NEW_LOC THEN
    let l1 = { Lexing.pos_fname = "";
	       Lexing.pos_lnum = _p1_line;
	       Lexing.pos_bol = _p1_line_start;
	       Lexing.pos_cnum = p1_pos } in
    let l2 = { Lexing.pos_fname = "";
	       Lexing.pos_lnum = _p2_line;
	       Lexing.pos_bol = _p2_line_start;
	       Lexing.pos_cnum = p2_pos } in
    (l1,l2)
  ELSE
    (p1_pos,p2_pos)
  END
;;


let raise_at (p1:pos) (p2:pos) exn =
(*
  let (p1_l,p1_s,p1_p) = p1 in 
  Printf.eprintf "Raise_at %d %d %d\n" p1_l p1_s p1_p;
  let (p2_l,p2_s,p2_p) = p2 in 
  Printf.eprintf "Raise_at %d %d %d\n" p2_l p2_s p2_p;
*)
  Stdpp.raise_with_loc (mkloc p1 p2) exn
;;


let catch_errors f =
  try f()
  with
      Lex_error(msg, p) ->
	let (l,s,c) = p in
	raise_at p (l,s,c+1) (Failure ("pxp-pp: Lexical error: " ^ msg))
    | Syntax_error(p1,p2) ->
	raise_at p1 p2 (Failure "pxp-pp: Syntax error")
    | Typing_error(msg,p1,p2) ->
	raise_at p1 p2 (Failure("pxp-pp: Typing error: " ^ msg))
;;


let generate_list loc el =
  List.fold_right (fun x l -> <:expr< [$x$ :: $l$] >>) el <:expr< [] >>
;;


let generate_ann_list loc el =
  List.fold_right (fun (ann,x) l -> 
		     match ann with
			 `Single -> <:expr< [$x$ :: $l$] >>
		       | `List   -> <:expr< $x$ @ $l$ >>) 
                  el 
                  <:expr< [] >>
;;


let generate_ident loc name =
  (* TODO: "." separation *)
  (* TODO: Convert back to latin 1 *)
  <:expr< $lid:name$ >>
;;


let expand_tree_expr (valcheck:bool) (s:string) : MLast.expr =
  (* valcheck: Whether to do DTD validation *)

  check_file();

  let valcheck_expr =
    let loc = mkloc (0,0,0) (0,0,0) in
    if valcheck then <:expr< True >> else <:expr< False >> in

  let to_rep s =
    Netconversion.convert 
      ~in_enc:`Enc_utf8 ~out_enc:(!current_decl.rep_enc) s in

  let to_src s =
    Netconversion.convert 
      ~in_enc:`Enc_utf8 ~out_enc:(!current_decl.source_enc) s in

  let rec generate_for_any_expr : ast_any_node -> MLast.expr =
    function
	`Node n -> generate_for_node_expr false n
      | `Nodelist nl -> generate_for_nodelist_expr false nl

  and generate_for_node_expr nsmode : ast_node -> MLast.expr = (
    (* nsmode: Whether there is a variable [scope] in the environment *)
    function
	(`Element(name,attrs,subnodes),p1,p2) ->
	  let loc = mkloc p1 p2 in
	  let name_expr = generate_for_string_expr name in
	  let attrs_expr_l = List.map generate_for_attr_expr attrs in
	  let attrs_expr = generate_ann_list loc attrs_expr_l in
	  let subnodes_expr = generate_for_nodelist_expr nsmode subnodes in
	  let el_only_expr =
	    <:expr< Pxp_document.create_element_node
	              ~valcheck:$valcheck_expr$
	              spec dtd $name_expr$ $attrs_expr$ >> in
	  let do_validation =
	    if valcheck then
	      <:expr< node#validate_contents() >>
	    else
	      <:expr< () >> in
	  let do_set_scope =
	    if nsmode then
	      <:expr< node#set_namespace_scope scope >>
	    else
	      <:expr< () >> in
	  <:expr< let node = $el_only_expr$ in
                  do { node # set_nodes $subnodes_expr$;
		       $do_set_scope$;
		       $do_validation$;
                       node } >>
      | (`Data text,p1,p2) ->
	  let text_expr = generate_for_string_expr text in
	  let loc = mkloc p1 p2 in
	  <:expr< Pxp_document.create_data_node spec dtd $text_expr$ >>
      | (`Comment text,p1,p2) ->
	  let text_expr = generate_for_string_expr text in
	  let loc = mkloc p1 p2 in
	  <:expr< Pxp_document.create_comment_node spec dtd $text_expr$ >>
      | (`PI(target,value),p1,p2) ->
	  let target_expr = generate_for_string_expr target in
	  let value_expr = generate_for_string_expr value in
	  let loc = mkloc p1 p2 in
	  <:expr< Pxp_document.create_pinstr_node spec dtd
	          (new Pxp_dtd.proc_instruction 
                         $target_expr$ $value_expr$ dtd#encoding)
	        >>
      | (`Super subnodes,p1,p2) ->
	  let subnodes_expr = generate_for_nodelist_expr nsmode subnodes in
	  let loc = mkloc p1 p2 in
	  <:expr< let node = Pxp_document.create_super_root_node spec dtd in
                  do { node # set_nodes $subnodes_expr$;
	               node } >>
      | (`Meta(name,attrs,subnode),p1,p2) ->
	  let loc = mkloc p1 p2 in
	  ( match name with
		"scope"      -> generate_scope loc attrs subnode
	      | "autoscope"  -> generate_autoscope loc subnode
	      | "emptyscope" -> generate_emptyscope loc subnode
	      | _            -> assert false (* already caught above *)
	  )
      | (`Ident name,p1,p2) ->
	  let loc = mkloc p1 p2 in
	  generate_ident loc (to_src name)
      | (`Anti text,p1,p2) ->
	  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
      | _ ->
	  (* `Literal and `Concat are impossible after type check *)
	  assert false )

  and generate_for_nodelist_expr nsmode : ast_node_list -> MLast.expr = (
    function
	(`Nodes l, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let l' = List.map (generate_for_node_expr nsmode) l in
	  generate_list loc l'
      | (`Concat l, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let l' = List.map (generate_for_nodelist_expr nsmode) l in
	  let l'' = generate_list loc l' in
	  <:expr< List.concat $l''$ >>
      | (`Ident name, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  generate_ident loc (to_src name)
      | (`Anti text, p1, p2) ->
	  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
  )

  and generate_for_attr_expr : ast_attr -> [`Single|`List] * MLast.expr = (
    function
	(`Attr(n,v), p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let n_expr = generate_for_string_expr n in
	  let v_expr = generate_for_string_expr v in
	  `Single, <:expr< ($n_expr$, $v_expr$) >>
      | (`Anti text, p1, p2) ->
	  `List, 
	  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
  )

  and generate_scope loc attrs subnode : MLast.expr = (
    let subexpr = generate_for_node_expr true subnode in
    if attrs = [] then
      subexpr
    else
      let decl_expr_l = List.map generate_for_attr_expr attrs in
      let decl_expr = generate_ann_list loc decl_expr_l in
      <:expr< let scope = 
                new Pxp_dtd.namespace_scope_impl 
		  (dtd # namespace_manager)
		  (Some scope)
		  $decl_expr$ in $subexpr$>>
  )

  and generate_autoscope loc subnode : MLast.expr = (
    let subexpr = generate_for_node_expr true subnode in
    <:expr< let scope =
            ( let mng = dtd # namespace_manager in
	      new Pxp_dtd.namespace_scope_impl 
		mng None mng#as_declaration ) in $subexpr$ >>
  )

  and generate_emptyscope loc subnode : MLast.expr = (
    let subexpr = generate_for_node_expr true subnode in
    <:expr< let scope =
            ( let mng = dtd # namespace_manager in
	      new Pxp_dtd.namespace_scope_impl 
		mng None [] ) in $subexpr$ >>
  )

  and generate_for_string_expr : ast_string -> MLast.expr = (
    function
	(`Literal s, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let s' = to_rep s in
	  <:expr< $str:s'$ >>
      | (`Concat l, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let l' = List.map generate_for_string_expr l in
	  let l'' = generate_list loc l' in
	  <:expr< String.concat "" $l''$ >>
      | (`Ident name, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  generate_ident loc (to_src name)
      | (`Anti text, p1, p2) ->
	  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
  )

  in

  catch_errors
    (fun () ->
       let stream = scan_string s in
       let ast = call_parser parse_any_expr stream in
       let ast' = check_any_expr ast in
       let ocaml_expr = generate_for_any_expr ast' in
       let loc = mkloc (1,0,0) (last_pos stream) in
       <:expr< $anti:ocaml_expr$ >>
    )
;;


(**********************************************************************)
(* Code generator for event streams *)


type ann = [`Single|`Tree|`Forest];;

let generate_event_generator
      (generate_tree : (ann * MLast.expr) list -> MLast.expr)
      (generate_forest : (ann * MLast.expr) list -> MLast.expr)
      (s:string) 
      : MLast.expr =
  (* Generates code to generate events. The input arguments
   * [generate_tree] and [generate_forest] process an intermediate
   * representation, the so-called annotated expression lists
   * (type (ann * MLast.expr) list), and return the final code.
   *
   * Kinds of annotations:
   * - `Single: The expression is a single event, i.e. an O'Caml value
   *   of type [event].
   * - `Tree: The expression represents a list of events corresponding
   *   to a node tree. It is left
   *   open how such lists are represented. The expression is either
   *   an O'Caml identifier or a subexpression from an antiquotation.
   * - `Forest: The expression represents a list of events corresponding
   *   to a list of node trees.
   *
   * The argument [generate_tree] is a function that generates the
   * final code for an annotated list of expressions. It can be expected
   * that the input list for [generate_tree] represents a node tree.
   *
   * The argument [generate_forest] does the same for an annotated
   * list of expressions that represents a list of node trees.
   *)

  let to_rep s =
    Netconversion.convert 
      ~in_enc:`Enc_utf8 ~out_enc:(!current_decl.rep_enc) s in

  let to_src s =
    Netconversion.convert 
      ~in_enc:`Enc_utf8 ~out_enc:(!current_decl.source_enc) s in

  let rec generate_for_any_expr loc : ast_any_node -> MLast.expr =
    function
	`Node n -> 
	  let e = generate_tree (generate_for_node_expr false n) in
	  <:expr< let _eid = Pxp_dtd.Entity.create_entity_id() in $e$ >>
      | `Nodelist nl -> 
	  let e = generate_forest (generate_for_nodelist_expr false nl) in
	  <:expr< let _eid = Pxp_dtd.Entity.create_entity_id() in $e$ >>

  and generate_for_node_expr nsmode : ast_node -> (ann * MLast.expr) list = (
    (* nsmode: Whether there is a variable [scope] in the environment *)
    function
	(`Element(name,attrs,subnodes),p1,p2) ->
	  let loc = mkloc p1 p2 in
	  let name_expr = generate_for_string_expr name in
	  let attrs_expr_l = List.map generate_for_attr_expr attrs in
	  let attrs_expr = generate_ann_list loc attrs_expr_l in
	  let subnodes_expr = generate_for_nodelist_expr nsmode subnodes in
	  let scope_opt_expr =
	    if nsmode then <:expr< Some scope >> else <:expr< None >> in

	  let start_tag =
	    <:expr< Pxp_types.E_start_tag($name_expr$,
					  $attrs_expr$,
					  $scope_opt_expr$,
					  _eid) >> in
	  let end_tag =
	    <:expr< Pxp_types.E_end_tag($name_expr$,_eid) >> in

	  [`Single, start_tag] @ subnodes_expr @ [`Single, end_tag]
      | (`Data text,p1,p2) ->
	  let text_expr = generate_for_string_expr text in
	  let loc = mkloc p1 p2 in
	  [ `Single, <:expr< Pxp_types.E_char_data($text_expr$) >> ]
      | (`Comment text,p1,p2) ->
	  let text_expr = generate_for_string_expr text in
	  let loc = mkloc p1 p2 in
	  [ `Single, <:expr< Pxp_types.E_comment($text_expr$) >> ]
      | (`PI(target,value),p1,p2) ->
	  let target_expr = generate_for_string_expr target in
	  let value_expr = generate_for_string_expr value in
	  let loc = mkloc p1 p2 in
	  [ `Single, <:expr< Pxp_types.E_pinstr($target_expr$,$value_expr$,_eid) >> ]
      | (`Super subnodes,p1,p2) ->
	  let subnodes_expr = generate_for_nodelist_expr nsmode subnodes in
	  let loc = mkloc p1 p2 in
	  ( [ `Single, <:expr< Pxp_types.E_start_super >> ] @
	    subnodes_expr @
	    [ `Single, <:expr< Pxp_types.E_end_super >> ] )
      | (`Meta(name,attrs,subnode),p1,p2) ->
	  let loc = mkloc p1 p2 in
	  ( match name with
		"scope"      -> generate_scope loc attrs subnode
	      | "autoscope"  -> generate_autoscope loc subnode
	      | "emptyscope" -> generate_emptyscope loc subnode
	      | _            -> assert false (* already caught above *)
	  )
      | (`Ident name,p1,p2) ->
	  let loc = mkloc p1 p2 in
	  [ `Tree, (generate_ident loc (to_src name)) ]
      | (`Anti text,p1,p2) ->
	  let expr = 
	    Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
	  in
	  [ `Tree, expr ]
      | _ ->
	  (* `Literal and `Concat are impossible after type check *)
	  assert false )

  and generate_for_nodelist_expr nsmode : 
                                 ast_node_list -> (ann * MLast.expr) list = (
    function
	(`Nodes l, p1, p2) ->
	  (* let loc = mkloc p1 p2 in *)
	  let l' = List.map (generate_for_node_expr nsmode) l in
	  List.flatten l'
      | (`Concat l, p1, p2) ->
	  (* let loc = mkloc p1 p2 in *)
	  let l' = List.map (generate_for_nodelist_expr nsmode) l in
	  List.flatten l'
      | (`Ident name, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  [ `Forest, (generate_ident loc (to_src name)) ]
      | (`Anti text, p1, p2) ->
	  let expr = 
	    Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
	  in
	  [ `Forest, expr ]
  )

  and generate_for_attr_expr : ast_attr -> [`Single|`List] * MLast.expr = (
    function
	(`Attr(n,v), p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let n_expr = generate_for_string_expr n in
	  let v_expr = generate_for_string_expr v in
	  `Single, <:expr< ($n_expr$, $v_expr$) >>
      | (`Anti text, p1, p2) ->
	  `List, 
	  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
  )

  and generate_scope loc attrs subnode : (ann * MLast.expr) list = (
    let subexpr = generate_for_node_expr true subnode in
    if attrs = [] then
      subexpr
    else
      let decl_expr_l = List.map generate_for_attr_expr attrs in
      let decl_expr = generate_ann_list loc decl_expr_l in
      let old_scope_expr = <:expr< Some scope >> in
      let scope_expr =
	<:expr< new Pxp_dtd.namespace_scope_impl 
		  (dtd # namespace_manager)
		  $old_scope_expr$
		  $decl_expr$>> in
      let compiled_subexpr = generate_tree subexpr in
      [ `Tree, ( <:expr< let scope = $scope_expr$ in $compiled_subexpr$ >> ) ]
  )

  and generate_autoscope loc subnode : (ann * MLast.expr) list = (
    let subexpr = generate_for_node_expr true subnode in
    let compiled_subexpr = generate_tree subexpr in
    let scope_expr = 
      <:expr< ( let mng = dtd # namespace_manager in
		new Pxp_dtd.namespace_scope_impl 
		  mng None mng#as_declaration ) >> in
    [ `Tree, ( <:expr< let scope = $scope_expr$ in $compiled_subexpr$ >> ) ]
  )

  and generate_emptyscope loc subnode : (ann * MLast.expr) list = (
    let subexpr = generate_for_node_expr true subnode in
    let compiled_subexpr = generate_tree subexpr in
    let scope_expr = 
      <:expr< ( let mng = dtd # namespace_manager in
		new Pxp_dtd.namespace_scope_impl 
		  mng None [] ) >> in
    [ `Tree, ( <:expr< let scope = $scope_expr$ in $compiled_subexpr$ >> ) ]
  )

  and generate_for_string_expr : ast_string -> MLast.expr = (
    function
	(`Literal s, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let s' = to_rep s in
	  <:expr< $str:s'$ >>
      | (`Concat l, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  let l' = List.map generate_for_string_expr l in
	  let l'' = generate_list loc l' in
	  <:expr< String.concat "" $l''$ >>
      | (`Ident name, p1, p2) ->
	  let loc = mkloc p1 p2 in
	  generate_ident loc (to_src name)
      | (`Anti text, p1, p2) ->
	  Grammar.Entry.parse Pcaml.expr_eoi (Stream.of_string (to_src text))
  )

  in

  catch_errors
    (fun () ->
       let stream = scan_string s in
       let ast = call_parser parse_any_expr stream in
       let ast' = check_any_expr ast in
       let loc = mkloc (1,0,0) (last_pos stream) in
       let expr = generate_for_any_expr loc ast' in
       <:expr< $anti:expr$ >>
    )
;;


let expand_evlist_expr s =
  let loc = mkloc (0,0,0) (0,0,0) in  (* ??? *)
  let rec generate_tree annlist =
    match annlist with
	(`Single, e) :: annlist' ->
	  let rest = generate_tree annlist' in
	  <:expr< [$e$ :: $rest$] >>
      | ((`Tree | `Forest), e) :: annlist' ->
	  let rest = generate_tree annlist' in
	  <:expr< $e$ @ $rest$ >>
      | [] ->
	  <:expr< [] >>
  in
  let generate_forest annlist = generate_tree annlist in
  check_file();
  generate_event_generator generate_tree generate_forest s
;;


let expand_evpull_expr s =
  let loc = mkloc (0,0,0) (0,0,0) in  (* ??? *)
  let generate_tree annlist =
    let rec generate_match k annlist =
      match annlist with
	(`Single, e) :: annlist' ->
	  ( <:patt< $int:string_of_int k$ >>, 
	    None, 
	    <:expr< let ev = $e$ in
	            do { _state.val := $int:string_of_int(k+1)$;
	                  Some ev } 
            >> ) :: generate_match (k+1) annlist'
      | ((`Tree | `Forest), e) :: annlist' ->
	  ( <:patt< $int:string_of_int k$ >>, 
	    None, 
	    <:expr< match $e$ _arg with
		    [ None -> do { _state.val := $int:string_of_int(k+1)$;
				   _generator _arg }
	            | Some Pxp_types.E_end_of_stream -> _generator _arg
                    | Some ev -> Some ev ]
            >> ) :: generate_match (k+1) annlist'
      | [] ->
	  [ <:patt< $int:string_of_int k$ >>,
	    None,
	    <:expr< None >>;

	    <:patt< _ >>,
	    None,
	    <:expr< assert False >>
	  ]
    in
    <:expr< let rec _generator =
	       let _state = ref 0 in
	       fun _arg ->
	         match _state.val with
	         [$list:generate_match 0 annlist$]
            in _generator >>
  in
  let generate_forest annlist = generate_tree annlist in
  check_file();
  generate_event_generator generate_tree generate_forest s
;;
  


(**********************************************************************)
(* Other expanders *)

let expand_charset_expr s =
  check_file();
  catch_errors
    (fun () ->
       let stream = scan_string s in
       let decl = call_parser parse_charset_decl stream in
       current_decl := decl;
       let loc = mkloc (1,0,0) (last_pos stream) in
       <:expr< () >>
    )
;;


let expand_text_expr s =
  check_file();
  let loc = mkloc (1,0,0) (1,0,String.length s) in
  <:expr< $str:s$ >>
;;


let na_pat _ =
  failwith "not available as pattern"
;;

Quotation.add
  "pxp_charset" (Quotation.ExAst(expand_charset_expr, na_pat)) ;;
Quotation.add 
  "pxp_tree" (Quotation.ExAst(expand_tree_expr false, na_pat)) ;;
Quotation.add 
  "pxp_vtree" (Quotation.ExAst(expand_tree_expr true, na_pat)) ;;
Quotation.add
  "pxp_evlist" (Quotation.ExAst(expand_evlist_expr, na_pat)) ;;
Quotation.add
  "pxp_evpull" (Quotation.ExAst(expand_evpull_expr, na_pat)) ;;
Quotation.add
  "pxp_text" (Quotation.ExAst(expand_text_expr, na_pat)) ;;
