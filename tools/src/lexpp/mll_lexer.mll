(* $Id$ *)

{
  type mlltok =
      [ `Brace of Lexing.position * mlltok list
      | `Bracket of mlltok list
      | `Char of char
      | `Charliteral of string
      | `Comment of mlltok list
      | `EOF
      | `E_Brace
      | `E_Bracket
      | `E_Comment
      | `E_Paren
      | `Ident of string
      | `Paren of mlltok list
      | `Stringliteral of string
      | `Sep of string  (* "" or white space string *)
      ]

  let rec recurse_until stop_tok f lexbuf =
    let tok = f lexbuf in
    if tok = stop_tok then
      []
    else
      if tok = `EOF then
	failwith "Unexpected end of file"
      else
	tok :: (recurse_until stop_tok f lexbuf)
}

let ident_start = [ 'A'-'Z' 'a'-'z' '_' ]
let ident_rest  = [ 'A'-'Z' 'a'-'z' '_' '\'' '0'-'9' ]

rule definition = parse
    [' ' '\013' '\009' '\012' '\010' ]+
      { let s = Lexing.lexeme lexbuf in
	`Sep s
      }
  | "(*"
      { let r =
	  `Comment (recurse_until `E_Comment definition lexbuf) in
	r
      }
  | "*)"
      { `E_Comment }
  | '{'
      { let p = Lexing.lexeme_start_p lexbuf in
	`Brace (p, recurse_until `E_Brace definition lexbuf) }
  | '}'
      { `E_Brace }
  | '('
      { `Paren (recurse_until `E_Paren definition lexbuf) }
  | ')'
      { `E_Paren }
  | '['
      { `Bracket (recurse_until `E_Bracket definition lexbuf) }
  | ']'
      { `E_Bracket }
  | '"'
      { let buf = Buffer.create 256 in
	while stringliteral buf lexbuf do () done;
	`Stringliteral (Buffer.contents buf)
      }
  | '\'' [ ^ '\\' ] '\''
      { let s = Lexing.lexeme lexbuf in
	`Charliteral (String.sub s 1 1)
      }
  | '\'' '\\' ( [ 'n' 't' 'b' 'r' '"' '\\' '\'' ] |
		( ['0'-'9'] ['0'-'9'] ['0'-'9'] ) |
		( 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] ) )
    '\''
      { let s = Lexing.lexeme lexbuf in
	`Charliteral (String.sub s 1 (String.length s - 2))
      }
  | ident_start ident_rest*
      { `Ident (Lexing.lexeme lexbuf) }
  | _
      { let s = Lexing.lexeme lexbuf in
	`Char s.[0]
      }
  | eof
      { `EOF }


and stringliteral buf = parse
    '"'
      { false }
  | [ ^ '\\' '"' ] +
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
	true
      }
  | '\\' ("\010" | "\013" | "\013\010") [ ' ' '\t' ] *
      { true
      }
  | '\\' ( [ 'n' 't' 'b' 'r' '"' '\\' '\'' ] |
	 ( ['0'-'9'] ['0'-'9'] ['0'-'9'] ) |
         ( 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] ) )
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
	true
      }
  | '\\' _
      { failwith "illegal backslash escape" }
  | eof
      { failwith "Unexpected end of file" }

