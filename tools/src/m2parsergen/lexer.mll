(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

{
  open Parser
}

rule scan_file = parse
    "/*" [^ '*']* ('*'+ [^ '/' '*'] [^ '*']* )* '*'* "*/"
      { Space }
  | "%token"
      { Token }
  | "<" [' ' '\t' '\r' '\n']* ">"
      { Type 
      }
  | [ 'a'-'z' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]*
      { let s = Lexing.lexeme lexbuf in
	Lname s
      }
  | [ 'A'-'Z' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]*
      { let s = Lexing.lexeme lexbuf in
	Uname s
      }
  | "%%"
      { Separator }
  | "("
      { Lparen }
  | ","
      { Comma }
  | ")"
      { Rparen }
  | "[" 
      { Lbracket }
  | "]" 
      { Rbracket }
  | ":"
      { Colon }
  | "{{" [^ '}']* ( '}' [^ '}']+ )* "}}"
      { let s = Lexing.lexeme lexbuf in
	Code (String.sub s 2 (String.length s - 4), 0, 0)
      }
  | "?"
      { Error }
  | "|"
      { Alt }
  | "+"
      { Loop_plus }
  | "*"
      { Loop_star }
  | [' ' '\t' '\r' '\n']+
      { Space }
  | "$"
      { Dollar }
  | eof
      { Eof }

and scan_header = parse
    "%%"
      { Separator }
  | "%"
      { Code("%", 0, 0) }
  | [^ '%']*
      { Code(Lexing.lexeme lexbuf, 0, 0) }
  | eof
      { Eof }

and scan_rest = parse
    _*
      { Code(Lexing.lexeme lexbuf, 0, 0) }
  | eof 
      { Eof }

