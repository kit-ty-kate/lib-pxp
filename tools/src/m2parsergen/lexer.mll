(* $Id: lexer.mll,v 1.3 2000/05/09 00:03:22 gerd Exp $
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

(* ======================================================================
 * History:
 * 
 * $Log: lexer.mll,v $
 * Revision 1.3  2000/05/09 00:03:22  gerd
 * 	Added [ ml_name ] symbols, where ml_name is an arbitrary
 * OCaml identifier.
 *
 * Revision 1.2  2000/05/06 21:51:24  gerd
 * 	New symbol Dollar.
 *
 * Revision 1.1  2000/05/06 17:36:17  gerd
 * 	Initial revision.
 *
 * 
 *)
