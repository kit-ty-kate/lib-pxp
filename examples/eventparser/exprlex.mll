(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

{ open Expr }

rule scan_expr = parse
    [ ' ' '\t' ]+ 
      { Space }
  | '\013' '\010'
      { Newline }
  | '\013'
      { Newline }
  | '\010'
      { Newline }
  | [ '0'-'9' ]+
      { Number(int_of_string(Lexing.lexeme lexbuf)) }
  | '+'
      { Add }
  | '-'
      { Sub }
  | '*'
      { Mult }
  | '/'
      { Div }
  | '('
      { LParen }
  | ')'
      { RParen }
  | '}'
      { Stop }


