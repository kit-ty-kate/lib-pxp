(* $Id: exprlex.mll,v 1.1 2002/08/03 17:39:43 gerd Exp $
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


(* ======================================================================
 * History:
 * 
 * $Log: exprlex.mll,v $
 * Revision 1.1  2002/08/03 17:39:43  gerd
 * 	Initial revision.
 *
 * Revision 1.1  2002/07/14 23:02:51  gerd
 * 	Initial revision.
 *
 * 
 *)
