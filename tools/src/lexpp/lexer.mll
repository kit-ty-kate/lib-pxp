(* $Id: lexer.mll,v 1.1 2002/08/28 23:48:28 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

{
(******************************************************)
(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
(*                   14/05/2000                       *)
(******************************************************)

open Parser

let comment_depth = ref 0;;

let charint_of_lexeme l =
 String.set l 0 '0' ;
 int_of_string l
;;
}

let digit = ['0'-'9']|['A'-'F']|['a'-'f']

rule token =
 parse
    [' ' '\t' '\n']                           { token lexbuf }
  | "let"                                     { LET }
  | (['a'-'z']|'_')(['a'-'z']|['A'-'Z']|'_'|['0'-'9']|'\'')*
                                              { IDENT (Lexing.lexeme lexbuf) }
  | '='                                       { EQ }
  | ";;"                                      { END_OF_LET }
  | "|"                                       { PIPE }
  | '['                                       { LBRACKET }
  | ']'                                       { RBRACKET }
  | '-'                                       { RANGE }
  | "(*"                                      { incr comment_depth ;
                                                comment lexbuf
                                              }
  | "#x" digit +                              { CHAR (charint_of_lexeme (Lexing.lexeme lexbuf)) }
  | eof                                       { EOF }

and comment =
 parse
    "(*" { incr comment_depth ; comment lexbuf }
  | "*)" { decr comment_depth ;
           if !comment_depth = 0 then token lexbuf else comment lexbuf
         }
  | _    { comment lexbuf }

(* ======================================================================
 * History:
 * 
 * $Log: lexer.mll,v $
 * Revision 1.1  2002/08/28 23:48:28  gerd
 * 	Initial revision.
 *
 * 
 *)
