(* $Id: number.mll,v 1.1 2002/07/14 23:02:51 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

{
  type token =
      Space
    | Newline
    | Number of int
    | Stop
    | Other

} 

rule scan_number = parse
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
  | '}'
      { Stop }
  | _
      { Other }


(* ======================================================================
 * History:
 * 
 * $Log: number.mll,v $
 * Revision 1.1  2002/07/14 23:02:51  gerd
 * 	Initial revision.
 *
 * 
 *)
