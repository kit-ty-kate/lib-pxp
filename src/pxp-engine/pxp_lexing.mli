(* $Id: pxp_lexing.mli,v 1.1 2002/03/13 22:25:51 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* The following types and functions are the same as in the module
 * Lexing of the stdlib. For some versions of O'Caml, some of the functions
 * have a more efficient implementation.
 *)

type lexbuf = Lexing.lexbuf

val from_channel : in_channel -> lexbuf

val from_string : string -> lexbuf

val from_function : (string -> int -> int) -> lexbuf

val lexeme : lexbuf -> string

val lexeme_char : lexbuf -> int -> char

val lexeme_start : lexbuf -> int

val lexeme_end : lexbuf -> int

(* Extensions: *)

val from_string_inplace : string -> Lexing.lexbuf
  (* Similar to Lexing.from_string, but does not copy the passed string
   * intially
   *)

val from_another_string_inplace : Lexing.lexbuf -> string -> unit
  (* lexbuf: a buffer from a previous Lexing.from_string 
   * (or from_string_inplace).
   * Modifies lexbuf such that the lexer starts again with the passed string
   *)

val sub_lexeme : Lexing.lexbuf -> int -> int -> string
  (* Same as String.sub (Lexing.lexeme lexbuf) k l, but avoids one string
   * allocation
   *)


(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexing.mli,v $
 * Revision 1.1  2002/03/13 22:25:51  gerd
 * 	Initial revision.
 *
 * 
 *)
