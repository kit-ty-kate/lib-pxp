(* $Id$
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

val from_function : (Bytes.t -> int -> int) -> lexbuf

val lexeme : lexbuf -> string

val lexeme_char : lexbuf -> int -> char

(* val lexeme_start : lexbuf -> int *)   (* Removed because of wlex problems *)

(* val lexeme_end : lexbuf -> int *)     (* Removed because of wlex problems *)

(* Extensions: *)

val lexeme_len : lexbuf -> int
  (* = String.length(lexeme lexbuf) *)

val from_bytes_inplace : Bytes.t -> Lexing.lexbuf
  (* Similar to Lexing.from_string, but does not copy the passed string
   * intially
   *)

val sub_lexeme : Lexing.lexbuf -> int -> int -> string
  (* Same as String.sub (Lexing.lexeme lexbuf) k l, but avoids one string
   * allocation
   *)


