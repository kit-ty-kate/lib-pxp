(* $Id: pxp_lexing.ml,v 1.1 2002/08/28 23:04:38 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(* This is a version of lexing.ml (from stdlib) that can cope with large
 * tokens.
 *)

open Lexing

type lexbuf = Lexing.lexbuf

let lex_refill read_fun aux_buffer lexbuf =
  let read =
    read_fun aux_buffer (String.length aux_buffer) in
  let n =
    if read > 0
    then read
    else (lexbuf.lex_eof_reached <- true; 0) in
  (* Is there enough space at the end of the buffer? *)
  let space = String.length lexbuf.lex_buffer - lexbuf.lex_buffer_len in
  if space < n then begin
    (* No *)
    (* First try to remove the first lex_start_pos bytes of the buffer *)
    let s = lexbuf.lex_start_pos in
    let space' = space - s in
    let efflen = lexbuf.lex_buffer_len - s in
    if space' >= n then begin
      (* There is enough space at the beginning of the buffer *)
      String.(*unsafe_*)blit lexbuf.lex_buffer s lexbuf.lex_buffer 0 efflen;
    end
    else begin
      (* Allocate a bigger buffer *)
      let oldlen = String.length lexbuf.lex_buffer in
      let newlen = max (oldlen * 2) (n + efflen) in
      let newlen = 
	if newlen > Sys.max_string_length && n+efflen <= Sys.max_string_length
	then Sys.max_string_length
	else newlen in
      let newbuf = String.create newlen in
      String.(*unsafe_*)blit lexbuf.lex_buffer s newbuf 0 efflen;
      lexbuf.lex_buffer <- newbuf;
    end;
    (* Anyway, the first s bytes have been removed. Update the positions. *)
    lexbuf.lex_abs_pos <- lexbuf.lex_abs_pos + s;
    lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - s;
    lexbuf.lex_start_pos <- 0;
    lexbuf.lex_last_pos <- lexbuf.lex_last_pos - s;
    lexbuf.lex_buffer_len <- efflen;
  end;
  (* There is now enough space at the end of the buffer *)
  String.unsafe_blit aux_buffer 0
                     lexbuf.lex_buffer lexbuf.lex_buffer_len
                     n;
  lexbuf.lex_buffer_len <- lexbuf.lex_buffer_len + n

let from_function f =
  { refill_buff = lex_refill f (String.create 512);
    lex_buffer = String.create 1024;
    lex_buffer_len = 0;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_eof_reached = false }

let from_channel ic =
  from_function (fun buf n -> input ic buf 0 n)

let from_string s =
  { refill_buff = (fun lexbuf -> lexbuf.lex_eof_reached <- true);
    lex_buffer = s ^ "";
    lex_buffer_len = String.length s;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_eof_reached = true }

let lexeme = Lexing.lexeme
let lexeme_char = Lexing.lexeme_char
let lexeme_start = Lexing.lexeme_start
let lexeme_end = Lexing.lexeme_end

let from_string_inplace s =
  (* avoids copying s *)
  { refill_buff = (fun lexbuf -> lexbuf.lex_eof_reached <- true);
    lex_buffer = s;   (* instead of s ^ "" *)
    lex_buffer_len = String.length s;
    lex_abs_pos = 0;
    lex_start_pos = 0;
    lex_curr_pos = 0;
    lex_last_pos = 0;
    lex_last_action = 0;
    lex_eof_reached = true }
;;

let from_another_string_inplace lexbuf s =
  (* uses lexbuf again for another string (avoids memory allocation) *)
  lexbuf.lex_buffer <- s;
  lexbuf.lex_buffer_len <- String.length s;
  lexbuf.lex_abs_pos <- 0;
  lexbuf.lex_start_pos <- 0;
  lexbuf.lex_curr_pos <- 0;
  lexbuf.lex_last_pos <- 0;
  lexbuf.lex_last_action <- 0;
  lexbuf.lex_eof_reached <- true
;;


let sub_lexeme lexbuf k l =
  (* = String.sub (Lexing.lexeme lexbuf) k l *)
  let lexeme_len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
  if (k < 0 || k > lexeme_len || l < 0 || k+l > lexeme_len) then
    invalid_arg "sub_lexeme";
  let s = String.create l in
  String.unsafe_blit 
    lexbuf.lex_buffer (lexbuf.lex_start_pos + k) s 0 l;
  s
;;

(* ======================================================================
 * History:
 * 
 * $Log: pxp_lexing.ml,v $
 * Revision 1.1  2002/08/28 23:04:38  gerd
 * 	Removed the lex_buffer_len stuff
 *
 * Revision 1.4  2002/03/15 16:14:40  gerd
 * 	Fixed the max_string_length bug.
 *
 * Revision 1.3  2002/03/13 22:45:42  gerd
 * 	Improved Pxp_lexing.
 *
 * Revision 1.2  2002/03/13 22:26:08  gerd
 * 	Added some functions from Pxp_lexer_types.
 *
 * Revision 1.1  2002/02/20 00:24:54  gerd
 * 	Initial revision.
 *
 * 
 *)