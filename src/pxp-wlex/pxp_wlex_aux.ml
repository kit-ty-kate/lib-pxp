(* $Id: pxp_wlex_aux.ml,v 1.1 2000/09/17 00:14:06 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* NOTE: Currently, this module is *identical* to Pxp_lex_aux_utf8 *)

  open Pxp_types
  open Pxp_lexer_types

  let get_name_end s k =
    (* Get the index of the end+1 of the name beginning at position k *)
    let l = String.length s in
    let rec find j =
      if j < l then
	match s.[j] with
	  | ('\009'|'\010'|'\013'|'\032') -> j
	  |_                              -> find (j+1)
      else
	l
    in
    find k

  let get_ws_end s k =
    let l =  String.length s in
    let rec find j =
      if j < l then
	match s.[j] with
	    (' '|'\t'|'\r'|'\n') -> find (j+1)
	  | _                    -> j
      else
	l
    in
    find k

  let scan_pi pi xml_scanner =
    let s = String.sub pi 2 (String.length pi - 4) in
            (* the PI without the leading "<?" and the trailing "?>" *)
    let xml_lexbuf = Lexing.from_string (s ^ " ") in
      (* Add space because the lexer expects whitespace after every
       * clause; by adding a space there is always whitespace at the 
       * end of the string.
       *)

    (* The first word of a PI must be a name: Extract it. *)

    let s_name, s_len =
      match xml_scanner xml_lexbuf with
	  Pro_name n -> 
	    let ltok = String.length (Lexing.lexeme xml_lexbuf) in
	    if String.length n = ltok then
              (* No whitespace after the name *)
	      raise (WF_error ("Bad processing instruction"));
	    n, ltok
	| _ -> raise (WF_error ("Bad processing instruction"))
    in

    (* Note: s_len is the length of s_name + the whitespace following s_name *)

    match s_name with
	"xml" -> begin
	  (* It is a <?xml ...?> PI: Get the other tokens *)
	  let rec collect () =
	    let t = xml_scanner xml_lexbuf in
	    (* prerr_endline (string_of_int (Lexing.lexeme_end xml_lexbuf)); *)
	    if t = Pro_eof then
	      []
	    else
	      t :: collect()
	  in
	  PI_xml (collect())
	end
      | _ -> 
	  let len_param = String.length s - s_len in
	  (* It is possible that len_param = -1 *)
	  if len_param >= 1 then
	    PI(s_name, String.sub s s_len len_param)
	  else
	    PI(s_name, "")



(* ======================================================================
 * History:
 * 
 * $Log: pxp_wlex_aux.ml,v $
 * Revision 1.1  2000/09/17 00:14:06  gerd
 * 	Initial revision.
 *
 * Revision 1.2  2000/05/29 23:53:12  gerd
 * 	Updated because Markup_* modules have been renamed to Pxp_*.
 *
 * Revision 1.1  2000/05/20 20:33:25  gerd
 * 	Initial revision.
 *
 * 
 *)
