(* $Id: pxp_top.ml,v 1.1 2001/06/09 22:15:04 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


let exec s =
  let l = Lexing.from_string s in
  let ph = !Toploop.parse_toplevel_phrase l in
  assert(Toploop.execute_phrase false Format.err_formatter ph)
;;


exec "#install_printer Pxp_document.print_node;;";;
exec "#install_printer Pxp_document.print_doc;;";;


(* ======================================================================
 * History:
 * 
 * $Log: pxp_top.ml,v $
 * Revision 1.1  2001/06/09 22:15:04  gerd
 * 	Added Pxp_top.
 *
 * 
 *)
