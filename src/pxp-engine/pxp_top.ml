(* $Id$
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


