(* $Id: pxp_codewriter.ml,v 1.10 2001/06/08 01:15:46 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_document
open Pxp_yacc
open Pxp_dtd
open Pxp_types
open Pxp_marshal

(*
let write_local_dtd out (dtd : dtd) =
  output_string out "let dtd = Pxp_yacc.parse_dtd_entity\n";
  output_string out "  config\n";
  output_string out "  (Pxp_yacc.from_string\n";
  output_string out "     ~fixenc: `Enc_utf8\n";
  output_string out "     \"";
  let b = Buffer.create 100 in
  dtd # write (Out_buffer b) `Enc_utf8 false;
  output_string out (String.escaped (Buffer.contents b));
  output_string out "\"";
  output_string out "  ) in\n";
  let root = dtd # root in
  ( match root with
	None -> ()
      | Some r ->
	  output_string out "dtd # set_root \"";
	  output_string out (String.escaped r);
	  output_string out "\"\n";
  );
  output_string out "dtd\n"
;;
*)


let rec write_local_subtree out n =
  (* Outputs the term generating the subtree *)
  output_string out "let cmds = ref [\n";
  subtree_to_cmd_sequence 
    (fun cmd ->
       let s = Marshal.to_string cmd [] in
       output_string out "\"";
       output_string out (String.escaped s);
       output_string out "\";\n";
    )
    n;
  output_string out "] in\n";
  output_string out "Pxp_marshal.subtree_from_cmd_sequence\n";
  output_string out "  ?enable_namespace_processing\n";
  output_string out "  (fun () -> match !cmds with\n";
  output_string out "     cmd :: cmds' -> cmds := cmds'; Marshal.from_string cmd 0\n";
  output_string out "   | []           -> assert false\n";
  output_string out "  )\n";
  output_string out "  dtd\n";
  output_string out "  spec\n"
;;


let write_local_document out (d : 'ext document) =
  output_string out "let cmds = ref [\n";
  document_to_cmd_sequence 
    (fun cmd ->
       let s = Marshal.to_string cmd [] in
       output_string out "\"";
       output_string out (String.escaped s);
       output_string out "\";\n";
    )
    d;
  output_string out "] in\n";
  output_string out "Pxp_marshal.document_from_cmd_sequence\n";
  output_string out "  (fun () -> match !cmds with\n";
  output_string out "     cmd :: cmds' -> cmds := cmds'; Marshal.from_string cmd 0\n";
  output_string out "   | []           -> assert false\n";
  output_string out "  )\n";
  output_string out "  config\n";
  output_string out "  spec\n";
;;


let write_document out d =
  output_string out "let create_document config spec =\n";
  write_local_document out d;
  output_string out ";;\n"
;;

(*
let write_dtd out dtd =
  output_string out "let create_dtd config =\n";
  write_local_dtd out dtd;
  output_string out ";;\n"
;;
*)

let write_subtree out t =
  output_string out "let create_subtree ?enable_namespace_processing dtd spec =\n";
  write_local_subtree out t;
  output_string out ";;\n"
;;

(* ======================================================================
 * History:
 * 
 * $Log: pxp_codewriter.ml,v $
 * Revision 1.10  2001/06/08 01:15:46  gerd
 * 	Moved namespace_manager from Pxp_document to Pxp_dtd. This
 * makes it possible that the DTD can recognize the processing instructions
 * <?pxp:dtd namespace prefix="..." uri="..."?>, and add the namespace
 * declaration to the manager.
 *
 * Revision 1.9  2001/06/07 22:39:20  gerd
 * 	Pxp_codewriter now uses Pxp_marshal to generate the code.
 * This simply reduces the complexity of the whole package a lot...
 * 	A consequence is that Pxp_codewriter works for namespaces,
 * because Pxp_marshal already does.
 *
 * Revision 1.8  2001/04/22 14:14:41  gerd
 * 	Updated to support private IDs.
 *
 * Revision 1.7  2000/08/30 15:48:07  gerd
 * 	Minor update.
 *
 * Revision 1.6  2000/08/18 20:16:59  gerd
 * 	Updates because of new node types T_comment, T_pinstr, T_super_root.
 *
 * Revision 1.5  2000/07/23 02:16:51  gerd
 * 	Changed signature of local_validate.
 *
 * Revision 1.4  2000/07/09 17:59:35  gerd
 * 	Updated: The position of element nodes is also written.
 *
 * Revision 1.3  2000/07/09 00:30:00  gerd
 * 	Notations are written before they are used.
 * 	Unparsed entities are included.
 * 	Further changes.
 *
 * Revision 1.2  2000/07/08 22:59:14  gerd
 * 	[Merging 0.2.10:] Improved: The resulting code can be compiled
 * faster, and the compiler is less hungry on memory.
 * 	Updated because of PXP interface changes.
 *
 * Revision 1.1  2000/05/29 23:48:38  gerd
 * 	Changed module names:
 * 		Markup_aux          into Pxp_aux
 * 		Markup_codewriter   into Pxp_codewriter
 * 		Markup_document     into Pxp_document
 * 		Markup_dtd          into Pxp_dtd
 * 		Markup_entity       into Pxp_entity
 * 		Markup_lexer_types  into Pxp_lexer_types
 * 		Markup_reader       into Pxp_reader
 * 		Markup_types        into Pxp_types
 * 		Markup_yacc         into Pxp_yacc
 * See directory "compatibility" for (almost) compatible wrappers emulating
 * Markup_document, Markup_dtd, Markup_reader, Markup_types, and Markup_yacc.
 *
 * ======================================================================
 * Old logs from markup_codewriter.ml:
 *
 * Revision 1.1  2000/03/11 22:57:28  gerd
 * 	Initial revision.
 *
 * 
 *)
