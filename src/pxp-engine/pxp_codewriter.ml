(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_document
open Pxp_yacc
open Pxp_dtd
open Pxp_core_types
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

