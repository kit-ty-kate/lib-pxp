(* $Id: test_write.ml,v 1.3 2001/05/17 22:35:46 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Pxp_document;;
open Pxp_yacc;;
open Pxp_types;;

let error_happened = ref false;;

let rec prerr_error e =
  prerr_endline (string_of_exn e)
;;

class warner =
  object 
    method warn w =
      prerr_endline ("WARNING: " ^ w)
  end
;;

let parse_and_write in_filename =
  let spec =
    let e = new element_impl default_extension in
    make_spec_from_mapping
      ~super_root_exemplar:      (new super_root_impl default_extension)
      ~default_pinstr_exemplar:  (new pinstr_impl default_extension)
      ~data_exemplar:            (new data_impl default_extension)
      ~default_element_exemplar: e
      ~element_mapping:          (Hashtbl.create 1)
      ()
  in
  let config =
      { default_config with 
	  warner = new warner;
	  enable_pinstr_nodes = true;
	  enable_super_root_node = true;
	  encoding = `Enc_utf8;
      }
  in
  try 
    let tree =
      parse_document_entity
        config
	(from_file in_filename)
	spec 
    in
    
    tree # write (Out_channel stdout) `Enc_utf8;
  with
      e ->
	error_happened := true;
	prerr_error e
;;


let main() =
  let in_file = ref "" in
  Arg.parse
      [ "-in", (Arg.String (fun s -> in_file := s)),
            " <file>      Set the XML file to read";
      ]
      (fun x -> raise (Arg.Bad "Unexpected argument"))
      "
usage: test_write [ options ]

List of options:";
  if !in_file = "" then begin
    prerr_endline "No input file specified.";
    exit 1
  end;
  parse_and_write !in_file 
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: test_write.ml,v $
 * Revision 1.3  2001/05/17 22:35:46  gerd
 * 	Updated.
 *
 * Revision 1.2  2000/08/16 23:44:21  gerd
 * 	Updates because of changes of the PXP API.
 *
 * Revision 1.1  2000/07/16 17:50:39  gerd
 * 	Initial revision.
 *
 *)
