(* $Id: test_write.ml,v 1.1 2000/07/16 17:50:39 gerd Exp $
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
      ~data_exemplar:            (new data_impl default_extension)
      ~default_element_exemplar: e
      ~element_mapping:          (Hashtbl.create 1)
  in
  let config =
      { default_config with 
	  warner = new warner;
	  processing_instructions_inline = true;
	  virtual_root = true;
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
 * Revision 1.1  2000/07/16 17:50:39  gerd
 * 	Initial revision.
 *
 *)
