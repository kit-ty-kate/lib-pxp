(* $Id$
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
    let e = new namespace_element_impl default_extension in
    let r = new super_root_impl default_extension in
    make_spec_from_mapping
      ~super_root_exemplar:      r
      ~default_pinstr_exemplar:  e
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
	  enable_namespace_processing = Some(new Pxp_dtd.namespace_manager);
	  encoding = `Enc_iso88591;
      }
  in
  try 
    let tree =
      parse_document_entity
        config
	(from_file in_filename)
	spec 
    in
    
    tree # write (`Out_channel stdout) `Enc_iso88591;
  with
      Not_found as e ->
	raise e
    | e ->
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
usage: test_namespace [ options ]

List of options:";
  if !in_file = "" then begin
    prerr_endline "No input file specified.";
    exit 1
  end;
  parse_and_write !in_file 
;;


main();
if !error_happened then exit(1);;

