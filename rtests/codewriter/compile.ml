(* $Id: compile.ml,v 1.1 2000/07/09 00:33:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Pxp_document;;
open Pxp_yacc;;
open Pxp_types;;

let error_happened = ref false;;

let rec prerr_error e =
  match e with
      Pxp_types.At(where,what) ->
	prerr_endline where;
	prerr_error what
    | _ ->
	prerr_endline (Printexc.to_string e)
;;


let compile in_filename out_filename print =
  let spec =
    let e = new element_impl default_extension in
    make_spec_from_mapping
      ~data_exemplar:            (new data_impl default_extension)
      ~default_element_exemplar: e
      ~element_mapping:          (Hashtbl.create 1)
  in
  let config =
      { default_config with 
	  processing_instructions_inline = false;
	  virtual_root = false;
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
    let s = config.warner # print_warnings in
    if s <> "" then prerr_endline s;
    config.warner # reset;
    
    let ch = open_out out_filename in
    Pxp_codewriter.write_document ch tree;
    output_string ch "(create_document (new Pxp_types.drop_warnings) Pxp_yacc.default_spec) # write_compact_as_latin1 (Pxp_types.Out_channel stdout);;\n";
    close_out ch;

    if print then
      tree # write_compact_as_latin1 (Out_channel stdout);
  with
      e ->
	let s = config.warner # print_warnings in
	if s <> "" then prerr_endline s;
	config.warner # reset;
	error_happened := true;
	prerr_error e
;;


let main() =
  let in_file = ref "" in
  let out_file = ref "" in
  let print_file = ref false in
  Arg.parse
      [ "-in", (Arg.String (fun s -> in_file := s)),
            " <file>      Set the XML file to read";
	"-out", (Arg.String (fun s -> out_file := s)),
	     " <file>     Set the Ocaml file to write";
	"-print", (Arg.Set print_file),
	       "          Print the XML file in standard form";
      ]
      (fun x -> raise (Arg.Bad "Unexpected argument"))
      "
usage: compile [ options ]

List of options:";
  if !in_file = "" then begin
    prerr_endline "No input file specified.";
    exit 1
  end;
  if !out_file = "" then begin
    prerr_endline "No output file specified.";
    exit 1
  end;
  compile !in_file !out_file !print_file
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: compile.ml,v $
 * Revision 1.1  2000/07/09 00:33:32  gerd
 * 	Initial revision.
 *
 *)
