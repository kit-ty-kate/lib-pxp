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


let compile in_filename out_filename print super_root pis comments =
  let spec =
    let e = new element_impl default_extension in
    make_spec_from_mapping
      ~super_root_exemplar:      (new super_root_impl default_extension)
      ~default_pinstr_exemplar:  (new pinstr_impl default_extension)
      ~comment_exemplar:         (new comment_impl default_extension)
      ~data_exemplar:            (new data_impl default_extension)
      ~default_element_exemplar: e
      ~element_mapping:          (Hashtbl.create 1)
      ()
  in
  let config =
      { default_config with 
	  encoding = `Enc_utf8;
	  warner = new warner;
	  enable_super_root_node = super_root;
	  enable_pinstr_nodes = pis;
	  enable_comment_nodes = comments;
      }
  in
  try 
    let tree =
      parse_document_entity
        config
	(from_file in_filename)
	spec 
    in
    
    let ch = open_out out_filename in
    Pxp_codewriter.write_document ch tree;
    output_string ch "(create_document { Pxp_yacc.default_config with Pxp_yacc.encoding = `Enc_utf8 } Pxp_yacc.default_spec) # write (`Out_channel stdout) `Enc_utf8;;\n";
    close_out ch;

    if print then
      tree # write (`Out_channel stdout) `Enc_utf8;
  with
      e ->
	error_happened := true;
	prerr_error e
;;


let main() =
  let in_file = ref "" in
  let out_file = ref "" in
  let print_file = ref false in
  let super_root = ref false in
  let pis = ref false in
  let comments = ref false in
  Arg.parse
      [ "-in", (Arg.String (fun s -> in_file := s)),
            " <file>      Set the XML file to read";
	"-out", (Arg.String (fun s -> out_file := s)),
	     " <file>     Set the Ocaml file to write";
	"-print", (Arg.Set print_file),
	       "          Print the XML file in standard form";
	"-super-root", Arg.Set super_root,
	            "     Generate a super root node";
	"-pis", Arg.Set pis,
	     "            Generate wrapper nodes for processing instructions";
	"-comments", Arg.Set comments,
	          "       Generate nodes for comments";
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
  compile !in_file !out_file !print_file !super_root !pis !comments
;;


main();
if !error_happened then exit(1);;

