(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


open Pxp_document;;
open Pxp_yacc;;
open Pxp_types;;

let error_happened = ref false;;

let rec print_error e =
  print_endline (string_of_exn e)
;;

class warner =
  object 
    method warn w =
      print_endline ("WARNING: " ^ w)
  end
;;

let parse debug wf iso88591 filename =
  try 
  let config =
      { default_config with 
	  warner = new warner;
          debugging_mode = debug;
          encoding = if iso88591 then `Enc_iso88591 else `Enc_utf8;
	  idref_pass = true;
      }
  in
    let parse_fn =
      if wf then parse_wfdocument_entity config
      else 
	let index = new hash_index in
	parse_document_entity 
	  ~id_index:(index :> 'ext index)
	  config
    in
    let tree =
      parse_fn
	(from_file filename)
	default_spec
    in
    print_endline "Parsed without error";
  with
      e ->
	error_happened := true;
	print_error e
;;


let main() =
  let debug = ref false in
  let wf = ref false in
  let iso88591 = ref false in
  let files = ref [] in
  Arg.parse
      [ "-d",   Arg.Set debug, "turn debugging mode on";
	"-wf",  Arg.Set wf,    "check only on well-formedness";
        "-iso-8859-1", Arg.Set iso88591, "use ISO-8859-1 as internal encoding instead of UTF-8";
      ]
      (fun x -> files := x :: !files)
      "
usage: test_negative [options] file ...

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf !iso88591) !files;
;;


main();
if !error_happened then exit(1);;

