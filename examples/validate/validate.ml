(* $Id: validate.ml,v 1.9 2000/07/14 14:57:30 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Pxp_document;;
open Pxp_yacc;;
open Pxp_types;;

let error_happened = ref false;;

let print_error e =
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
    (* Parse the document: *)
    let parse_fn =
      if wf then parse_wfdocument_entity 
      else 
	let index = new hash_index in
	parse_document_entity 
	  ?transform_dtd:None 
	  ~id_index:(index :> 'ext index)
    in
    let doc =
      parse_fn
	  { default_config with 
	      debugging_mode = debug;
	      encoding = if iso88591 then `Enc_iso88591 else `Enc_utf8;
	      idref_pass = true;
	      warner = new warner
          }
	  (from_file filename)
	  default_spec 
    in
    ()
  with
      e ->
	(* Print error; remember that there was an error *)
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
usage: validate [options] file ...

- checks the validity of XML documents. See below for list of options.

<title>PXP - The XML parser for Objective Caml</title>

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf !iso88591) !files;
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: validate.ml,v $
 * Revision 1.9  2000/07/14 14:57:30  gerd
 * 	Updated: warner
 *
 * Revision 1.8  2000/07/14 14:13:15  gerd
 * 	Cosmetic changes.
 *
 * Revision 1.7  2000/07/14 14:11:06  gerd
 * 	Updated because of changes of the PXP API.
 *
 * Revision 1.6  2000/07/08 21:53:00  gerd
 * 	Updated because of PXP interface changes.
 *
 * Revision 1.5  2000/06/04 20:21:55  gerd
 * 	Updated to new module names.
 *
 * Revision 1.4  2000/05/01 16:44:57  gerd
 * 	Added check for ID uniqueness.
 * 	Using new error formatter.
 *
 * Revision 1.3  1999/11/09 22:27:30  gerd
 * 	The programs returns now an exit code of 1 if one of the
 * XML files produces an error.
 *
 * Revision 1.2  1999/09/01 23:09:56  gerd
 * 	Added the option -wf that switches to well-formedness checking
 * instead of validation.
 *
 * Revision 1.1  1999/08/14 22:20:53  gerd
 * 	Initial revision.
 *
 * 
 *)
