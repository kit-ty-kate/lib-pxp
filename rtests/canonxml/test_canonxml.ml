(* $Id: test_canonxml.ml,v 1.5 2000/07/14 14:17:58 gerd Exp $
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
    method print_warnings =
      ""
    method reset =
      ()
  end
;;

let outbuf = String.create 8192;;

let output_utf8 config s =
  match config.encoding  with
      `Enc_utf8 ->
	print_string s
    | `Enc_iso88591 ->
	for i = 0 to String.length s - 1 do
	  let c = Char.code(s.[i]) in
	  if c <= 127 then
	    print_char(Char.chr(c))
	  else begin
	    print_char(Char.chr(0xc0 lor (c lsr 6)));
	    print_char(Char.chr(0x80 lor (c land 0x3f)));
	  end
	done
    | _ -> assert false
;;


let re = Str.regexp "[&<>\"\009\010\013]";;

let escaped s =
  Str.global_substitute 
    re
    (fun _ ->
       match Str.matched_string s with
	   "&"    -> "&amp;"
	 | "<"    -> "&lt;"
	 | ">"    -> "&gt;"
	 | "\""   -> "&quot;"
	 | "\009" -> "&#9;"
	 | "\010" -> "&#10;"
	 | "\013" -> "&#13;"
	 | _      -> assert false
    )
    s
;;


let rec output_xml config n =
  match n # node_type with
      T_element "-vr" ->
	n # iter_nodes (output_xml config)
    | T_element "-pi" ->
	let [ pi_name ] = n # pinstr_names in
	let [ pi ] = n # pinstr pi_name in
	output_utf8 config "<?";
	output_utf8 config (pi # target);
	output_utf8 config " ";
	output_utf8 config (pi # value);
	output_utf8 config "?>";
    | T_element name ->
	output_utf8 config "<";
	output_utf8 config name;
	let sorted_attnames = 
	  Sort.list ( <= ) (n # attribute_names) in
	List.iter
	  (fun attname ->
	     match n # attribute attname with
		 Value v ->
		   output_utf8 config " ";
		   output_utf8 config attname;
		   output_utf8 config "=\"";
		   output_utf8 config (escaped v);
		   output_utf8 config "\"";
	       | Valuelist vl ->
		   let v = String.concat " " vl in
		   output_utf8 config " ";
		   output_utf8 config attname;
		   output_utf8 config "=\"";
		   output_utf8 config (escaped v);
		   output_utf8 config "\"";
	       | Implied_value -> 
		   ()
	  )
	  sorted_attnames;
	output_utf8 config ">";
	n # iter_nodes (output_xml config);
	output_utf8 config "</";
	output_utf8 config name;
	output_utf8 config ">";
    | T_data ->
	let v = n # data in
	output_utf8 config (escaped v)
;;


let parse debug wf iso88591 filename =
  let spec =
    let e = new element_impl default_extension in
    e # keep_always_whitespace_mode;
    make_spec_from_mapping
      ~data_exemplar:            (new data_impl default_extension)
      ~default_element_exemplar: e
      ~element_mapping:          (Hashtbl.create 1)
  in
  let config =
      { default_config with 
	  warner = new warner;
	  debugging_mode = debug;
	  processing_instructions_inline = true;
	  virtual_root = true;
	  encoding = if iso88591 then `Enc_iso88591 else `Enc_utf8;
	  idref_pass = true;
      }
  in
  try 
    let parse_fn =
      if wf then parse_wfdocument_entity 
      else 
	let index = new hash_index in
	parse_document_entity 
	  ?transform_dtd:None 
	  ~id_index:(index :> 'ext index)
    in
    let tree =
      parse_fn
        config
	(from_file filename)
	spec 
    in
    output_xml config (tree # root)
  with
      e ->
	error_happened := true;
	prerr_error e
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
usage: test_canonxml [options] file ...

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf !iso88591) !files;
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: test_canonxml.ml,v $
 * Revision 1.5  2000/07/14 14:17:58  gerd
 * 	Updated because of iterface changes.
 *
 * Revision 1.4  2000/07/09 01:06:20  gerd
 * 	Updated.
 *
 * Revision 1.3  2000/06/04 20:31:03  gerd
 * 	Updates because of renamed PXP modules.
 *
 * Revision 1.2  2000/05/20 20:34:28  gerd
 * 	Changed for UTF-8 support.
 *
 * Revision 1.1  2000/04/30 20:13:01  gerd
 * 	Initial revision.
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
