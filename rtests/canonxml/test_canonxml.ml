(* $Id: test_canonxml.ml,v 1.12 2003/06/21 12:15:19 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Pxp_document;;
open Pxp_yacc;;
open Pxp_types;;
open Pxp_lexer_types;;

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
      T_super_root ->
	n # iter_nodes (output_xml config)
    | T_pinstr pi_name ->
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
    | T_comment ->
	let v =
	  match n # comment with
	      None -> assert false
	    | Some x -> x
	in
	output_utf8 config ("<!--" ^ v ^ "-->")
    | _ -> 
	assert false
;;


let parse debug wf iso88591 comments eb_atts filename =
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
  let escape_atts tok pos mng =
    match tok with
	Lcurly -> "{"
      | LLcurly -> "{{"
      | Rcurly -> "}"
      | RRcurly -> "}}"
      | _ -> assert false
  in
  let config =
      { default_config with 
	  warner = new warner;
	  debugging_mode = debug;
	  enable_pinstr_nodes = true;
	  enable_super_root_node = true;
	  enable_comment_nodes = comments;
	  drop_ignorable_whitespace = false;
	  encoding = if iso88591 then `Enc_iso88591 else `Enc_utf8;
	  idref_pass = true;
	  escape_attributes = if eb_atts then Some escape_atts else None;
      }
  in
  try 
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
  let comments = ref false in
  let eb_atts = ref false in
  let files = ref [] in
  Arg.parse
      [ "-d",   Arg.Set debug, 
	   "                 turn debugging mode on";
	"-wf",  Arg.Set wf,    
            "                check only on well-formedness";
	"-iso-8859-1", Arg.Set iso88591, 
                    "        use ISO-8859-1 as internal encoding instead of UTF-8";
	"-comments", Arg.Set comments, 
	          "          output comments, too";
	"-event-based-atts", Arg.Set eb_atts,
	                  "  use the event-based attribute parsing algorithm";
      ]
      (fun x -> files := x :: !files)
      "
usage: test_canonxml [options] file ...

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf !iso88591 !comments !eb_atts) !files;
;;


main();
if !error_happened then exit(1);;

(* ======================================================================
 * History:
 * 
 * $Log: test_canonxml.ml,v $
 * Revision 1.12  2003/06/21 12:15:19  gerd
 * 	Updates because of changed API
 *
 * Revision 1.11  2002/08/05 22:35:44  gerd
 * 	Testing escape_attributes option.
 *
 * Revision 1.10  2001/06/13 18:42:00  gerd
 * 	Updated
 *
 * Revision 1.9  2001/05/17 22:31:03  gerd
 * 	Updated
 *
 * Revision 1.8  2000/08/17 00:51:57  gerd
 * 	Added -comments option to test enable_comment_nodes.
 *
 * Revision 1.7  2000/08/16 23:44:17  gerd
 * 	Updates because of changes of the PXP API.
 *
 * Revision 1.6  2000/07/14 14:56:55  gerd
 * 	Updated: warner.
 *
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
