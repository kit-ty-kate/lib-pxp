(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


open Pxp_document;;
open Pxp_tree_parser;;
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

let resolve_by_helper scheme program sends_mime_header =
  let url_syntax =
    { Neturl.ip_url_syntax with 
	Neturl.url_accepts_8bits = true
    }
  in
  let get_url id =
    (* Only accept SYSTEM Ids with the right scheme: *)
    match id with
	System sysid ->
	  ( try
	      let sysid_scheme = 
		try Neturl.extract_url_scheme sysid 
		with Neturl.Malformed_URL -> scheme
		     (* If no scheme found: assume our own scheme *)
	      in
	      if sysid_scheme = scheme then
		Neturl.url_of_string url_syntax sysid  (* or Malformed_URL *)
	      else
		raise Pxp_reader.Not_competent
	    with
		(* If the URL is syntactically wrong, do not accept it: *)
		Neturl.Malformed_URL -> raise Pxp_reader.Not_competent
	  )
      | _ ->
	  raise Pxp_reader.Not_competent
  in
  let read_mime_header ch =
    let empty_re = Str.regexp "^[ \t\r\n]*$" in
    let is_empty s = Str.string_match empty_re s 0 in
    let buffer = Buffer.create 1024 in
    let line = ref(input_line ch) in
    if String.length !line >= 6 && String.sub !line 0 5 = "HTTP/" then
      line := input_line ch;
    while not (is_empty !line) do
      Buffer.add_string buffer !line;
      Buffer.add_string buffer "\n";
      line := input_line ch
    done;
    Buffer.add_string buffer "\n";
    Buffer.contents buffer
  in
  let open_channel id url =
    let url_string = Neturl.string_of_url url in
    let command = program ^ " " ^ Filename.quote url_string in
    let ch = Unix.open_process_in command in
    if sends_mime_header then
      let header_string = read_mime_header ch in
      let header_alist,_ = Mimestring.scan_header 
			     header_string 0 (String.length header_string) in
      let content_type = 
	try List.assoc "content-type" header_alist 
	with Not_found -> "application/octet-stream" in
      let mime_type, mime_type_params =
	Mimestring.scan_mime_type content_type [] in
      let encoding =
	try Some(Netconversion.encoding_of_string
		   (List.assoc "charset" mime_type_params))
	with Not_found -> None in
      ch, encoding
    else
      ch, None
  in
  let close_channel ch =
    match Unix.close_process_in ch with
	Unix.WEXITED 0 ->
	  ()
      | Unix.WEXITED n ->
	  failwith("Command terminated with exit code " ^ string_of_int n)
      | Unix.WSIGNALED n ->
	  failwith("Command terminated by signal " ^ string_of_int n)
      | _ -> assert false
  in
  new Pxp_reader.resolve_read_url_channel
    ~close:close_channel
    ~url_of_id: get_url
    ~channel_of_url: open_channel
    ()
;;


let parse debug wf namespaces iso88591 helpers filename =
  try 
    (* Parse the document: *)
    let parse_fn =
      if wf then parse_wfdocument_entity ?transform_dtd:None
      else 
	let index = new hash_index in
	parse_document_entity 
	  ?transform_dtd:None 
	  ~id_index:(index :> 'ext index)
    in
    let mng =
      if namespaces then
	Some (new Pxp_dtd.namespace_manager)
      else
	None
    in
    let resolver =
      let file_resolver =
	new Pxp_reader.resolve_as_file() in
      new Pxp_reader.combine (helpers @ [file_resolver])
    in
    let start_id =
      System filename in
    let spec =
      if namespaces then default_namespace_spec else default_spec in
    let doc =
      parse_fn
	  { default_config with 
	      debugging_mode = debug;
	      encoding = if iso88591 then `Enc_iso88591 else `Enc_utf8;
	      idref_pass = true;
	      enable_namespace_processing = mng;
	      warner = new warner
          }
	  (ExtID(start_id, resolver))
	  spec 
    in
    ()
  with
      e ->
	(* Print error; remember that there was an error *)
	error_happened := true;
	print_error e;
	(* raise e *)
;;


let main() =
  let debug = ref false in
  let wf = ref false in
  let namespaces = ref false in
  let iso88591 = ref false in
  let helpers = ref [] in
  let files = ref [] in

  let eq_split s =
    let eq = 
      try String.index s '='
      with Not_found -> raise(Arg.Bad "Syntax error")
    in
    let before_eq = String.sub s 0 eq in
    let after_eq = String.sub s (eq+1) (String.length s - eq - 1) in
    (before_eq, after_eq)
  in

  let add_helper sends_mime_header s =
    let scheme,cmd = eq_split s in
    let h = resolve_by_helper scheme cmd sends_mime_header in
    helpers := !helpers @ [h]
  in

  let add_pubid s =
    let pubid,filename = eq_split s in
    let h = Pxp_reader.lookup_public_id_as_file [pubid,filename] in
    helpers := !helpers @ [h]
  in

  let add_sysid s =
    let sysid,filename = eq_split s in
    let h = Pxp_reader.lookup_system_id_as_file [sysid,filename] in
    helpers := !helpers @ [h]
  in

  Arg.parse
      [ "-d",   Arg.Set debug, 
           "                     turn debugging mode on";
	"-wf",  Arg.Set wf,    
            "                    check only for well-formedness";
	"-namespaces", Arg.Set namespaces,
	            "            enable namespace support";
        "-iso-8859-1", Arg.Set iso88591, 
                    "            use ISO-8859-1 as internal encoding instead of UTF-8";
	"-helper", Arg.String (add_helper false),
	        "scheme=cmd      add this helper command";
	"-helper-mh", Arg.String (add_helper true),
	           "scheme=cmd   add this helper command (which sends mime headers)";
	"-pubid", Arg.String add_pubid,
	       "id=file          map this PUBLIC id to this file";
	"-sysid", Arg.String add_sysid,
	       "id=file          map this SYSTEM id to this file";
      ]
      (fun x -> files := x :: !files)
      "
usage: pxpvalidate [options] URL ...

- checks the validity of XML documents. See below for list of options.

<title>PXP - The XML parser for Objective Caml</title>

List of options:";
  files := List.rev !files;
  List.iter (parse !debug !wf !namespaces !iso88591 !helpers) !files;
;;


main();
if !error_happened then exit(1);;

