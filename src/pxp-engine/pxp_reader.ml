(* $Id: pxp_reader.ml,v 1.16 2001/07/01 09:46:40 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types;;
exception Not_competent;;
exception Not_resolvable of exn;;

class type resolver =
  object
    method init_rep_encoding : rep_encoding -> unit
    method init_warner : collect_warnings -> unit
    method rep_encoding : rep_encoding
    method open_in : ext_id -> Lexing.lexbuf
    method close_in : unit
    method close_all : unit
    method change_encoding : string -> unit
    method clone : resolver
  end
;;


class virtual resolve_general
 =
  object (self)
    val mutable internal_encoding = `Enc_utf8

    val mutable encoding = `Enc_utf8
    val mutable encoding_requested = false

    val mutable warner = new drop_warnings

    val mutable enc_initialized = false
    val mutable wrn_initialized = false

    val mutable clones = []

    method init_rep_encoding e =
      internal_encoding <- e;
      enc_initialized <- true;

    method init_warner w =
      warner <- w;
      wrn_initialized <- true;

    method rep_encoding = (internal_encoding :> rep_encoding)

(*
    method clone =
      ( {< encoding = `Enc_utf8;
	   encoding_requested = false;
	>}
	: # resolver :> resolver )
*)

    method private warn (k:int) =
      (* Called if a character not representable has been found.
       * k is the character code.
       *)
	if k < 0xd800 or (k >= 0xe000 & k <= 0xfffd) or
	   (k >= 0x10000 & k <= 0x10ffff) then begin
	     warner # warn ("Code point cannot be represented: " ^ string_of_int k);
	   end
	else
	  raise (WF_error("Code point " ^ string_of_int k ^
		    " outside the accepted range of code points"))


    method private autodetect s =
      (* s must be at least 4 bytes long. The slot 'encoding' is
       * set to:
       * "UTF-16-BE": UTF-16/UCS-2 encoding big endian
       * "UTF-16-LE": UTF-16/UCS-2 encoding little endian
       * "UTF-8":     UTF-8 encoding
       *)
      if String.length s < 4 then
	encoding <- `Enc_utf8
      else if String.sub s 0 2 = "\254\255" then
	encoding <- `Enc_utf16
	  (* Note: Netconversion.recode will detect the big endianess, too *)
      else if String.sub s 0 2 = "\255\254" then
	encoding <- `Enc_utf16
	  (* Note: Netconversion.recode will detect the little endianess, too *)
      else
	encoding <- `Enc_utf8


    method private virtual next_string : string -> int -> int -> int
    method private virtual init_in : ext_id -> unit
    method virtual close_in : unit

    method close_all =
      List.iter (fun r -> r # close_in) clones

    method open_in xid =
      assert(enc_initialized && wrn_initialized);

      encoding <- `Enc_utf8;
      encoding_requested <- false;
      self # init_in xid;         (* may raise Not_competent *)
      (* init_in: may already set 'encoding' *)

      let buffer_max = 512 in
      let buffer = String.make buffer_max ' ' in
      let buffer_len = ref 0 in
      let buffer_end = ref false in
      let fillup () =
	if not !buffer_end & !buffer_len < buffer_max then begin
	  let l =
	    self # next_string buffer !buffer_len (buffer_max - !buffer_len) in
	  if l = 0 then
	    buffer_end := true
	  else begin
	    buffer_len := !buffer_len + l
	  end
	end
      in
      let consume n =
	let l = !buffer_len - n in
	String.blit buffer n buffer 0 l;
	buffer_len := l
      in

      fillup();
      if not encoding_requested then self # autodetect buffer;

      Lexing.from_function
	(fun s n ->
	   (* TODO: if encoding = internal_encoding, it is possible to
	    * avoid copying buffer to s because s can be directly used
	    * as buffer.
	    *)

	   fillup();
	   if !buffer_len = 0 then
	     0
	   else begin
	     let m_in  = !buffer_len in
	     let m_max = if encoding_requested then n else 1 in
	     let n_in, n_out, encoding' =
	       if encoding = (internal_encoding : rep_encoding :> encoding) &&
	          encoding_requested
	       then begin
		 (* Special case encoding = internal_encoding *)
		 String.blit buffer 0 s 0 m_in;
		 m_in, m_in, encoding
	       end
	       else
		 Netconversion.recode
		   ~in_enc:encoding
		   ~in_buf:buffer
		   ~in_pos:0
		   ~in_len:m_in
		   ~out_enc:(internal_encoding : rep_encoding :> encoding)
		   ~out_buf:s
		   ~out_pos:0
		   ~out_len:n
		   ~max_chars:m_max
		   ~subst:(fun k -> self # warn k; "")
	     in
	     if n_in = 0 then
	       (* An incomplete character at the end of the stream: *)
	       raise Netconversion.Malformed_code;
	       (* failwith "Badly encoded character"; *)
	     encoding <- encoding';
	     consume n_in;
	     assert(n_out <> 0);
	     n_out
	   end)

    method change_encoding enc =
      if not encoding_requested then begin
	if enc <> "" then begin
	  match Netconversion.encoding_of_string enc with
	      `Enc_utf16 ->
		(match encoding with
		     (`Enc_utf16_le | `Enc_utf16_be) -> ()
		   | `Enc_utf16 -> assert false
		   | _ ->
		       raise(WF_error "Encoding of data stream and encoding declaration mismatch")
		)
	    | e ->
		encoding <- e
	end;
	(* else: the autodetected encoding counts *)
	encoding_requested <- true;
      end;
  end
;;


class resolve_read_any_channel ?(close=close_in) ~channel_of_id () =
  object (self)
    inherit resolve_general as super

    val f_open = channel_of_id
    val mutable current_channel = None
    val close = close

    method private init_in (id:ext_id) =
      if current_channel <> None then
	failwith "Pxp_reader.resolve_read_any_channel # init_in";
      let ch, enc_opt = f_open id in       (* may raise Not_competent *)
      begin match enc_opt with
	  None     -> ()
	| Some enc -> encoding <- enc; encoding_requested <- true
      end;
      current_channel <- Some ch;

    method private next_string s ofs len =
      match current_channel with
	  None -> failwith "Pxp_reader.resolve_read_any_channel # next_string"
	| Some ch ->
	    input ch s ofs len

    method close_in =
      match current_channel with
	  None -> ()
	| Some ch ->
	    close ch;
	    current_channel <- None

    method clone =
      let c = new resolve_read_any_channel
		?close:(Some close) f_open () in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      (c :> resolver)

  end
;;


class resolve_read_this_channel1 is_stale ?id ?fixenc ?close ch =

  let getchannel = ref (fun xid -> assert false) in

  object (self)
    inherit resolve_read_any_channel
              ?close
	      (fun xid -> !getchannel xid)
	      ()
	      as super

    val mutable is_stale = is_stale
      (* The channel can only be read once. To avoid that the channel
       * is opened several times, the flag 'is_stale' is set after the
       * first time.
       *)

    val fixid = id
    val fixenc = fixenc
    val fixch = ch

    initializer
      getchannel := self # getchannel

    method private getchannel xid =
      begin match fixid with
	  None -> ()
	| Some bound_xid ->
	    if xid <> bound_xid then raise Not_competent
      end;
      ch, fixenc

    method private init_in (id:ext_id) =
      if is_stale then
	raise Not_competent
      else begin
	super # init_in id;
	is_stale <- true
      end

    method close_in =
      current_channel <- None

    method clone =
      let c = new resolve_read_this_channel1
		is_stale
		?id:fixid ?fixenc:fixenc ?close:(Some close) fixch
      in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      (c :> resolver)

  end
;;


class resolve_read_this_channel =
  resolve_read_this_channel1 false
;;


class resolve_read_any_string ~string_of_id () =
  object (self)
    inherit resolve_general as super

    val f_open = string_of_id
    val mutable current_string = None
    val mutable current_pos    = 0

    method private init_in (id:ext_id) =
      if current_string <> None then
	failwith "Pxp_reader.resolve_read_any_string # init_in";
      let s, enc_opt = f_open id in       (* may raise Not_competent *)
      begin match enc_opt with
	  None     -> ()
	| Some enc -> encoding <- enc; encoding_requested <- true
      end;
      current_string <- Some s;
      current_pos    <- 0;

    method private next_string s ofs len =
      match current_string with
	  None -> failwith "Pxp_reader.resolve_read_any_string # next_string"
	| Some str ->
	    let l = min len (String.length str - current_pos) in
	    String.blit str current_pos s ofs l;
	    current_pos <- current_pos + l;
	    l

    method close_in =
      match current_string with
	  None -> ()
	| Some _ ->
	    current_string <- None

    method clone =
      let c = new resolve_read_any_string f_open () in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      (c :> resolver)
  end
;;


class resolve_read_this_string1 is_stale ?id ?fixenc str =

  let getstring = ref (fun xid -> assert false) in

  object (self)
    inherit resolve_read_any_string (fun xid -> !getstring xid) () as super

    val is_stale = is_stale
      (* For some reasons, it is not allowed to open a clone of the resolver
       * a second time when the original resolver is already open.
       *)

    val fixid = id
    val fixenc = fixenc
    val fixstr = str

    initializer
      getstring := self # getstring

    method private getstring xid =
      begin match fixid with
	  None -> ()
	| Some bound_xid ->
	    if xid <> bound_xid then raise Not_competent
      end;
      fixstr, fixenc


    method private init_in (id:ext_id) =
      if is_stale then
	raise Not_competent
      else
	super # init_in id

    method clone =
      let c = new resolve_read_this_string1
		(is_stale or current_string <> None)
		?id:fixid ?fixenc:fixenc fixstr
      in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      (c :> resolver)
  end
;;


class resolve_read_this_string =
  resolve_read_this_string1 false
;;


class resolve_read_url_channel
  ?(base_url = Neturl.null_url)
  ?close
  ~url_of_id
  ~channel_of_url
  ()

  : resolver
  =

  let getchannel = ref (fun xid -> assert false) in

  object (self)
    inherit resolve_read_any_channel
              ?close
	      (fun xid -> !getchannel xid)
	      ()
	      as super

    val base_url = base_url
    val mutable own_url = Neturl.null_url

    val url_of_id = url_of_id
    val channel_of_url = channel_of_url


    initializer
      getchannel := self # getchannel

    method private getchannel xid =
      let rel_url = url_of_id xid in    (* may raise Not_competent *)

      try
	(* Now compute the absolute URL: *)
	let abs_url = 
	  if Neturl.url_provides ~scheme:true rel_url then
	    rel_url
	  else
	    Neturl.apply_relative_url base_url rel_url in
            (* may raise Malformed_URL *)

	(* Simple check whether 'abs_url' is really absolute: *)
	if not(Neturl.url_provides ~scheme:true abs_url)
	then raise Not_competent;

	own_url <- abs_url;
        (* FIXME: Copy 'abs_url' ? *)

	(* Get and return the channel: *)
	channel_of_url xid abs_url            (* may raise Not_competent *)
      with
	  Neturl.Malformed_URL -> raise (Not_resolvable Neturl.Malformed_URL)
	| Not_competent        -> raise (Not_resolvable Not_found)

    method clone =
      let c =
	new resolve_read_url_channel
	  ?base_url:(Some own_url)
	  ?close:(Some close)
	  ~url_of_id:url_of_id
	  ~channel_of_url:channel_of_url
	  ()
      in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      (c :> resolve_read_url_channel)
  end
;;


type spec = [ `Not_recognized | `Allowed | `Required ]

class resolve_as_file
  ?(file_prefix = (`Allowed :> spec))
  ?(host_prefix = (`Allowed :> spec))
  ?(system_encoding = `Enc_utf8)
  ?(map_private_id = (fun _ -> raise Not_competent))
  ?(open_private_id = (fun _ -> raise Not_competent))
  ()
  =

  let url_syntax =
    let enable_if =
      function
	  `Not_recognized  -> Neturl.Url_part_not_recognized
	| `Allowed         -> Neturl.Url_part_allowed
	| `Required        -> Neturl.Url_part_required
    in
    { Neturl.null_url_syntax with
	Neturl.url_enable_scheme = enable_if file_prefix;
	Neturl.url_enable_host   = enable_if host_prefix;
	Neturl.url_enable_path   = Neturl.Url_part_required;
	Neturl.url_accepts_8bits = true;
    }
  in

  let base_url_syntax =
    { Neturl.null_url_syntax with
	Neturl.url_enable_scheme = Neturl.Url_part_required;
	Neturl.url_enable_host   = Neturl.Url_part_allowed;
	Neturl.url_enable_path   = Neturl.Url_part_required;
	Neturl.url_accepts_8bits = true;
    }
  in

  let default_base_url =
    Neturl.make_url
      ~scheme: "file"
      ~host:   ""
      ~path:   (Neturl.split_path (Sys.getcwd() ^ "/"))
      base_url_syntax
  in

  let file_url_of_id xid =
    let file_url_of_sysname sysname =
      (* By convention, we can assume that sysname is a URL conforming
       * to RFC 1738 with the exception that it may contain non-ASCII
       * UTF-8 characters.
       *)
      try
	Neturl.url_of_string url_syntax sysname
          (* may raise Malformed_URL *)
      with
	  Neturl.Malformed_URL -> raise Not_competent
    in
    let url =
      match xid with
	  Anonymous          -> raise Not_competent
	| Public (_,sysname) -> if sysname <> "" then file_url_of_sysname sysname
                                                 else raise Not_competent
	| System sysname     -> file_url_of_sysname sysname
	| Private pid        -> map_private_id pid
    in
    let scheme =
      try Neturl.url_scheme url with Not_found -> "file" in
    let host =
      try Neturl.url_host url with Not_found -> "" in

    if scheme <> "file" then raise Not_competent;
    if host <> "" && host <> "localhost" then raise Not_competent;

    url
  in

  let channel_of_file_url xid url =
    match xid with
	Private pid -> open_private_id pid
      | _ ->
	  ( try
	      let path_utf8 =
		try Neturl.join_path (Neturl.url_path ~encoded:false url)
		with Not_found -> raise Not_competent
	      in
	      
	      let path =
		Netconversion.recode_string
		  ~in_enc:  `Enc_utf8
		  ~out_enc: system_encoding
		  path_utf8 in
              (* May raise Malformed_code *)
	      
	      open_in_bin path, None
		(* May raise Sys_error *)
		
	    with
	      | Netconversion.Malformed_code -> assert false
  	        (* should not happen *)
	      | Sys_error _ as e ->
		  raise (Not_resolvable e)
	  )
  in

  resolve_read_url_channel
    ~base_url:       default_base_url
    ~url_of_id:      file_url_of_id
    ~channel_of_url: channel_of_file_url
    ()
;;


let make_file_url ?(system_encoding = `Enc_utf8) ?(enc = `Enc_utf8) filename =
  let utf8_filename =
    Netconversion.recode_string
    ~in_enc: enc
    ~out_enc: `Enc_utf8 
      filename
  in

  let utf8_abs_filename =
    if utf8_filename <> "" && utf8_filename.[0] = '/' then
      utf8_filename
    else
      let cwd = Sys.getcwd() in
      let cwd_utf8 =
	Netconversion.recode_string
	~in_enc: system_encoding
	~out_enc: `Enc_utf8 in
      cwd ^ "/" ^ utf8_filename
  in
  
  let syntax = { Neturl.ip_url_syntax with Neturl.url_accepts_8bits = true } in
  let url = Neturl.make_url
	    ~scheme:"file"
	    ~host:"localhost"
	    ~path:(Neturl.split_path utf8_abs_filename)
	      syntax
  in
  url
;;


class lookup_public_id (catalog : (string * resolver) list) =
  let norm_catalog =
    List.map (fun (id,s) -> Pxp_aux.normalize_public_id id, s) catalog in
( object (self)
    val cat = norm_catalog
    val mutable internal_encoding = `Enc_utf8
    val mutable warner = new drop_warnings
    val mutable active_resolver = None

    method init_rep_encoding enc =
      internal_encoding <- enc

    method init_warner w =
      warner <- w;

    method rep_encoding = internal_encoding
      (* CAUTION: This may not be the truth! *)

    method open_in xid =

      if active_resolver <> None then failwith "Pxp_reader.lookup_* # open_in";

      let r =
	match xid with
	    Public(pubid,_) ->
	      begin
		(* Search pubid in catalog: *)
		try
		  let norm_pubid = Pxp_aux.normalize_public_id pubid in
		  List.assoc norm_pubid cat
		with
		    Not_found ->
		      raise Not_competent
	      end
	  | _ ->
	      raise Not_competent
      in

      let r' = r # clone in
      r' # init_rep_encoding internal_encoding;
      r' # init_warner warner;
      let lb = r' # open_in xid in   (* may raise Not_competent *)
      active_resolver <- Some r';
      lb

    method close_in =
      match active_resolver with
	  None   -> ()
	| Some r -> r # close_in;
	            active_resolver <- None

    method close_all =
      self # close_in

    method change_encoding (enc:string) =
      match active_resolver with
	  None   -> failwith "Pxp_reader.lookup_* # change_encoding"
	| Some r -> r # change_encoding enc

    method clone =
      let c = new lookup_public_id cat in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      c
  end : resolver )
;;


let lookup_public_id_as_file ?(fixenc:encoding option) catalog =
  let ch_of_id filename id =
    let ch = open_in_bin filename in  (* may raise Sys_error *)
    ch, fixenc
  in
  let catalog' =
    List.map
      (fun (id,s) ->
	 (id, new resolve_read_any_channel (ch_of_id s) ())
      )
      catalog
  in
  new lookup_public_id catalog'
;;


let lookup_public_id_as_string ?(fixenc:encoding option) catalog =
   let catalog' =
    List.map
      (fun (id,s) ->
	 (id, new resolve_read_any_string (fun _ -> s, fixenc) ())
      )
      catalog
  in
  new lookup_public_id catalog'
;;
   

class lookup_system_id (catalog : (string * resolver) list) =
( object (self)
    val cat = catalog
    val mutable internal_encoding = `Enc_utf8
    val mutable warner = new drop_warnings
    val mutable active_resolver = None

    method init_rep_encoding enc =
      internal_encoding <- enc

    method init_warner w =
      warner <- w;

    method rep_encoding = internal_encoding
      (* CAUTION: This may not be the truth! *)


    method open_in xid =

      if active_resolver <> None then failwith "Pxp_reader.lookup_system_id # open_in";

      let lookup sysid =
	try
	  List.assoc sysid cat
	with
	    Not_found ->
	      raise Not_competent
      in

      let r =
	match xid with
	    System sysid    -> lookup sysid
	  | Public(_,sysid) -> lookup sysid
	  | _               -> raise Not_competent
      in

      let r' = r # clone in
      r' # init_rep_encoding internal_encoding;
      r' # init_warner warner;
      let lb = r' # open_in xid in   (* may raise Not_competent *)
      active_resolver <- Some r';
      lb


    method close_in =
      match active_resolver with
	  None   -> ()
	| Some r -> r # close_in;
	            active_resolver <- None

    method close_all =
      self # close_in

    method change_encoding (enc:string) =
      match active_resolver with
	  None   -> failwith "Pxp_reader.lookup_system # change_encoding"
	| Some r -> r # change_encoding enc

    method clone =
      let c = new lookup_system_id cat in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      c
  end : resolver)
;;


let lookup_system_id_as_file ?(fixenc:encoding option) catalog =
  let ch_of_id filename id =
    let ch = open_in_bin filename in  (* may raise Sys_error *)
    ch, fixenc
  in
  let catalog' =
    List.map
      (fun (id,s) ->
	 (id, new resolve_read_any_channel (ch_of_id s) ())
      )
      catalog
  in
  new lookup_system_id catalog'
;;


let lookup_system_id_as_string ?(fixenc:encoding option) catalog =
   let catalog' =
    List.map
      (fun (id,s) ->
	 (id, new resolve_read_any_string (fun _ -> s, fixenc) ())
      )
      catalog
  in
  new lookup_system_id catalog'
;;
   

type combination_mode =
    Public_before_system
  | System_before_public
;;


class combine ?prefer ?(mode = Public_before_system) rl =
  object (self)
    val prefered_resolver = prefer
    val mode = mode
    val resolvers = (rl : resolver list)
    val mutable internal_encoding = `Enc_utf8
    val mutable warner = new drop_warnings
    val mutable active_resolver = None
    val mutable clones = []

    method init_rep_encoding enc =
      List.iter
	(fun r -> r # init_rep_encoding enc)
	rl;
      internal_encoding <- enc

    method init_warner w =
      List.iter
	(fun r -> r # init_warner w)
	rl;
      warner <- w;

    method rep_encoding = internal_encoding
      (* CAUTION: This may not be the truth! *)

    method open_in xid =
      let rec find_competent_resolver_for xid' rl =
	match rl with
	    r :: rl' ->
	      begin try
		r, (r # open_in xid')
	      with
		  Not_competent -> find_competent_resolver_for xid' rl'
	      end;
	  | [] ->
	      raise Not_competent
      in

      let find_competent_resolver rl =
	match xid with
	    Public(pubid,sysid) ->
	      ( match mode with
		    Public_before_system ->
		      ( try
			  find_competent_resolver_for(Public(pubid,"")) rl
			with
			    Not_competent ->
			      find_competent_resolver_for(System sysid) rl
		      )
		  | System_before_public ->
		      ( try
			  find_competent_resolver_for(System sysid) rl
			with
			    Not_competent ->
			      find_competent_resolver_for(Public(pubid,"")) rl
		      )
	      )
	  | other ->
	      find_competent_resolver_for other rl
      in

      if active_resolver <> None then failwith "Pxp_reader.combine # open_in";
      let r, lb =
	match prefered_resolver with
	    None ->   find_competent_resolver resolvers
	  | Some r -> find_competent_resolver (r :: resolvers)
      in
      active_resolver <- Some r;
      lb

    method close_in =
      match active_resolver with
	  None   -> ()
	| Some r -> r # close_in;
	            active_resolver <- None

    method close_all =
      List.iter (fun r -> r # close_in) clones

    method change_encoding (enc:string) =
      match active_resolver with
	  None   -> failwith "Pxp_reader.combine # change_encoding"
	| Some r -> r # change_encoding enc

    method clone =
      let c =
	match active_resolver with
	    None   ->
	      new combine ?prefer:None ?mode:(Some mode) 
                          (List.map (fun q -> q # clone) resolvers)
	  | Some r ->
	      let r' = r # clone in
	      new combine
		?prefer:(Some r')
		?mode:(Some mode)
		(List.map
		   (fun q -> if q == r then r' else q # clone)
		   resolvers)
      in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      c
  end



(* ======================================================================
 * History:
 *
 * $Log: pxp_reader.ml,v $
 * Revision 1.16  2001/07/01 09:46:40  gerd
 * 	Fix: resolve_read_url_channel does not use the base_url if
 * the current URL is already absolute
 *
 * Revision 1.15  2001/07/01 08:35:23  gerd
 * 	Instead of the ~auto_close argument, there is now a
 * ~close argument for several functions/classes. This allows some
 * additional action when the resolver is closed.
 *
 * Revision 1.14  2001/06/14 23:28:02  gerd
 * 	Fix: class combine works now with private IDs.
 *
 * Revision 1.13  2001/04/22 14:16:48  gerd
 * 	resolve_as_file: you can map private IDs to arbitrary channels.
 * 	resolve_read_url_channel: changed type of the channel_of_url
 * argument (ext_id is also passed)
 * 	More examples and documentation.
 *
 * Revision 1.12  2001/04/21 17:40:48  gerd
 * 	Bugfix in 'combine'
 *
 * Revision 1.11  2001/04/03 20:22:44  gerd
 * 	New resolvers for catalogs of PUBLIC and SYSTEM IDs.
 * 	Improved "combine": PUBLIC and SYSTEM IDs are handled
 * separately.
 * 	Rewritten from_file: Is now a simple application of the
 * Pxp_reader classes and functions. (The same has still to be done
 * for from_channel!)
 *
 * Revision 1.10  2001/02/01 20:38:49  gerd
 * 	New support for PUBLIC identifiers.
 *
 * Revision 1.9  2000/08/14 22:24:55  gerd
 * 	Moved the module Pxp_encoding to the netstring package under
 * the new name Netconversion.
 *
 * Revision 1.8  2000/07/16 18:31:09  gerd
 * 	The exception Illegal_character has been dropped.
 *
 * Revision 1.7  2000/07/09 15:32:01  gerd
 * 	Fix in resolve_this_channel, resolve_this_string
 *
 * Revision 1.6  2000/07/09 01:05:33  gerd
 * 	New methode 'close_all' that closes the clones, too.
 *
 * Revision 1.5  2000/07/08 16:24:56  gerd
 * 	Introduced the exception 'Not_resolvable' to indicate that
 * 'combine' should not try the next resolver of the list.
 *
 * Revision 1.4  2000/07/06 23:04:46  gerd
 * 	Quick fix for 'combine': The active resolver is "prefered",
 * but the other resolvers are also used.
 *
 * Revision 1.3  2000/07/06 21:43:45  gerd
 * 	Fix: Public(_,name) is now treated as System(name) if
 * name is non-empty.
 *
 * Revision 1.2  2000/07/04 22:13:30  gerd
 * 	Implemented the new API rev. 1.2 of pxp_reader.mli.
 *
 * Revision 1.1  2000/05/29 23:48:38  gerd
 * 	Changed module names:
 * 		Markup_aux          into Pxp_aux
 * 		Markup_codewriter   into Pxp_codewriter
 * 		Markup_document     into Pxp_document
 * 		Markup_dtd          into Pxp_dtd
 * 		Markup_entity       into Pxp_entity
 * 		Markup_lexer_types  into Pxp_lexer_types
 * 		Markup_reader       into Pxp_reader
 * 		Markup_types        into Pxp_types
 * 		Markup_yacc         into Pxp_yacc
 * See directory "compatibility" for (almost) compatible wrappers emulating
 * Markup_document, Markup_dtd, Markup_reader, Markup_types, and Markup_yacc.
 *
 * ======================================================================
 * Old logs from markup_reader.ml:
 *
 * Revision 1.3  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.2  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.1  2000/03/13 23:41:44  gerd
 * 	Initial revision; this code was formerly part of Markup_entity.
 *
 *
 *)
