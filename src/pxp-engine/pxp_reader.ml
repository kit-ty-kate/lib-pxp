(* $Id: pxp_reader.ml,v 1.21 2003/01/21 00:18:48 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types;;
open Netchannels;;

exception Not_competent;;
exception Not_resolvable of exn;;

class type resolver =
  object
    method init_rep_encoding : rep_encoding -> unit
    method init_warner : collect_warnings -> unit
    method rep_encoding : rep_encoding
    method open_in : ext_id -> Lexing.lexbuf
    method open_rid : resolver_id -> Lexing.lexbuf
    method active_id : resolver_id
    method close_in : unit
    method close_all : unit
    method change_encoding : string -> unit
    method clone : resolver
  end
;;


let null_resolver = resolver_id_of_ext_id Anonymous ;;
(* All components are None *)


class virtual resolve_general
 =
  object (self)
    val mutable internal_encoding = `Enc_utf8

    val mutable is_open = false

    val mutable encoding = `Enc_utf8
    val mutable encoding_requested = false

    val mutable active_id = null_resolver

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
    method private virtual init_in : resolver_id -> unit
    method virtual close_in : unit
      (* must reset is_open! *)

    method close_all =
      List.iter (fun r -> r # close_in) clones

    method open_in xid =
      self # open_rid (resolver_id_of_ext_id xid)

    method open_rid rid =
      assert(enc_initialized && wrn_initialized);

      encoding <- `Enc_utf8;
      encoding_requested <- false;
      self # init_in rid;         (* may raise Not_competent *)
      (* init_in: may already set 'encoding' *)

      is_open <- true;

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
	if l > 0 then String.blit buffer n buffer 0 l;
	buffer_len := l
      in

      fillup();
      if not encoding_requested then self # autodetect buffer;

      Pxp_lexing.from_function
	(fun s n ->
	   (* TODO: if encoding = internal_encoding, it is possible to
	    * avoid copying buffer to s because s can be directly used
	    * as buffer.
	    *)

	   if not is_open then
	     failwith "trying to read from resolver, but resolver is not open";

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
      if not is_open then
	failwith "#change_encoding: resolver is not open";
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

    method active_id = 
      if not is_open then failwith "#active_id: resolver is not open";
      active_id

  end
;;

type accepted_id =
    in_obj_channel * encoding option * resolver_id option
;;

let close_ch (ch : in_obj_channel) = ch # close_in() ;;

class resolve_to_any_obj_channel ?(close=close_ch) ~channel_of_id () =
object(self)
    inherit resolve_general as super

    val f_open = channel_of_id
    val mutable current_channel = None
    val close = close

    method private init_in (id:resolver_id) =
      if current_channel <> None then
	failwith "Pxp_reader.resolve_to_any_obj_channel # init_in";
      let ch, enc_opt, rid_opt = f_open id in    (* may raise Not_competent *)
      begin match enc_opt with
	  None     -> ()
	| Some enc -> encoding <- enc; encoding_requested <- true
      end;
      begin match rid_opt with
	  None     -> active_id <- id
	| Some r   -> active_id <- r
      end;
      current_channel <- Some ch;

    method private next_string s ofs len =
      match current_channel with
	  None -> failwith "Pxp_reader.resolve_read_any_channel # next_string"
	| Some ch ->
	    ch # input s ofs len

    method close_in =
      is_open <- false;
      match current_channel with
	  None -> ()
	| Some ch ->
	    close ch;
	    current_channel <- None

    method clone =
      let c = new resolve_to_any_obj_channel
		?close:(Some close) ~channel_of_id:f_open () in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      (c :> resolver)

end
;;


let rid_rid_intersection bound_rid actual_rid =
  (* Returns a resolver_id where unequal IDs are reset to None. The
   * rid_system_base is set corresponding to rid_system.
   *)
  let isect opt1 opt2 =
    if opt1 = opt2 then opt1 else None
  in
  { rid_private = isect bound_rid.rid_private actual_rid.rid_private;
    rid_public  = isect bound_rid.rid_public actual_rid.rid_public;
    rid_system  = isect bound_rid.rid_system actual_rid.rid_system;
    rid_system_base = if bound_rid.rid_system = actual_rid.rid_system then
                        actual_rid.rid_system_base
                      else
			None;
  }
;;


let rid_matches_rid bound_rid actual_rid =
  (* definition:
   * rid_matches_rid r1 r2 =def= 
   * rid_rid_intersection r1 r2 <> null_resolver
   *)
  (* CHECK: Do we need to normalize PUBLIC Ids first? *)
  (bound_rid.rid_private <> None && 
   bound_rid.rid_private = actual_rid.rid_private) ||
  (bound_rid.rid_public <> None && 
   bound_rid.rid_public = actual_rid.rid_public) ||
  (bound_rid.rid_system <> None && 
   bound_rid.rid_system = actual_rid.rid_system)
;;


let xid_rid_intersection bound_xid actual_rid =
  rid_rid_intersection (resolver_id_of_ext_id bound_xid) actual_rid
;;


let xid_matches_rid bound_xid actual_rid =
  (* definition:
   * xid_matches_rid x r =def= 
   * xid_rid_intersection x r <> null_resolver
   *)
  match bound_xid with
      System sys -> 
	actual_rid.rid_system = Some sys
    | Public(pub,sys) ->
	actual_rid.rid_public = Some pub || actual_rid.rid_system = Some sys
    | Anonymous ->
	false
    | Private p ->
	actual_rid.rid_private = Some p
;;


let id_intersection bound_rid_opt bound_xid_opt actual_rid =
  (* Intersections the actual_rid with both bound_rid_opt and bound_xid_opt
   * in turn
   *)
  let rid1 =
    match bound_rid_opt with
	Some bound_rid -> rid_rid_intersection bound_rid actual_rid
      | None           -> actual_rid
  in
  let rid2 =
    match bound_xid_opt with
	Some bound_xid -> xid_rid_intersection bound_xid rid1
      | None           -> rid1
  in
  rid2
;;


class resolve_to_this_obj_channel1 is_stale ?id ?rid ?fixenc ?close ch =

  let getchannel = ref (fun rid -> assert false) in

  object (self)
    inherit resolve_to_any_obj_channel
              ?close
	      ~channel_of_id:(fun rid -> !getchannel rid)
	      ()
	      as super

    val is_stale = is_stale
      (* The channel can only be read once. To avoid that the channel
       * is opened several times, the flag 'is_stale' is set after the
       * first time.
       *)

    val fixid = id
    val fixrid = rid
    val fixenc = fixenc
    val fixch = ch

    initializer
      getchannel := self # getchannel

    method private getchannel rid =
      let m_xid =
	match fixid with
	    None -> false
	  | Some bound_xid ->
	      xid_matches_rid bound_xid rid
      in
      let m_rid =
	match fixrid with
	    None -> false
	  | Some bound_rid ->
	      rid_matches_rid bound_rid rid
      in
      (* By definition, if both fixid and fixrid are None, the resolver
       * will match always.
       *)
      if (fixid <> None || fixrid <> None) && not m_xid && not m_rid then
	raise Not_competent;
      let final_id = id_intersection fixrid fixid rid in
      ch, fixenc, Some final_id

    method private init_in (rid:resolver_id) =
      if !is_stale then
	raise Not_competent
      else begin
	super # init_in rid;
	is_stale := true
      end

    method clone =
      let c = new resolve_to_this_obj_channel1
		is_stale
		?id:fixid ?rid:fixrid ?fixenc:fixenc ?close:(Some close) fixch
      in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      (c :> resolver)

  end
;;


class resolve_to_this_obj_channel =
  let is_stale = ref false in
  resolve_to_this_obj_channel1 is_stale
;;


class resolve_to_url_obj_channel ?close 
                                 ~url_of_id ~base_url_of_id ~channel_of_url () =
  let channel_of_id rid =
    let rel_url = url_of_id rid in    (* may raise Not_competent *)
    try
      (* Now compute the absolute URL: *)
      let abs_url = 
	if Neturl.url_provides ~scheme:true rel_url then
	  rel_url
	else
	  let base_url = base_url_of_id rid in
	  Neturl.apply_relative_url base_url rel_url in
          (* may raise Malformed_URL *)

      (* Simple check whether 'abs_url' is really absolute: *)
      if not(Neturl.url_provides ~scheme:true abs_url)
      then raise Not_competent;

      let rid' =
	{ rid with 
	    rid_system = Some(Neturl.string_of_url abs_url)
	} in
      
      (* Get and return the channel: *)
      let ch, enc_opt, active_id_opt =
	channel_of_url rid' abs_url            (* may raise Not_competent *)
      in
      (ch, 
       enc_opt, 
       (match active_id_opt with
	    None -> Some rid'
	  | _    -> active_id_opt
       ))
    with
	Neturl.Malformed_URL -> raise (Not_resolvable Neturl.Malformed_URL)
      | Not_competent        -> raise (Not_resolvable Not_found)
  in
  resolve_to_any_obj_channel ?close ~channel_of_id ()
;;


let base_url_syntax =
  { Neturl.null_url_syntax with
      Neturl.url_enable_scheme = Neturl.Url_part_required;
      Neturl.url_enable_host   = Neturl.Url_part_allowed;
      Neturl.url_enable_path   = Neturl.Url_part_required;
      Neturl.url_accepts_8bits = true;
  }
;;


type spec = [ `Not_recognized | `Allowed | `Required ]

class resolve_as_file
  ?(file_prefix = (`Allowed :> spec))
  ?(host_prefix = (`Allowed :> spec))
  ?(system_encoding = `Enc_utf8)
  ?(map_private_id = (fun _ -> raise Not_competent))
  ?(open_private_id = (fun _ -> raise Not_competent))
  ?(base_url_defaults_to_cwd = true)
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

  let default_base_url =
    if base_url_defaults_to_cwd then begin
      let cwd = Sys.getcwd() in
      let cwd_slash = if cwd = "/" then cwd else cwd ^ "/" in
      Some(Neturl.make_url
	     ~scheme: "file"
	     ~host:   ""
	     ~path:   (Neturl.split_path cwd_slash)
	     base_url_syntax)
    end
    else
      None
  in

  let use_private_id = ref false in
  let url_of_id rid =
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
    use_private_id := false;
    let url =
      match rid.rid_system with
	  None -> 
	    ( match rid.rid_private with
		  None -> 
		    raise Not_competent
		| Some p -> 
		    let url = map_private_id p in
		    use_private_id := true;
		    url
	    )
	| Some sysname -> file_url_of_sysname sysname
    in
    let scheme =
      try Neturl.url_scheme url with Not_found -> "file" in
    let host =
      try Neturl.url_host url with Not_found -> "" in

    if scheme <> "file" then raise Not_competent;
    if host <> "" && host <> "localhost" then raise Not_competent;

    url
  in

  let base_url_of_id rid =
    match rid.rid_system_base with
	Some sysname ->
	  Neturl.url_of_string base_url_syntax sysname
      | None ->
	  ( match default_base_url with
		Some url -> url
	      | None     -> raise Not_competent
	  )
  in

  let channel_of_url rid url =
    if !use_private_id then begin
      match rid.rid_private with
	  Some p ->
	    let ch, enc_opt = open_private_id p in
	    (new input_channel ch, enc_opt, None)
	| None ->
	    assert false
    end
    else begin
      try
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
	      
	(new input_channel(open_in_bin path), None, None)
	(* May raise Sys_error *)
		
      with
	| Netconversion.Malformed_code -> assert false
  	    (* should not happen *)
	| Sys_error _ as e ->
	    raise (Not_resolvable e)
    end
  in

  resolve_to_url_obj_channel
    ~url_of_id
    ~base_url_of_id
    ~channel_of_url
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
	~out_enc: `Enc_utf8 cwd in
      if cwd = "/" then "/" ^ utf8_filename else cwd_utf8 ^ "/" ^ utf8_filename
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


class lookup_id (catalog : (ext_id * resolver) list) =
  let norm_catalog =
    (* catalog with normalized PUBLIC ids *)
    List.map 
      (fun (id,s) -> 
	 match id with
	     Public(pubid,sysid) ->
	       let norm_pubid = Pxp_aux.normalize_public_id pubid in
	       (Public(norm_pubid,sysid), s)
	   | _ ->
	       (id,s)
      )
      catalog in
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
      self # open_rid (resolver_id_of_ext_id xid)

    method open_rid rid =

      if active_resolver <> None then failwith "Pxp_reader.lookup_* # open_rid";

      let selected_xid, r =
	try
	  List.find
	    (fun (xid,r) ->
	       xid_matches_rid xid rid
	    )
	    cat
	with
	    Not_found -> raise Not_competent
      in

      let r' = r # clone in
      r' # init_rep_encoding internal_encoding;
      r' # init_warner warner;
      let lb = r' # open_rid rid in   (* may raise Not_competent *)
      active_resolver <- Some (selected_xid,r');
      lb

    method close_in =
      match active_resolver with
	  None   -> ()
	| Some(_,r) -> 
	    r # close_in;
	    active_resolver <- None

    method active_id =
      match active_resolver with
	  None   -> failwith "#active_id: resolver is not open"
	| Some(selected_xid, r) -> 
	    ( match selected_xid with
		  Private p ->
		    { null_resolver with rid_private = Some p }
		| System sysid ->
		    { null_resolver with rid_system = Some sysid }
		    (* Note: Relative URLs do not make sense in catalogs,
		     * so ignore this case here
		     *)
		| Public(pubid,sysid) ->
		    { null_resolver with
			rid_public = Some pubid;
			rid_system = if sysid = "" then None else Some sysid;
		    }
		| Anonymous ->
		    assert false
	    )

    method close_all =
      (* CHECK: Müssen nicht die Klone auch geschlossen werden? *)
      self # close_in


    method change_encoding (enc:string) =
      match active_resolver with
	  None      -> failwith "Pxp_reader.lookup_* # change_encoding"
	| Some(_,r) -> r # change_encoding enc


    method clone =
      let c = new lookup_id cat in
      (* CHECK: How to avoid that public ids are normalized again? *)
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      c
  end : resolver )
;;


class lookup_id_as_file ?(fixenc:encoding option) catalog =
  let ch_of_id filename id =
    let ch = open_in_bin filename in  (* may raise Sys_error *)
    (new input_channel ch, fixenc, None)
  in
  let catalog' =
    List.map
      (fun (id,s) ->
	 (id, 
	  new resolve_to_any_obj_channel ~channel_of_id:(ch_of_id s) ()
	 )
      )
      catalog
  in
  lookup_id catalog'
;;


class lookup_id_as_string ?(fixenc:encoding option) catalog =
  let ch_of_id s rid =
    (new input_string s, fixenc, None)
  in
  let catalog' =
    List.map
      (fun (id,s) ->
	 (id, 
	  new resolve_to_any_obj_channel ~channel_of_id:(ch_of_id s) ()
	 )
      )
      catalog
  in
  lookup_id catalog'
;;
   

let map_public_id catalog =
  List.map (fun (pubid,x) -> (Public(pubid,""), x)) catalog
;;


let map_system_id catalog =
  List.map (fun (sysid,x) -> (System sysid, x)) catalog
;;
  

class lookup_public_id catalog =
  lookup_id (map_public_id catalog)
;;


class lookup_public_id_as_file ?fixenc catalog =
  lookup_id_as_file ?fixenc (map_public_id catalog)
;;


class lookup_public_id_as_string ?fixenc catalog =
  lookup_id_as_string ?fixenc (map_public_id catalog)
;;


class lookup_system_id catalog =
  lookup_id (map_system_id catalog)
;;


class lookup_system_id_as_file ?fixenc catalog =
  lookup_id_as_file ?fixenc (map_system_id catalog)
;;


class lookup_system_id_as_string ?fixenc catalog =
  lookup_id_as_string ?fixenc (map_system_id catalog)
;;
   

type combination_mode =
    Public_before_system
  | System_before_public
;;


class combine ?prefer ?mode rl =
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
      self # open_rid (resolver_id_of_ext_id xid)

    method open_rid rid =
      let rec find_competent_resolver_for rid' rl =
	match rl with
	    r :: rl' ->
	      begin try
		r, (r # open_rid rid')
	      with
		  Not_competent -> find_competent_resolver_for rid' rl'
	      end;
	  | [] ->
	      raise Not_competent
      in

      let find_competent_resolver rl =
	match mode with
	    None -> 
	      find_competent_resolver_for rid rl
	  | Some Public_before_system ->
	      ( try
		  find_competent_resolver_for 
		    { rid with rid_system = None } rl
		with
		    Not_competent ->
		      find_competent_resolver_for 
		        { rid with rid_public = None } rl
	      )
	  | Some System_before_public ->
	      ( try
		  find_competent_resolver_for 
		    { rid with rid_public = None } rl
		with
		    Not_competent ->
		      find_competent_resolver_for 
		        { rid with rid_system = None } rl
	      )
      in

      if active_resolver <> None then failwith "Pxp_reader.combine # open_rid";
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

    method active_id =
      match active_resolver with
	  None -> failwith "#active_id: resolver not open"
	| Some r -> r # active_id

    method clone =
      let c =
	match active_resolver with
	    None   ->
	      new combine ?prefer:None ?mode
                          (List.map (fun q -> q # clone) resolvers)
	  | Some r ->
	      let r' = r # clone in
	      new combine
		?prefer:(Some r')
		?mode
		(List.map
		   (fun q -> if q == r then r' else q # clone)
		   resolvers)
      in
      c # init_rep_encoding internal_encoding;
      c # init_warner warner;
      clones <- c :: clones;
      c
  end
;;


(**********************************************************************)
(* EMULATION OF DEPRECATED CLASSES                                    *)
(**********************************************************************)

let rec try_several f l =
  (* Applies the function f to all elements of l in turn. The function can
   * return a result value, or can raise Not_competent. The elements are
   * tried until a result value is found. If no element leads to a result,
   * the exception Not_competent is raised.
   *)
  match l with
      [] -> 
	raise Not_competent
    | x :: l' ->
	( try f x with Not_competent -> try_several f l')
;;


let xid_list_of_rid rid =
  (* Returns a list of ext_ids that are compatible to the rid *)
  (match rid.rid_private with
       Some p -> [ Private p ]
     | None   -> []
  ) @
  (match rid.rid_public, rid.rid_system with
       (Some pub, Some sys) -> [ Public(pub,sys) ]
     | (Some pub, None)     -> [ Public(pub,"") ]
     | (None, Some sys)     -> [ System(sys) ]
     | (None, None)         -> []
  )
;;


class resolve_read_any_channel ?(close=close_in) ~channel_of_id () =
  (* reduce resolve_read_any_channel to resolve_to_any_obj_channel *)
  let current_ch = ref None in
  let obj_channel_of_id rid =
    try_several
      (fun xid ->
	 let ch, enc_opt = channel_of_id xid in  
	                   (* may raise Not_competent *)
	 current_ch := Some ch;
	 (new input_channel ch,
	  enc_opt,
	  Some (resolver_id_of_ext_id xid))
      )
      (xid_list_of_rid rid)
  in
  let close_obj _ =
    match !current_ch with
	Some ch -> close ch; current_ch := None
      | None -> ()
  in

  resolve_to_any_obj_channel 
    ~close:close_obj
    ~channel_of_id:obj_channel_of_id
    ()
;;


class resolve_read_this_channel ?id ?fixenc ?close ch =
  (* reduce resolve_read_this_channel to resolve_to_this_obj_channel *)
  let obj_ch =
    new input_channel ch in
  let close_obj _ =
    match close with
	Some f -> f ch
      | None -> ()
  in
  resolve_to_this_obj_channel ?id ?fixenc ~close:close_obj obj_ch
;;


class resolve_read_this_string ?id ?fixenc s =
  (* reduce resolve_read_this_string to resolve_to_this_obj_channel *)
  let obj_ch =
    new input_string s in
  resolve_to_this_obj_channel ?id ?fixenc obj_ch
;;


class resolve_read_any_string ~string_of_id () =
  (* reduce resolve_read_any_string to resolve_to_any_obj_channel *)
  let obj_channel_of_id rid =
    try_several
      (fun xid ->
	 let s, enc_opt = string_of_id xid in  
	                   (* may raise Not_competent *)
	 (new input_string s,
	  enc_opt,
	  Some(resolver_id_of_ext_id xid))
      )
      (xid_list_of_rid rid)
  in
  resolve_to_any_obj_channel ~channel_of_id:obj_channel_of_id ()
;;


class resolve_read_url_channel
  (* reduce resolve_read_url_channel to resolve_to_url_obj_channel *)
  ?(base_url = Neturl.null_url)
  ?(close = close_in)
  ~url_of_id
  ~channel_of_url
  () =
  let current_ch = ref None in
  let current_xid = ref Anonymous in
  let url_of_id' rid =
    try_several 
      (fun xid ->
	 let url = url_of_id xid  in (* or Not_competent *)
	 current_xid := xid;
	 url
      )
      (xid_list_of_rid rid) in
  let base_url_of_id rid =
    ( match rid.rid_system_base with
	  Some sys -> Neturl.url_of_string base_url_syntax sys
	| None     -> raise Not_competent
    ) in
  let channel_of_url' rid url =
    let ch, enc_opt = channel_of_url !current_xid url in  
	              (* may raise Not_competent *)
    current_ch := Some ch;
    let active_id = resolver_id_of_ext_id !current_xid in
    let active_id' = { active_id with
			 rid_system = rid.rid_system
		     } in
    (new input_channel ch, enc_opt, Some active_id')
  in
  let close_obj _ =
    match !current_ch with
	Some ch -> close ch; current_ch := None
      | None -> ()
  in

  resolve_to_url_obj_channel
    ~close:close_obj
    ~url_of_id:url_of_id'
    ~base_url_of_id
    ~channel_of_url:channel_of_url'
    ()
;;


let lookup_public_id_as_file ?fixenc catalog =
  new lookup_public_id_as_file ?fixenc catalog ;;

let lookup_public_id_as_string ?fixenc catalog =
  new lookup_public_id_as_string ?fixenc catalog ;;

let lookup_system_id_as_file ?fixenc catalog =
  new lookup_system_id_as_file ?fixenc catalog ;;

let lookup_system_id_as_string ?fixenc catalog =
  new lookup_system_id_as_string ?fixenc catalog ;;




(* ======================================================================
 * History:
 *
 * $Log: pxp_reader.ml,v $
 * Revision 1.21  2003/01/21 00:18:48  gerd
 * 	Reimplementation of the reader classes, now basing on the
 * resolver_id type to identify entities.
 *
 * Revision 1.20  2002/03/08 14:37:15  gerd
 * 	Fixed the case that cwd=/
 *
 * Revision 1.19  2002/02/20 00:25:23  gerd
 * 	using Pxp_lexing instead of Lexing.
 *
 * Revision 1.18  2002/02/18 00:25:40  gerd
 * 	Minor enhancement: String.blit only called for n>0
 *
 * Revision 1.17  2001/10/12 21:38:14  gerd
 * 	Changes for O'caml 3.03-alpha.
 *
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
