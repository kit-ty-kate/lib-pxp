(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_core_types;;
open Netchannels;;

module ULB = struct
  type unicode_lexbuf =
	{ mutable ulb_encoding : encoding;
	  mutable ulb_encoding_start : int;
	  mutable ulb_rawbuf : string;
	  mutable ulb_rawbuf_len : int;
	  mutable ulb_rawbuf_end : int;
	  mutable ulb_rawbuf_const : bool;
	  mutable ulb_chars : int array;
	  mutable ulb_chars_pos : int array;
	  mutable ulb_chars_len : int;
	  mutable ulb_eof : bool;
	  mutable ulb_refill : string -> int -> int -> int;
	  mutable ulb_enc_change_hook : unicode_lexbuf -> unit;
	}

  let from_function ?(raw_size = 512) ?(char_size = 250) 
                    ?(enc_change_hook = fun _ -> ())
                    ~refill enc =
    { ulb_encoding = enc;
      ulb_encoding_start = 0;
      ulb_rawbuf = String.create raw_size;
      ulb_rawbuf_len = 0;
      ulb_rawbuf_end = 0;
      ulb_rawbuf_const = false;
      ulb_chars = Array.make char_size (-1);
      ulb_chars_pos = ( let cp = Array.make (char_size+1) (-1) in
			cp.(0) <- 0;
			cp );
      ulb_chars_len = 0;
      ulb_eof = false;
      ulb_refill = refill;
      ulb_enc_change_hook = enc_change_hook;
    }

  let from_string ?(enc_change_hook = fun _ -> ()) enc s =
    let char_size = 250 in
    { ulb_encoding = enc;
      ulb_encoding_start = 0;
      ulb_rawbuf = String.copy s;
      ulb_rawbuf_len = String.length s;
      ulb_rawbuf_end = 0;
      ulb_rawbuf_const = true;
      ulb_chars = Array.make char_size (-1);
      ulb_chars_pos = ( let cp = Array.make (char_size+1) (-1) in
			cp.(0) <- 0;
			cp );
      ulb_chars_len = 0;
      ulb_eof = true;
      ulb_refill = (fun _ _ _ -> assert false);
      ulb_enc_change_hook = enc_change_hook;
    }

  let from_string_inplace ?(enc_change_hook = fun _ -> ()) enc s =
    let char_size = 250 in
    { ulb_encoding = enc;
      ulb_encoding_start = 0;
      ulb_rawbuf = s;
      ulb_rawbuf_len = String.length s;
      ulb_rawbuf_end = 0;
      ulb_rawbuf_const = true;
      ulb_chars = Array.make char_size (-1);
      ulb_chars_pos = ( let cp = Array.make (char_size+1) (-1) in
			cp.(0) <- 0;
			cp );
      ulb_chars_len = 0;
      ulb_eof = true;
      ulb_refill = (fun _ _ _ -> assert false);
      ulb_enc_change_hook = enc_change_hook;
    }

(*
  let append ~src ?(pos = 0) ?len ulb =
    let len =
      match len with
	  None -> Array.length src - pos
	| Some l -> l in
    XXX
*)

  let delete n ulb =
    if n < 0 || n > ulb.ulb_chars_len then
      invalid_arg "ULB.delete";
    let m = ulb.ulb_chars_len - n in
    Array.blit ulb.ulb_chars n ulb.ulb_chars 0 m;
    Array.blit ulb.ulb_chars_pos n ulb.ulb_chars_pos 0 (m+1);

    if not ulb.ulb_rawbuf_const then (
      let k = ulb.ulb_chars_pos.(0) in
      assert (ulb.ulb_rawbuf_end >= k);
      let m' = ulb.ulb_rawbuf_len - k in
      String.blit ulb.ulb_rawbuf k ulb.ulb_rawbuf 0 m';
      let cp = ulb.ulb_chars_pos in
      for i = 0 to m do
	cp.(i) <- cp.(i) - k
      done;

      ulb.ulb_rawbuf_len <- m';
      ulb.ulb_rawbuf_end <- ulb.ulb_rawbuf_end - k;
    );

    ulb.ulb_chars_len  <- m;
    ulb.ulb_encoding_start <- max 0 (ulb.ulb_encoding_start - n)
      
  let set_encoding enc ulb =
    if enc <> ulb.ulb_encoding then (
      ulb.ulb_encoding <- enc;
      ulb.ulb_encoding_start <- ulb.ulb_chars_len;
      ulb.ulb_enc_change_hook ulb
    )

  let close ulb =
    ulb.ulb_eof <- true

  let utf8_sub_string k n ulb =
    if k < 0 || k > ulb.ulb_chars_len || n < 0 || k+n > ulb.ulb_chars_len then
      invalid_arg "ULB.utf8_sub_string";

    if ulb.ulb_encoding = `Enc_utf8 && k >= ulb.ulb_encoding_start then (
      (* Extract the substring from [ulb_rawbuf] ! *)
      let k' = ulb.ulb_chars_pos.(k) in
      let n' = ulb.ulb_chars_pos.(k+n) - k' in
      String.sub ulb.ulb_rawbuf k' n'
    )
    else (
      (* Create the UTF-8 string from [ulb_chars] *)
      Netconversion.ustring_of_uarray `Enc_utf8 ~pos:k ~len:n ulb.ulb_chars
    )

  let utf8_sub_string_length k n ulb =
    if k < 0 || k > ulb.ulb_chars_len || n < 0 || k+n > ulb.ulb_chars_len then
      invalid_arg "ULB.utf8_sub_string_length";

    if ulb.ulb_encoding = `Enc_utf8 && k >= ulb.ulb_encoding_start then (
      (* Extract the substring from [ulb_rawbuf] ! *)
      let k' = ulb.ulb_chars_pos.(k) in
      let n' = ulb.ulb_chars_pos.(k+n) - k' in
      n'
    )
    else (
      (* Count the UTF-8 string from [ulb_chars] *)
      (* Maybe better algorithm: divide into several slices, and call
       * ustring_of_uarray for them. Goal: Reduction of memory allocation
       *)
      let conv = Netconversion.ustring_of_uchar `Enc_utf8 in
      let n' = ref 0 in
      for i = k to k+n-1 do
	n' := !n' + String.length (conv ulb.ulb_chars.(i))
      done;
      !n'
    )


  let rec refill_aux ulb = 
    (* Check whether we cannot add at least one byte to [ulb_chars] because
     * of EOF:
     *)
    if ulb.ulb_eof && ulb.ulb_rawbuf_len = ulb.ulb_rawbuf_end then
      0
    else (

      (* Enlarge [ulb_chars] if necessary (need at least space for one character)
       *)
      if ulb.ulb_chars_len >= Array.length ulb.ulb_chars then (
	let n = min (Sys.max_array_length-1) (2 * (Array.length ulb.ulb_chars)) in
	if n = Array.length ulb.ulb_chars then
	  failwith "ULB.refill: array too large";
	
	let c = Array.make n (-1) in
	let cp = Array.make (n+1) (-1) in
	Array.blit ulb.ulb_chars 0 c 0 ulb.ulb_chars_len;
	Array.blit ulb.ulb_chars_pos 0 cp 0 (ulb.ulb_chars_len+1);
	
	ulb.ulb_chars <- c;
	ulb.ulb_chars_pos <- cp;
      );
      
      (* If there is unanalysed material in [ulb_rawbuf], try to convert it.
       * It may happen, however, that there is only the beginning of a 
       * multi-byte character, so this may not add any new character.
       *)
      let new_chars =
	if ulb.ulb_rawbuf_end < ulb.ulb_rawbuf_len then (
	  let cs = Netconversion.create_cursor 
		     ~range_pos:ulb.ulb_rawbuf_end
		     ~range_len:(ulb.ulb_rawbuf_len - ulb.ulb_rawbuf_end)
		     ulb.ulb_encoding
		     ulb.ulb_rawbuf in
	  let counter = ref 0 in
	  ( try
	      while ulb.ulb_chars_len < Array.length ulb.ulb_chars do
		let space = Array.length ulb.ulb_chars - ulb.ulb_chars_len in
		(* cursor_blit may raise End_of_string, too *)
		let n = Netconversion.cursor_blit 
			  cs ulb.ulb_chars ulb.ulb_chars_len space in
		let n' = Netconversion.cursor_blit_positions
			   cs ulb.ulb_chars_pos ulb.ulb_chars_len space in
		assert(n=n');
		if n>0 then (
		  ulb.ulb_chars_len <- ulb.ulb_chars_len+n;
		  counter := !counter + n;
		  Netconversion.move ~num:n cs; (* may raise Malformed_code *)
		) 
		else (
		  (* We are at a special position in the string! *)
		  try ignore(Netconversion.uchar_at cs); assert false
		  with
		      Netconversion.Byte_order_mark ->
			(* Skip the BOM: *)
			Netconversion.move cs   (* may raise Malformed_code *)
			(* Note: this [move] does not count *)
		    | Netconversion.Partial_character ->
			(* Stop here *)
			raise Exit
		    (* Netconversion.End_of_string: already handled *)
		)
	      done
	    with
		Exit ->
		  ()
	      | Netconversion.End_of_string ->
		  ()
	  );
	  
	  let e = Netconversion.cursor_pos cs; in
	  ulb.ulb_chars_pos.(ulb.ulb_chars_len) <- e;
	  ulb.ulb_rawbuf_end <- e;

	  (* Encoding might have changed: *)
	  set_encoding (Netconversion.cursor_encoding cs) ulb;
	
	  !counter
	)
	else
	  0
      in
      
      (* In the case we still did not add any char: Check if we are near
       * EOF (the last multi-byte character is not complete).
       *)
      if new_chars = 0 then (
	if ulb.ulb_eof then raise Netconversion.Malformed_code;

	assert(not ulb.ulb_rawbuf_const);

	(* Now try to get new data into [ulb_rawbuf]. First, we check whether
	 * we have enough free space in this buffer. We insist on at least
	 * 50 bytes (quite arbitrary...). Then call the [ulb_refill] function
	 * to get the data.
	 *)
	if ulb.ulb_rawbuf_len + 50 >= String.length ulb.ulb_rawbuf then (
	  let n = min Sys.max_string_length (2 * (String.length ulb.ulb_rawbuf)) in
	  if n = String.length ulb.ulb_rawbuf then
	    failwith "ULB.refill: string too large";
	  
	  let s = String.create n in
	  String.blit ulb.ulb_rawbuf 0 s 0 ulb.ulb_rawbuf_len;
	  ulb.ulb_rawbuf <- s;
	);
	
	(* Call now [ulb_refill]. If we detect EOF, record this. Anyway,
	 * start over.
	 *)
	let space = (String.length ulb.ulb_rawbuf) - ulb.ulb_rawbuf_len in
	let n = ulb.ulb_refill ulb.ulb_rawbuf ulb.ulb_rawbuf_len space in
	assert(n>=0);
	if n=0 then (
	  (* EOF *)
	  ulb.ulb_eof <- true;
	)
	else (
	  ulb.ulb_rawbuf_len <- ulb.ulb_rawbuf_len + n
	);
	
	refill_aux ulb
      )
      else
	new_chars
    )

  let refill ulb =
    let n = refill_aux ulb in
    assert(n>=0);
    if n=0 then (
      assert(ulb.ulb_eof);
      assert(ulb.ulb_rawbuf_len = ulb.ulb_rawbuf_end);
      raise End_of_file
    )

end


module Ulexing = struct
  type lexbuf =
      { ulb : ULB.unicode_lexbuf;
	mutable offset : int;
	mutable pos : int;
	mutable start : int;
	mutable marked_pos : int;
	mutable marked_val : int;
      }

  exception Error

  let from_ulb_lexbuf ulb =
    { ulb = ulb;
      offset = 0;
      pos = 0;
      start = 0;
      marked_pos = 0;
      marked_val = 0;
    }

  let lexeme_start lexbuf = lexbuf.start + lexbuf.offset
  let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset
  let lexeme_length lexbuf = lexbuf.pos - lexbuf.start

  let lexeme lexbuf =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    Array.sub buf lexbuf.start (lexbuf.pos - lexbuf.start)

  let sub_lexeme lexbuf pos len =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    Array.sub buf (lexbuf.start + pos) len

  let lexeme_char lexbuf pos =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    buf.(lexbuf.start + pos)

  let utf8_lexeme lexbuf =
    ULB.utf8_sub_string lexbuf.start (lexbuf.pos - lexbuf.start) lexbuf.ulb

  let utf8_sub_lexeme lexbuf pos len =
    ULB.utf8_sub_string (lexbuf.start + pos) len lexbuf.ulb

  let utf8_sub_lexeme_length lexbuf pos len =
    ULB.utf8_sub_string_length (lexbuf.start + pos) len lexbuf.ulb
    
  (* "Internal" interface *)
  let start lexbuf =
    lexbuf.start <- lexbuf.pos;
    lexbuf.marked_pos <- lexbuf.pos;
    lexbuf.marked_val <- (-1)

  let mark  lexbuf i =
    lexbuf.marked_pos <- lexbuf.pos;
    lexbuf.marked_val <- i
      
  let backtrack lexbuf =
    lexbuf.pos <- lexbuf.marked_pos;
    lexbuf.marked_val

  let rollback lexbuf =
    lexbuf.pos <- lexbuf.start

  let eof = (-1)

  let refill lexbuf = 
    try
      (* Delete all characters in ulexbuf before the current lexeme: *)
      if lexbuf.start > 0 then (
	let n = lexbuf.start in
	ULB.delete n lexbuf.ulb;
	lexbuf.offset <- lexbuf.offset + n;
	lexbuf.pos <- lexbuf.pos - n;
	lexbuf.marked_pos <- lexbuf.marked_pos - n;
	lexbuf.start <- 0;
      );
      ULB.refill lexbuf.ulb;
      (* raises either End_of_file, or ensures there is one char in ulb *)
      lexbuf.ulb.ULB.ulb_chars.(lexbuf.pos)
    with
	End_of_file ->
	  (* We cannot modify the buffer as the original Ulexing implementation
	   *)
	  eof
	  

  let next lexbuf =
    let ulb = lexbuf.ulb in
    let i =
      if lexbuf.pos = ulb.ULB.ulb_chars_len then
	refill lexbuf
      else 
	ulb.ULB.ulb_chars.(lexbuf.pos)
    in
    if i <> eof then lexbuf.pos <- lexbuf.pos + 1;
    i

end


exception Not_competent;;
exception Not_resolvable of exn;;

type lexer_source =
    { lsrc_lexbuf : Lexing.lexbuf Lazy.t;
      lsrc_unicode_lexbuf : ULB.unicode_lexbuf Lazy.t;
    }



let ensure_space_minimum p f g =
  (* The functional [f] is called with a function as argument that refills
   * a buffer string (think f = Lexing.from_function). This argument is [g],
   * i.e. in most cases this is the same as [f g]. However, it is ensured
   * that the string buffer has the minimum free space [p]. This is achieved
   * by using an auxiliary buffer.
   *)
  let buf = String.create p in
  let bufpos = ref 0 in
  let buflen = ref 0 in
  f (fun s n ->
       assert(n>0);
       if !buflen > 0 then (
	 let m = min n !buflen in
	 String.blit buf !bufpos s 0 m;
	 bufpos := !bufpos + m;
	 buflen := !buflen - m;
	 m
       )
       else
	 if n < p then (
	   let l = g buf p in
	   let m = min l n in
	   String.blit buf 0 s 0 m;
	   bufpos := m;
	   buflen := l-m;
	   m
	 )
	 else 
	   g s n
    )
;;


class type resolver =
  object
    method init_rep_encoding : rep_encoding -> unit
    method init_warner : symbolic_warnings option -> collect_warnings -> unit
    method rep_encoding : rep_encoding
    method open_in : ext_id -> lexer_source
    method open_rid : resolver_id -> lexer_source
    method active_id : resolver_id
    method close_in : unit
    (* method close_all : unit *)
    (* [close_all] is no longer supported in PXP 1.2 *)
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

    val mutable mode = None
		(* Whether the [lexbuf_reader] or the [unicode_lexbuf_reader]
		 * is used. One can only invoke one of them.
		 *)

    val mutable encoding = `Enc_utf8
    val mutable encoding_requested = false
    val mutable encoding_request_post = (fun () -> ()) (* post action *)

    val mutable active_id = null_resolver

    val mutable warner = new drop_warnings
    val mutable swarner = None

    val mutable enc_initialized = false
    val mutable wrn_initialized = false


    method init_rep_encoding e =
      internal_encoding <- e;
      enc_initialized <- true;

    method init_warner sw w =
      warner <- w;
      swarner <- sw;
      wrn_initialized <- true;

    method rep_encoding = (internal_encoding :> rep_encoding)


    method private warn (k:int) =
      (* Called if a character not representable has been found.
       * k is the character code.
       *)
	if k < 0xd800 or (k >= 0xe000 & k <= 0xfffd) or
	   (k >= 0x10000 & k <= 0x10ffff) then begin
	     warn swarner warner (`W_code_point_cannot_be_represented k);
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
       *
       * Note: Four bytes are required for cases not yet handled 
       * (e.g. UTF-32).
       *)
      if String.sub s 0 2 = "\254\255" then
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


    method private lexbuf_reader () =
      if mode <> None && mode <> Some `Lexbuf then 
	failwith "lexbuf_reader: other reader already working";

      mode <- Some `Lexbuf;
      let direct_reader = ref false in  (* whether to bypass the buffer *)

      let buf_max = 4096 in
      let buf     = ref (String.make buf_max ' ')  in
      let buf_beg = ref 0 in
      let buf_end = ref 0 in
      let buf_eof = ref false in
      (* The buffer is used if [not direct_reader]. [buf_beg] is the 
       * beginning of the filled part of the buffer, [buf_end] is the
       * end (plus 1). [buf_eof] indicates that EOF was already signaled,
       * and should be processed after the current contents.
       *)

      let refill() =
	(* Refill the buffer from [next_string].
	 * It is important to refill as much as possible for the
	 * algorithm below.
	 *)
	while not !buf_eof && !buf_end < buf_max do
	  let n = self # next_string !buf !buf_end (buf_max - !buf_end) in
	  if n=0 then
	    buf_eof := true
	  else
	    buf_end := !buf_end + n
	done
      in

      let convert s n max_chars =
	(* Convert characters from [buf] to [s], [n]. The number of characters
	 * it limited by [max_chars]
	 *)
	assert(n>=6);
	if !buf_beg + 6 > !buf_end && not !buf_eof then (
	  (* Less than 6 bytes in [buf]. We need at least this
	   * number of bytes, which is the length of the longest
	   * UTF-8 char, otherwise we cannot ensure to convert
	   * at least one character.
	   *)
	  let m = !buf_end - !buf_beg in
	  String.blit !buf !buf_beg !buf 0 m;
	  buf_beg := 0;
	  buf_end := m;
	  refill();
	);

	(* It is still possible that there are less than 6 bytes
	 * in [buf], but only if [buf_eof] is true at the same
	 * time.
	 *)
	
	let m = !buf_end - !buf_beg in
	if m=0 then (
	  assert !buf_eof;
	  0  (* EOF! *)
	)
	else (
	  let (n_in, n_out, encoding') =
	    Netconversion.recode
	      ~in_enc:encoding
	      ~in_buf:!buf
	      ~in_pos:!buf_beg
	      ~in_len:m
	      ~out_enc:(internal_encoding : rep_encoding :> encoding)
	      ~out_buf:s
	      ~out_pos:0
	      ~out_len:n
	      ~max_chars
	      ~subst:(fun k -> self # warn k; "") in
	  if n_in = 0 then (
	    (* An incomplete character at the end of the stream. 
	     * Note: This test assumes that there is one character to 
	     * convert in [buf], and that there is enough free space in [s].
	     * This is the case because we ensure [m>=6] and [n>=6].
	     *)
	    assert !buf_eof;
	    raise Netconversion.Malformed_code;
	  );
	  encoding <- encoding';
	  buf_beg := !buf_beg + n_in;
	  
	  assert(n_out > 0);
	  n_out
	)
      in

      (* Fill the buffer initially. We start always with buffered reading,
       * but we try to switch to direct reading later.
       *)
      refill();
      if !buf_end >= 4 && not encoding_requested then self # autodetect !buf;

      (* Ensure that [n >= 6], the longest UTF-8 character, so we can always
       * put at least one character into [s]
       *)
      ensure_space_minimum 6
	Lexing.from_function
	(fun s n ->
	   (* Fill the string [s] with at most [n] bytes. Return the number
	    * of bytes, or 0 to signal EOF.
	    *)
	   if not is_open then
	     failwith "trying to read from resolver, but resolver is not open";
	   
	   if !direct_reader then
	     self # next_string s 0 n 
	   else (
	     if encoding_requested then (
	       (* In this case, the encoding will not change any more. We
		* can read any number of characters at once.
		*)
	       if encoding = (internal_encoding : rep_encoding :> encoding)
	       then (
		 (* No conversion is needed. In order to speed up reading,
		  * we are going to enable [direct_reader].
		  *)
		 if !buf_beg < !buf_end then (
		   (* There are still bytes in [buf], return them first *)
		   let m = min n (!buf_end - !buf_beg) in
		   String.blit !buf !buf_beg s 0 m;
		   buf_beg := !buf_beg + m;
		   m
		 )
		 else (
		   (* Either we are already at EOF, or we can switch to
		    * [direct_reader].
		    *)
		   buf := "";  (* Free buf, it will never be used again *)
		   if !buf_eof then
		     0
		   else (
		     direct_reader := true;
		     self # next_string s 0 n
		   )
		 )
	       )
	       else (
		 (* Character conversion is needed from [encoding] to
		  * [internal_encoding].
		  *)
		 convert s n max_int
	       )
	     )
	     else (
	       (* In this case, the encoding might change at any time.
		* Because of this, we only read one character at a time.
		*)
	       convert s n 1
	     )
	   )
	)


    method private unicode_lexbuf_reader () =
      if mode <> None && mode <> Some `Unicode_lexbuf then 
	failwith "unicode_lexbuf_reader: other reader already working";

      mode <- Some `Unicode_lexbuf;

      let buf = Netbuffer.create 4 in
      (* Only used for autodetection! *)

      let buf_eof = ref false in

      if not encoding_requested then (
	while not !buf_eof && Netbuffer.length buf < 4 do
	  let n =
	    Netbuffer.add_inplace ~len:(4-Netbuffer.length buf) buf self#next_string
	  in
	  buf_eof := (n=0)
	done;
	if Netbuffer.length buf >= 4  then
	  self # autodetect (Netbuffer.contents buf);
      );

      let lexbuf =
	ULB.from_function
	  ~enc_change_hook:(
	    fun ulb -> 
	      encoding <- ulb.ULB.ulb_encoding )
	  ~refill:(
	    fun s p n ->
	      (* Fill the string [s] at position [p] with at most [n] bytes. 
	       * Return the number of bytes, or 0 to signal EOF.
	       *)
	      if not is_open then
		failwith "trying to read from resolver, but resolver is not open";
	      
	      if encoding_requested && Netbuffer.length buf = 0 then (
		(* In this case, the encoding will not change any more. We
		 * can read any number of characters at once.
		 *)
		self # next_string s p n
	      )
	      else (
		(* In this case, the encoding might change at any time.
		 * Because of this, we only read one character at a time.
		 * If there are still characters in [buf], take these first.
		 *)
		
		if Netbuffer.length buf > 0 then (
		  let c = (Netbuffer.contents buf).[0] in
		  s.[p] <- c;
		  Netbuffer.delete buf 0 1;
		  1
		)

		else (
		  if !buf_eof then
		    0 (* EOF already seen *)
		  else
		    self # next_string s p 1
		)
	      )
	  )
	  encoding
      in

      encoding_request_post <- (
	fun () ->
	  (* This function is called after the encoding was requested for
	   * the first time
	   *)  
	  ULB.set_encoding encoding lexbuf
      );

      lexbuf

    method open_in xid =
      self # open_rid (resolver_id_of_ext_id xid)

    method open_rid rid =
      assert(enc_initialized && wrn_initialized);

      encoding <- `Enc_utf8;
      encoding_requested <- false;
      self # init_in rid;         (* may raise Not_competent *)
      (* init_in: may already set 'encoding' *)

      is_open <- true;

      { lsrc_lexbuf         = lazy(self # lexbuf_reader() );
	lsrc_unicode_lexbuf = lazy(self # unicode_lexbuf_reader() );
      }

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
	encoding_request_post();
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
	    try
	      ch # input s ofs len
	    with
		End_of_file -> 0

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
      c # init_warner swarner warner;
      (* clones <- c :: clones; *)
      (c :> resolver)

end
;;


let rid_rid_intersection bound_rid actual_rid =
  (* Returns a resolver_id where unequal IDs are reset to None. The
   * rid_system_base is set corresponding to rid_system.
   *
   * Notes: 
   * (1) an empty SYSTEM name does not match another empty SYSTEM name
   * (2) PUBLIC names must be normalized
   *)
  let isect opt1 opt2 =
    if opt1 = opt2 then opt1 else None
  in
  let sys_isect opt1 opt2 =
    if opt1 = opt2 && opt1 <> Some "" then opt1 else None
  in
  { rid_private = isect bound_rid.rid_private actual_rid.rid_private;
    rid_public  = isect bound_rid.rid_public actual_rid.rid_public;
    rid_system  = sys_isect bound_rid.rid_system actual_rid.rid_system;
    rid_system_base = if bound_rid.rid_system = actual_rid.rid_system  &&
                         bound_rid.rid_system <> None &&
                         bound_rid.rid_system <> Some ""
                      then
                        actual_rid.rid_system_base
                      else
			None;
  }
;;


let rid_matches_rid bound_rid actual_rid =
  (* definition:
   * rid_matches_rid r1 r2 =def= 
   * rid_rid_intersection r1 r2 <> null_resolver
   *
   * See also the notes for rid_rid_intersection
   *)
  (bound_rid.rid_private <> None && 
   bound_rid.rid_private = actual_rid.rid_private) ||
  (bound_rid.rid_public <> None && 
   bound_rid.rid_public = actual_rid.rid_public) ||
  (bound_rid.rid_system <> None && 
   bound_rid.rid_system <> Some "" && 
   bound_rid.rid_system = actual_rid.rid_system)
;;


let xid_rid_intersection bound_xid actual_rid =
  rid_rid_intersection (resolver_id_of_ext_id bound_xid) actual_rid
;;


let xid_matches_rid bound_xid actual_rid =
  (* definition:
   * xid_matches_rid x r =def= 
   * xid_rid_intersection x r <> null_resolver
   *
   * See also the notes for rid_rid_intersection
   *)
  match bound_xid with
      System sys -> 
	sys <> "" && actual_rid.rid_system = Some sys
    | Public(pub,sys) ->
	(actual_rid.rid_public = Some pub) || 
	(sys <> "" && actual_rid.rid_system = Some sys)
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
      c # init_warner swarner warner;
      (* clones <- c :: clones; *)
      (c :> resolver)

  end
;;


class resolve_to_this_obj_channel ?id ?rid ?fixenc ?close ch =
  let is_stale = ref false in
  resolve_to_this_obj_channel1 is_stale ?id ?rid ?fixenc ?close ch
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


class lookup_id_nonorm (catalog : (ext_id * resolver) list) =
( object (self)
    val cat = catalog
    val mutable internal_encoding = `Enc_utf8
    val mutable warner = new drop_warnings
    val mutable swarner = None
    val mutable active_resolver = None

    method init_rep_encoding enc =
      internal_encoding <- enc

    method init_warner sw w =
      swarner <- sw;
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
      r' # init_warner swarner warner;
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

(*
    method close_all =
      (* CHECK: Müssen nicht die Klone auch geschlossen werden? *)
      self # close_in
*)

    method change_encoding (enc:string) =
      match active_resolver with
	  None      -> failwith "Pxp_reader.lookup_* # change_encoding"
	| Some(_,r) -> r # change_encoding enc


    method clone =
      let c = new lookup_id_nonorm cat in
      c # init_rep_encoding internal_encoding;
      c # init_warner swarner warner;
      c
  end : resolver )
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
  lookup_id_nonorm norm_catalog
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
    val mutable swarner = None
    val mutable active_resolver = None

(*  (* needed to support close_all: *)
    val mutable clones = []
*)

    method init_rep_encoding enc =
      List.iter
	(fun r -> r # init_rep_encoding enc)
	rl;
      internal_encoding <- enc

    method init_warner sw w =
      List.iter
	(fun r -> r # init_warner sw w)
	rl;
      swarner <- sw;
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

(*
    method close_all =
      List.iter (fun r -> r # close_in) clones
*)

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
      c # init_warner swarner warner;
      (* clones <- c :: clones; *)
      c
  end
;;


let norm_url_syntax =
  { Neturl.null_url_syntax with
      Neturl.url_enable_scheme = Neturl.Url_part_allowed;
      Neturl.url_enable_user = Neturl.Url_part_allowed;
      Neturl.url_enable_password = Neturl.Url_part_allowed;
      Neturl.url_enable_host = Neturl.Url_part_allowed;
      Neturl.url_enable_port = Neturl.Url_part_allowed;
      Neturl.url_enable_path = Neturl.Url_part_required;
      (* rest: Url_part_not_recognized *)
      Neturl.url_accepts_8bits = true;
  }
;;


class norm_system_id (subresolver : resolver) =
object(self)
  val subresolver = subresolver
  val mutable current_rid = null_resolver  (* for rewrite_system_id *)

  method init_rep_encoding enc =
    subresolver # init_rep_encoding enc

  method init_warner sw w =
    subresolver # init_warner sw w;

  method rep_encoding =
    subresolver # rep_encoding

  method open_in xid =
    (* It is not possible to normalize the SYSTEM id of a xid *)
    subresolver # open_in xid

  method open_rid rid =
    (* (1) check that the system name is a URL
     * (2) if the URL is relative: make it absolute (use system base name)
     * (3) remove .. and . from the URL path as much as possible
     * (4) all other names are left unmodified
     *)

    let norm sysname =
      try
	(* prerr_endline ("sysname=" ^ sysname); *)
	let sysurl = Neturl.url_of_string norm_url_syntax sysname in
	let sysurl_abs =
	  if Neturl.url_provides ~scheme:true sysurl then
	    sysurl
	  else
	    match rid.rid_system_base with
		None -> 
		  (* The sysurl is relative, but we do not have a base URL.
		   * There is no way to interpret this case, so we reject
		   * it.
		   *)
		  raise Not_competent
	      | Some sysbase -> 
		  let baseurl = Neturl.url_of_string norm_url_syntax sysbase in
		  Neturl.apply_relative_url baseurl sysurl
	in
	let path = Neturl.url_path sysurl_abs in
	let path' = Neturl.norm_path path in  (* remove .., ., // *)
	let sysurl' = Neturl.modify_url ~path:path' sysurl_abs in
	(* prerr_endline ("Before rewrite: " ^ Neturl.string_of_url sysurl');
	 *)
	let sysurl'' = self # rewrite sysurl' in
	let sysname' = Neturl.string_of_url sysurl'' in
	(* prerr_endline ("sysname'=" ^ sysname'); *)
	sysname'
      with
	  Neturl.Malformed_URL ->
	    raise Not_competent
    in
      
    let rid' =
      { rid with
	  rid_system = ( match rid.rid_system with
			     None -> 
			       None
			   | Some sysname ->
			       Some(norm sysname)
		       )
      }
    in
    let lex = subresolver # open_rid rid' in
    current_rid <- rid;   (* the original, unmodified version! *)
    lex

  method private rewrite sysurl = sysurl

  method close_in =
    subresolver # close_in

(*
  method close_all =
    subresolver # close_all
*)

  method change_encoding enc =
    subresolver # change_encoding enc

  method active_id =
    subresolver # active_id

  method clone =
    let c = subresolver # clone in
    ( {< subresolver = c >} :> resolver )
end
;;


let try_to_get f arg =
  try Some(f arg) with Not_found -> None
;;


let remove_trailing_slash p =
  match p with
      [] -> []
    | [""] -> [""]
    | _ -> 
	let p' = List.rev p in
	if List.hd p' = "" then
	  List.rev(List.tl p')
	else
	  p
;;


let rec path_matches pattern p =
  match (pattern, p) with
      ( [], [] ) ->
	(* Case: pattern = p *)
	true
    | ( [""], (_::_) ) ->
	(* Case: pattern ends with slash, and is a prefix of p *)
	true
    | ( (pat :: pattern'), (p0 :: p') ) when pat = p0 ->
	path_matches pattern' p'
    | _ ->
	false
;;


let rec path_subst pattern subst p =
  match (pattern, p) with
      ( [], [] ) ->
	(* Case: pattern = p *)
	subst
    | ( [""], (_::_) ) ->
	(* Case: pattern ends with slash, and is a prefix of p *)
	(* If subst ends with a slash, remove it *)
	let subst' = remove_trailing_slash subst in
	subst' @ p
    | ( (pat :: pattern'), (p0 :: p') ) when pat = p0 ->
	path_subst pattern' subst p'
    | _ ->
	assert false  (* no match *)
;;


class rewrite_system_id ?(forward_unmatching_urls=false) rw_spec subresolver =
object(self)
  inherit norm_system_id subresolver
  val forward_unmatching_urls = forward_unmatching_urls
  val rw_spec = 
    List.map
      (fun (sysfrom, systo) ->
	 let sysfrom_url = 
	   Neturl.url_of_string norm_url_syntax sysfrom in
	 let systo_url =
	   Neturl.url_of_string norm_url_syntax systo in
	 (* if sysfrom_url ends with a slash, systo_url must end with it, too
	  *)
	 let ends_with_slash url =
	   let path = Neturl.url_path url in
	   List.hd (List.rev path) = ""
	 in
	 if ends_with_slash sysfrom_url && not(ends_with_slash systo_url) then
	   failwith "Illegal rewrite specification: Cannot map directory to non-directory";
	 (sysfrom_url, systo_url)
      )
      rw_spec

  method private rewrite url =
    try
      let sysfrom_url, systo_url =
	List.find                              (* may raise Not_found *)
	  (fun (sysfrom_url, systo_url) ->
	     (* Check whether url matches sysfrom_url *)
	     (try_to_get Neturl.url_scheme sysfrom_url =
		try_to_get Neturl.url_scheme url) &&
	     (try_to_get Neturl.url_user sysfrom_url =
		try_to_get Neturl.url_user url) &&
	     (try_to_get Neturl.url_password sysfrom_url =
		try_to_get Neturl.url_password url) &&
	     (try_to_get Neturl.url_host sysfrom_url =
		try_to_get Neturl.url_host url) &&
	     (try_to_get Neturl.url_port sysfrom_url =
		try_to_get Neturl.url_port url) &&
	     (let sysfrom_p = Neturl.url_path sysfrom_url in
	      let p = Neturl.url_path url in
	      (List.hd sysfrom_p = "") &&   (* i.e. sysfrom_p is absolute *)
	      (List.hd p = "") &&           (* i.e. p is absolute *)
	      (path_matches sysfrom_p p))
	  )
	  rw_spec
      in
      (* prerr_endline("sysfrom_url=" ^ Neturl.string_of_url sysfrom_url);
	 prerr_endline("systo_url=" ^ Neturl.string_of_url systo_url);
      *)
      let sysfrom_p = Neturl.url_path sysfrom_url in
      let systo_p = Neturl.url_path systo_url in
      let p = Neturl.url_path url in
      let p' = path_subst sysfrom_p systo_p p in
      Neturl.modify_url ~path:p' systo_url
    with
	Not_found ->
	  if forward_unmatching_urls then
	    url
	  else
	    raise Not_competent

  method active_id =
    (* hide the rewritten URL *)
    let aid = subresolver # active_id in
    { aid with
	rid_system = current_rid.rid_system;
	rid_system_base = current_rid.rid_system_base;
    }
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
  ) @ 
  [ Anonymous ]
  (* We always append Anonymous, because this xid is included in all rids *)
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


class resolve_read_this_string ?id ?fixenc s =
  let string_of_id tried_xid =
    match id with
	None ->
	  (* Open always! *)
	  (s, fixenc)
      | Some my_xid ->
	  if my_xid = tried_xid then
	    (s, fixenc)
	  else
	    raise Not_competent
  in
  resolve_read_any_string ~string_of_id ()
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




