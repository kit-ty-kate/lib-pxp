(* $Id: pxp_reader.ml,v 1.1 2000/05/29 23:48:38 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_types;;

class type resolver =
  object
    method open_in : ext_id -> Lexing.lexbuf
    method close_in : unit
    method change_encoding : string -> unit
    method clone : resolver
  end
;;


class virtual resolve_general (the_warner : collect_warnings) 
                              (init_internal_encoding : rep_encoding)
 =
  object (self)
    val internal_encoding = init_internal_encoding

    val mutable encoding = `Enc_utf8
    val mutable encoding_requested = false

    val warner = the_warner


    method clone =
      ( {< encoding = `Enc_utf8;
	   encoding_requested = false;
	>}
	: # resolver :> resolver )


    method private warn (k:int) =
      (* Called if a character not representable has been found.
       * k is the character code.
       *)
	if k < 0xd800 or (k >= 0xe000 & k <= 0xfffd) or
	   (k >= 0x10000 & k <= 0x10ffff) then begin
	     warner # warn ("Code point cannot be represented: " ^ string_of_int k);
	   end
	else
	  raise (Illegal_character 0)


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
	  (* Note: Pxp_encoding.recode will detect the big endianess, too *)
      else if String.sub s 0 2 = "\255\254" then
	encoding <- `Enc_utf16
	  (* Note: Pxp_encoding.recode will detect the little endianess, too *)
      else
	encoding <- `Enc_utf8


    method private virtual next_string : string -> int -> int -> int
    method private virtual init_in : ext_id -> unit
    method virtual close_in : unit

    method open_in xid =
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

      encoding <- `Enc_utf8;
      encoding_requested <- false;
      self # init_in xid;
      fillup();
      self # autodetect buffer;

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
	     let m_out = if encoding_requested then n else 1 in
	     let n_in, n_out, encoding' =
	       if encoding = (internal_encoding : rep_encoding :> encoding) 
	       then begin
		 (* Special case encoding = internal_encoding *)
		 let n = min m_in m_out in
		 String.blit buffer 0 s 0 n;
		 n, n, encoding
	       end
	       else
		 Pxp_encoding.recode
		   encoding
		   buffer
		   m_in
		   (internal_encoding : rep_encoding :> encoding)
		   s
		   m_out
		   (fun k -> self # warn k; "")
	     in
	     if n_in = 0 then
	       (* An incomplete character at the end of the stream: *)
	       raise Bad_character_stream;
	       (* failwith "Badly encoded character"; *)
	     encoding <- encoding';
	     consume n_in;
	     n_out
	   end)

    method change_encoding enc =
      begin match String.uppercase enc with
	  ("UTF-16"|"UTF16"|"ISO-10646-UCS-2") ->
	    (match encoding with
		 (`Enc_utf16_le | `Enc_utf16_be) -> ()
	       | `Enc_utf16 -> assert false
	       | _ ->
		   raise(WF_error "Encoding of data stream and encoding declaration mismatch")
	    )
	| "UTF-16-BE" ->
	    (match encoding with
		 `Enc_utf16_be -> ()
	       | `Enc_utf16 -> assert false
	       | _ ->
		   raise(WF_error "Encoding of data stream and encoding declaration mismatch")
	    )
	| "UTF-16-LE" ->
	    (match encoding with
		 `Enc_utf16_le -> ()
	       | `Enc_utf16 -> assert false
	       | _ ->
		   raise(WF_error "Encoding of data stream and encoding declaration mismatch")
	    )
	| ("UTF-8"|"UTF8") ->
	    encoding <- `Enc_utf8
	| ("ISO-8859-1"|"ISO88591"|"ISO8859-1"|"ISO-88591") ->
	    encoding <- `Enc_iso88591
	| "" ->
	    ()
	| _ ->
	    raise (WF_error "Unsupported character encoding")
      end;
      encoding_requested <- true;
  end
;;


class resolve_read_channel ch the_warner init_internal_encoding =
  object (self)
    inherit resolve_general the_warner init_internal_encoding

    (* This resolver takes the in_channel ch as character source. *)
    val ch = (ch : in_channel)
    method private init_in (_:ext_id) = ()
    method private next_string s ofs len =
      input ch s ofs len
    method close_in = ()
    method clone =
      failwith "This resolver for external references cannot open this entity"

  end
;;


class resolve_read_string str init_internal_encoding =
  object (self)
    inherit resolve_general (new collect_warnings) init_internal_encoding

    (* This resolver takes the string str as character source. *)
    val str = (str : string)
    val mutable pos = 0
    method private init_in (_:ext_id) = ()
    method private next_string s ofs len =
      let l = min len (String.length str - pos) in
      String.blit str pos s ofs l;
      pos <- pos + l;
      l
    method close_in = ()
    method clone =
      failwith "This resolver for external references cannot open this entity"
  end
;;


class resolve_as_file the_warner init_internal_encoding =
  object (self)
    inherit resolve_general the_warner init_internal_encoding as super

    (* This resolver interprets file names given as system identifiers. *)
    (* TODO:
     * - The file: prefix should be recognized
     * - How are relative paths interpreted?
     *)
    val mutable ch = stdin
    val mutable directory = "."

    method clone =
      ( {< encoding = `Enc_utf8;
	   encoding_requested = false;
	   ch = stdin;
	>}
	: #resolver :> resolver
      )

    method private init_in xid =
      let open_file fname =
	let fname' =
	  if Filename.is_relative fname then
	    Filename.concat directory fname
	  else
	    fname
	in
	open_in fname', Filename.dirname fname'
      in

      let the_ch, the_directory =
	match xid with
	    System fname     -> open_file fname
	  | Public (_,fname) -> open_file fname
      in
      ch <- the_ch;
      directory <- the_directory

    method private next_string s ofs len =
      input ch s ofs len

    method close_in =
      close_in ch

  end
;;


(* ======================================================================
 * History:
 * 
 * $Log: pxp_reader.ml,v $
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
