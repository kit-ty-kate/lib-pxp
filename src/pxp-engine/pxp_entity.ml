(* $Id: pxp_entity.ml,v 1.5 2000/07/09 17:51:50 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


(* TODO:
 * - Wie verhindert man, dass ein internal entity eine XML-Dekl. im 
 *   replacement text akzeptiert?
 *)


open Pxp_types
open Pxp_lexer_types
open Pxp_aux
open Pxp_reader

(* Hierarchy of parsing layers:
 *
 * - Parser: Pxp_yacc
 *   + gets input stream from the main entity object
 *   + checks most of the grammar
 *   + creates the DTD object as side-effect
 *   + creates the element tree as side-effect
 *   + creates further entity objects that are entered into the DTD
 * - Entity layer: Pxp_entity
 *   + gets input stream from the lexers, or another entity object
 *   + handles entity references: if a reference is encountered the
 *     input stream is redirected such that the tokens come from the
 *     referenced entity object
 *   + handles conditional sections
 * - Lexer layer: Pxp_lexers
 *   + gets input from lexbuffers created by resolvers
 *   + different lexers for different lexical contexts
 *   + a lexer returns pairs (token,lexid), where token is the scanned
 *     token, and lexid is the name of the lexer that must be used for
 *     the next token
 * - Resolver layer: Pxp_entity
 *   + a resolver creates the lexbuf from some character source
 *   + a resolver recodes the input and handles the encoding scheme
 *)

(**********************************************************************)

(* Variables of type 'state' are used to insert Begin_entity and End_entity
 * tokens into the stream.
 * - At_beginning: Nothing has been read so far
 * - First_token tok: A Begin_entity has been inserted; and the next token
 *   is 'tok' which is not Eof. (Begin_entity/End_entity must not be inserted
 *   if the entity is empty.)
 * - In_stream: After the first token has been read, but befor Eof.
 * - At_end: Eof has been read, and End_entity has been returned.
 *)

type state =
    At_beginning
  | Inserted_begin_entity
  | At_end
;;


(**********************************************************************)

class virtual entity the_dtd the_name the_warner 
              init_errors_with_line_numbers init_encoding =
  object (self)
    (* This class prescribes the type of all entity objects. Furthermore,
     * the default 'next_token' mechanism is implemented.
     *)

    (* 'init_errors_with_line_numbers': whether error messages contain line
     * numbers or not.
     * Calculating line numbers is expensive.
     *)

    val mutable dtd = the_dtd
    val mutable name = the_name
    val mutable warner = the_warner

    val encoding = (init_encoding : rep_encoding)
    val lexerset = Pxp_lexers.get_lexer_set init_encoding

    method encoding = encoding
    (* method lexerset = lexerset *)

    val mutable manager = None
      (* The current entity_manager, see below *)

    method private manager = 
      ( match manager with
	    None -> assert false
	  | Some m -> m
      : < current_entity : entity; 
	  pop_entity : unit;
	  push_entity : entity -> unit >
      )

    method set_manager m = manager <- Some m


    val mutable lexbuf = Lexing.from_string ""
      (* The lexical buffer currently used as character source. *)

    val mutable prolog = None
      (* Stores the initial <?xml ...?> token as PI_xml *)

    val mutable prolog_pairs = []
      (* If prolog <> None, these are the (name,value) pairs of the
       * processing instruction.
       *)


    val mutable lex_id = Document
      (* The name of the lexer that should be used for the next token *)

    method set_lex_id id = lex_id <- lex_id



    val mutable force_parameter_entity_parsing = false
      (* 'true' forces that inner entities will always be embraced by
       *        Begin_entity and End_entity.
       * 'false': the inner entity itself decides this
       *)

    val mutable check_text_declaration = true
      (* 'true': It is checked that the <?xml..?> declaration matches the
       *         production TextDecl.
       *)

    val mutable normalize_newline = true
      (* Whether this entity converts CRLF or CR to LF, or not *)


    val mutable line = 1     (* current line *)
    val mutable column = 0   (* current column *)
    val mutable pos = 0      (* current absolute character position *)
    val errors_with_line_numbers = init_errors_with_line_numbers

    val mutable p_line = 1
    val mutable p_column = 1

    method line = p_line
    method column = p_column


    val mutable counts_as_external = false

    method counts_as_external = counts_as_external
        (* Whether the entity counts as external (for the standalone check). *)

    method set_counts_as_external =
      counts_as_external <- true


    val mutable last_token = Bof
      (* XXX
       * These two variables are used to check that between certain pairs of
       * tokens whitespaces exist. 'last_token' is simply the last token,
       * but not Ignore, and not PERef (which both represent whitespace).
       * 'space_seen' records whether Ignore or PERef was seen between this
       * token and 'last_token'.
       *)

    val mutable deferred_token = None
      (* If you set this to Some tl, the next invocations of 
       * next_token_from_entity will return the tokens in tl.
       * This makes it possible to insert tokens into the stream.
       *)

    val mutable debug = false

    method is_ndata = false
      (* Returns if this entity is an NDATA (unparsed) entity *)

    method name = name

    method virtual open_entity : bool -> lexers -> unit
	(* open_entity force_parsing lexid:
	 * opens the entity, and the first token is scanned by the lexer
	 * 'lexid'. 'force_parsing' forces that Begin_entity and End_entity
	 * tokens embrace the inner tokens of the entity; otherwise this
	 * depends on the entity.
	 * By opening an entity, reading tokens from it, and finally closing
	 * the entity, the inclusion methods "Included",
	 * "Included if validating", and "Included as PE" can be carried out.
	 * Which method is chosen depends on the 'lexid', i.e. the lexical
	 * context: 'lexid = Content' performs "Included (if validating)" (we
	 * are always validating); 'lexid = Declaration' performs
	 * "Included as PE". The difference is which tokens are recognized,
	 * and how spaces are handled.
	 * 'force_parsing' causes that a Begin_entity token is inserted before
	 * and an End_entity token is inserted after the entity. The yacc
	 * rules allow the Begin_entity ... End_entity brace only at certain
	 * positions; this is used to restrict the possible positions where
	 * entities may be included, and to guarantee that the entity matches
	 * a certain production of the grammar ("parsed entities").
	 * 'open_entity' is currently invoked with 'force_parsing = true'
	 * for toplevel nodes, for inclusion of internal general entities,
	 * and for inclusion of parameter entities into document entities.
	 * 'force_parsing = false' is used for all other cases: External
	 * entities add the Begin_entity/End_entity tokens anyway; internal
	 * entities do not. Especially internal parameter entities referenced
	 * from non-document entities do not add these tokens.
	 *)

    method virtual close_entity : lexers
	(* close_entity:
	 * closes the entity and returns the name of the lexer that must
	 * be used to scan the next token.
	 *)

    method virtual replacement_text : (string * bool)
	(* replacement_text:
	 * returns the replacement text of the entity, and as second value,
	 * whether the replacement text was constructed by referencing
	 * external entities (directly or indirectly).
	 * This method implements the inclusion method "Included in Literal".
	 *)


    method lexbuf = lexbuf


    method xml_declaration =
      (* return the (name,value) pairs of the initial <?xml name=value ...?>
       * processing instruction.
       *)
      match prolog with
	  None ->
	    None
	| Some p ->
	    Some prolog_pairs


    method set_debugging_mode m =
      debug <- m

    method private virtual set_encoding : string -> unit


    method full_name =
      name


    method next_token =
      (* read next token from this entity *)

      match deferred_token with
	  Some toklist ->
	    ( match toklist with
		  [] -> 
		    deferred_token <- None;
		    self # next_token
		| tok :: toklist' ->
		    deferred_token <- Some toklist';
		    if debug then
		      prerr_endline ("- Entity " ^ name ^ ": " ^ string_of_tok tok ^ " (deferred)");
		    tok
	    )
	| None -> begin
            let this_line = line
            and this_column = column in
	    let this_pos = pos in
	    p_line <- this_line;
	    p_column <- this_column;
	    (* Read the next token from the appropriate lexer lex_id, and get the
	     * name lex_id' of the next lexer to be used.
	     *)
	    let tok, lex_id' =
	      match lex_id with
		  Document         -> lexerset.scan_document lexbuf
		| Document_type    -> lexerset.scan_document_type lexbuf
		| Content          -> lexerset.scan_content lexbuf
		| Within_tag       -> lexerset.scan_within_tag lexbuf
		| Declaration      -> lexerset.scan_declaration lexbuf
		| Content_comment  -> lexerset.scan_content_comment lexbuf
		| Decl_comment     -> lexerset.scan_decl_comment lexbuf
		| Document_comment -> lexerset.scan_document_comment lexbuf
		| Ignored_section  -> assert false
		      (* Ignored_section: only used by method next_ignored_token *)
	    in
	    if debug then
	      prerr_endline ("- Entity " ^ name ^ ": " ^ string_of_tok tok);
	    (* Find out the number of lines and characters of the last line: *)
	    let n_lines, n_columns =
	      if errors_with_line_numbers then
		count_lines (Lexing.lexeme lexbuf)
	      else
		0, (Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf)
	    in
	    line <- this_line + n_lines;
	    column <- if n_lines = 0 then this_column + n_columns else n_columns;
	    pos <- Lexing.lexeme_end lexbuf;
	    lex_id <- lex_id';
	    (* Throw Ignore and Comment away; Interpret entity references: *)
	    (* NOTE: Of course, references to general entities are not allowed
	     * everywhere; parameter references, too. This is already done by the
	     * lexers, i.e. &name; and %name; are recognized only where they
	     * are allowed.
	     *)

	    (* TODO: last_token is only used to detect Bof. Can be simplified *)

	    let at_bof = (last_token = Bof) in
	    last_token <- tok;

	    let tok' =
	      match tok with

          (* Entity references: *)

		| ERef n    -> 
                    let en, extdecl = dtd # gen_entity n in
		    if dtd # standalone_declaration && extdecl then
		      raise
			(Validation_error
			   ("Reference to entity `" ^ n ^ 
			    "' violates standalone declaration"));
		    en # set_debugging_mode debug;
	            en # open_entity true lex_id;
		    self # manager # push_entity en;
		    en # next_token;
		| PERef n   -> 
		    let en = dtd # par_entity n in
		    en # set_debugging_mode debug;
	            en # open_entity force_parameter_entity_parsing lex_id;
		    self # manager # push_entity en;
		    en # next_token;

          (* Convert LineEnd to CharData *)
		| LineEnd s -> 
		    if normalize_newline then 
		      CharData "\n"
		    else
		      CharData s

          (* Also normalize CDATA sections *)
		| Cdata value as cd ->
		    if normalize_newline then 
		      Cdata(normalize_line_separators lexerset value)
		    else
		      cd

          (* If there are CRLF sequences in a PI value, normalize them, too *)
		| PI(name,value) as pi ->
		    if normalize_newline then
		      PI(name, normalize_line_separators lexerset value)
		    else
		      pi
         
          (* Attribute values: If they are already normalized, they are turned
	   * into Attval_nl_normalized. This is detected by other code.
	   *)
		| Attval value as av ->
		    if normalize_newline then
		      av
		    else
		      Attval_nl_normalized value

          (* Another CRLF normalization case: Unparsed_string *)
		| Unparsed_string value as ustr ->
		    if normalize_newline then
		      Unparsed_string(normalize_line_separators lexerset value)
		    else
		      ustr
		      
          (* These tokens require that the entity_id parameter is set: *)
		| Doctype _      -> Doctype       (self :> entity_id)
		| Doctype_rangle _ ->Doctype_rangle(self :> entity_id)
		| Dtd_begin _    -> Dtd_begin     (self :> entity_id)
		| Dtd_end _      -> Dtd_end       (self :> entity_id)
		| Decl_element _ -> Decl_element  (self :> entity_id)
		| Decl_attlist _ -> Decl_attlist  (self :> entity_id)
		| Decl_entity _  -> Decl_entity   (self :> entity_id)
		| Decl_notation _ ->Decl_notation (self :> entity_id)
		| Decl_rangle _  -> Decl_rangle   (self :> entity_id)
		| Lparen _       -> Lparen        (self :> entity_id)
		| Rparen _       -> Rparen        (self :> entity_id)
		| RparenPlus _   -> RparenPlus    (self :> entity_id)
		| RparenStar _   -> RparenStar    (self :> entity_id)
		| RparenQmark _  -> RparenQmark   (self :> entity_id)
		| Conditional_begin _ -> Conditional_begin (self :> entity_id)
		| Conditional_body _  -> Conditional_body  (self :> entity_id)
		| Conditional_end _   -> Conditional_end   (self :> entity_id)
		| Tag_beg (n,_)  -> Tag_beg (n, (self :> entity_id))
		| Tag_end (n,_)  -> Tag_end (n, (self :> entity_id))

          (* End of file: *)

		| Eof       -> 
		    if debug then begin
		      prerr_endline ("- Entity " ^ name ^ " # handle_eof");
		      let tok = self # handle_eof in
		      prerr_endline ("- Entity " ^ name ^ " # handle_eof: returns " ^ string_of_tok tok);
		      tok
		    end
		    else
		      self # handle_eof;
		    
          (* The default case. *)

		| _         -> 
                    tok

	    in
	    if at_bof & tok <> Eof
	    then begin
	      if debug then
		prerr_endline ("- Entity " ^ name ^ " # handle_bof");
	      self # handle_bof tok'
	    end
	    else
	      tok'
	  end


    (* 'handle_bof' and 'handle_eof' can be used as hooks. Behaviour:
     *
     * - Normally, the first token t is read in, and 'handle_bof t' is
     *   called. The return value of this method is what is returned to
     *   the user.
     * - If the EOF has been reached, 'handle_eof' is called. 
     * - BUT: If the first token is already EOF, 'handle_eof' is called
     *   ONLY, and 'handle_bof' is NOT called.
     *
     * The default implementations:
     * - handle_bof: does nothing
     * - handle_eof: Pops the previous entity from the stack, switches back
     *   to this entity, and returns the next token of this entity.
     *)


    method private handle_bof tok =
      tok


    method private handle_eof =
      let mng = self # manager in
      begin try
	mng # pop_entity;
	let next_lex_id = self # close_entity in
	let en = mng # current_entity in
	en # set_lex_id next_lex_id;
	en # next_token
      with
	  Stack.Empty ->
	    (* The outermost entity is at EOF *)
	    Eof
      end


    method next_ignored_token =
        (* used after <![ IGNORE *)

      (* TODO: Do we need a test on deferred tokens here? *)

        let this_line = line
        and this_column = column in
	let this_pos = pos in
	let tok, lex_id' = lexerset.scan_ignored_section lexbuf in
	if debug then
	  prerr_endline ("- Entity " ^ name ^ ": " ^ string_of_tok tok ^ " (Ignored)");
	let n_lines, n_columns = count_lines (Lexing.lexeme lexbuf) in
	line <- this_line + n_lines;
	column <- if n_lines = 0 then this_column + n_columns else n_columns;
	pos <- Lexing.lexeme_end lexbuf;
	match tok with
	  | Conditional_begin _ -> Conditional_begin (self :> entity_id)
	  | Conditional_end _   -> Conditional_end   (self :> entity_id)
	  | _                   -> tok


    method process_xmldecl pl =
      (* The parser calls this method just after the XML declaration
       * <?xml ...?> has been detected.
       * 'pl': This is the argument of the PI_xml token.
       *)
      if debug then
	prerr_endline ("- Entity " ^ name ^ " # process_xmldecl");
      prolog <- Some pl;
      prolog_pairs <- decode_xml_pi pl;
      if check_text_declaration then
	check_text_xml_pi prolog_pairs;
      begin
	try
	  let e = List.assoc "encoding" prolog_pairs in
	  self # set_encoding e
	with
	    Not_found ->
	      self # set_encoding ""
      end;


    method process_missing_xmldecl =
      (* The parser calls this method if the XML declaration is missing *)
      if debug then
	prerr_endline ("- Entity " ^ name ^ " # process_missing_xmldecl");
      self # set_encoding ""


    (* Methods for NDATA entities only: *)
    method ext_id = (assert false : ext_id)
    method notation = (assert false : string)

  end
;;


class ndata_entity the_name the_ext_id the_notation init_encoding =
  object (self)
    (* An NDATA entity is very restricted; more or less you can only find out
     * its external ID and its notation.
     *)

    val mutable name = the_name
    val mutable ext_id = the_ext_id
    val mutable notation = the_notation
    val encoding = (init_encoding : rep_encoding)

    method name = (name : string)
    method ext_id = (ext_id : ext_id)
    method notation = (notation : string)

    method is_ndata = true

    method encoding = encoding


    val mutable counts_as_external = false

    method counts_as_external = counts_as_external
        (* Whether the entity counts as external (for the standalone check). *)

    method set_counts_as_external =
      counts_as_external <- true


    method set_manager (m : < current_entity : entity; 
			      pop_entity : unit;
			      push_entity : entity -> unit >) = 
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method set_lex_id (id : lexers) =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method line =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : int )

    method column =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : int )

    method full_name =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : string )

    method private set_encoding (_:string) =
      assert false

    method xml_declaration = (None : (string*string) list option)

    method set_debugging_mode (_:bool) = ()

    method open_entity (_:bool) (_:lexers) =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method close_entity =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : lexers )

    method replacement_text =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : (string * bool) )

    method lexbuf =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : Lexing.lexbuf )

    method next_token =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : token )

    method next_ignored_token =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : token )

    method process_xmldecl (pl:prolog_token list) =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method process_missing_xmldecl =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

  end
;;


class external_entity the_resolver the_dtd the_name the_warner the_ext_id
                      the_p_special_empty_entities
		      init_errors_with_line_numbers
		      init_encoding
  =
  object (self)
    inherit entity
              the_dtd the_name the_warner init_errors_with_line_numbers
	      init_encoding
            as super

    (* An external entity gets the lexbuf that is used as character source
     * from a resolver.
     * Furthermore, before the first token an Begin_entity is inserted, and
     * before Eof an End_entity token is inserted into the stream. This done
     * always regardless of the argument 'force_parsing' of the method
     * 'open_entity'.
     *
     * 'the_p_internal_subset': see class internal_entity
     * 'the_p_special_empty_entities': if true, a Begin_entity/End_entity
     * brace is left out if the entity is otherwise empty.
     *)

    val resolver = (the_resolver : resolver)
    val ext_id = (the_ext_id : ext_id)

    val p_special_empty_entities = (the_p_special_empty_entities : bool)

    val mutable resolver_is_open = false
      (* Track if the resolver is open. This is also used to find recursive
       * references of entities.
       *)

    val mutable state = At_beginning

    initializer
      counts_as_external <- true;


    method private set_encoding e =
      assert resolver_is_open;
      resolver # change_encoding e


    method full_name =
      name ^
      match ext_id with
	  System s    -> " = SYSTEM \"" ^ s ^ "\""
	| Public(p,s) -> " = PUBLIC \"" ^ p ^ "\" \"" ^ s ^ "\""
	| Anonymous   -> " = ANONYMOUS"


    method open_entity force_parsing init_lex_id =
      (* Note that external entities are always parsed, i.e. Begin_entity
       * and End_entity tokens embrace the inner tokens to force that
       * the entity is only called where the syntax allows it.
       *)
      if resolver_is_open then
	raise(Validation_error("Recursive reference to entity `" ^ name ^ "'"));
      let lex = 
	try
	  resolver # open_in ext_id 
	with
	    Pxp_reader.Not_competent ->
	      raise(Error ("No input method available for this external entity: " ^ 
			self # full_name))
	  | Pxp_reader.Not_resolvable Not_found ->
	      raise(Error ("Unable to open the external entity: " ^ 
			   self # full_name))
	  | Pxp_reader.Not_resolvable e ->
	      raise(Error ("Unable to open the external entity: " ^ 
			   self # full_name ^ "; reason: " ^ 
			   string_of_exn e))
      in
      resolver_is_open <- true;
      lexbuf  <- lex;
      prolog  <- None;
      lex_id  <- init_lex_id;
      state <- At_beginning;
      line <- 1;
      column <- 0;
      pos <- 0;
      last_token <- Bof;
      normalize_newline <- true;


    method private handle_bof tok =
      (* This hook is only called if the stream is not empty. *)
      deferred_token <- Some [ tok ];
      state <- Inserted_begin_entity;
      Begin_entity


    method private handle_eof =
      (* This hook is called if the end of  the stream is reached *)
      match state with
	  At_beginning ->
	    (* This is only possible if the stream is empty. *)
	    if p_special_empty_entities then begin
	      (* Continue immediately with the next token *)
	      state <- At_end;
	      super # handle_eof
	    end
	    else begin
	      (* Insert Begin_entity / End_entity *)
	      deferred_token <- Some [ End_entity ];
	      state <- At_end;
	      Begin_entity;
	      (* After these two token have been processed, the lexer
	       * is called again, and it will return another Eof.
	       *)
	    end
	| Inserted_begin_entity ->
	    (* Insert End_entity, too. *)
	    state <- At_end;
	    End_entity;
	| At_end ->
	    (* Continue with the next token: *)
	    super # handle_eof


    method close_entity =
      if not resolver_is_open then
	failwith ("External entity " ^ name ^ " not open");
      resolver # close_in;
      resolver_is_open <- false;
      lex_id


    method replacement_text =
      (* Return the replacement text of the entity. The method used for this
       * is more or less the same as for internal entities; i.e. character
       * and parameter entities are resolved immediately. In addition to that,
       * external entities may begin with an "xml" processing instruction
       * which is considered not to be part of the replacement text.
       *)
      if resolver_is_open then
	raise(Validation_error("Recursive reference to entity `" ^ name ^ "'"));
      let lex = resolver # open_in ext_id in
      resolver_is_open <- true;
      lexbuf  <- lex;
      prolog  <- None;
      (* arbitrary:    lex_id  <- init_lex_id; *)
      state <- At_beginning;
      line <- 1;
      column <- 0;
      pos <- 0;
      last_token <- Bof;
      (* First check if the first token of 'lex' is <?xml...?> *)
      begin match lexerset.scan_only_xml_decl lex with
	  PI_xml pl ->
	    self # process_xmldecl pl
	| Eof ->
	    (* This only means that the first token was not <?xml...?>;
	     * the "Eof" token represents the empty string.
	     *)
	    self # process_missing_xmldecl
	| _ ->
	    (* Must not happen. *)
	    assert false
      end;
      (* Then create the replacement text. *)
      let rec scan_and_expand () =
	match lexerset.scan_dtd_string lexbuf with
	    ERef n -> "&" ^ n ^ ";" ^ scan_and_expand()
	  | CRef(-1) -> "\n" ^ scan_and_expand()
	  | CRef(-2) -> "\n" ^ scan_and_expand()
	  | CRef(-3) -> "\n" ^ scan_and_expand()
	  | CRef k -> character encoding warner k ^ scan_and_expand()
	  | CharData x -> x ^ scan_and_expand()
	  | PERef n ->
	      let en = dtd # par_entity n in
	      let (x,_) = en # replacement_text in
	      x ^ scan_and_expand()
	  | Eof ->
	      ""
	  | _ ->
	      assert false
      in
      let rtext = scan_and_expand() in
      resolver # close_in;
      resolver_is_open <- false;
      rtext, true
	(* TODO:
	 * - The replaced text is not parsed [VALIDATION WEAKNESS]
	 *)
  end
;;


class document_entity  the_resolver the_dtd the_name the_warner the_ext_id
                       init_errors_with_line_numbers
		       init_encoding
  =
  object (self)
    inherit external_entity  the_resolver the_dtd the_name the_warner
                             the_ext_id false init_errors_with_line_numbers
			     init_encoding

    (* A document entity is an external entity that does not allow
     * conditional sections, and that forces that internal parameter entities
     * are properly nested.
     *)

    initializer
    force_parameter_entity_parsing <- true;
    check_text_declaration <- false;

    method counts_as_external = false
      (* Document entities count never as external! *)
  end
;;


class internal_entity the_dtd the_name the_warner the_literal_value
                      the_p_internal_subset init_errors_with_line_numbers
                      init_is_parameter_entity
		      init_encoding
  =
  (* An internal entity uses a "literal entity value" as character source.
   * This value is first expanded and preprocessed, i.e. character and
   * parameter references are expanded.
   *
   * 'the_p_internal_subset': indicates that the entity is declared in the
   * internal subset. Such entity declarations are not allowed to contain
   * references to parameter entities.
   * 'init_is_parameter_entity': whether this is a parameter entity or not
   *)

  object (self)
    inherit entity
              the_dtd the_name the_warner init_errors_with_line_numbers
	      init_encoding
	    as super

    val p_internal_subset = the_p_internal_subset

    val mutable replacement_text = ""
    val mutable contains_external_references = false
    val mutable p_parsed_actually = false
    val mutable is_open = false
    val mutable state = At_beginning
    val mutable is_parameter_entity = init_is_parameter_entity


    initializer
    let lexbuf = Lexing.from_string the_literal_value in
    let rec scan_and_expand () =
      match lexerset.scan_dtd_string lexbuf with
	  ERef n -> "&" ^ n ^ ";" ^ scan_and_expand()
	| CRef(-1) -> "\r\n" ^ scan_and_expand()
	| CRef(-2) -> "\r" ^ scan_and_expand()
	| CRef(-3) -> "\n" ^ scan_and_expand()
	| CRef k -> character encoding warner k ^ scan_and_expand()
	| CharData x -> x ^ scan_and_expand()
	| PERef n ->
	    if p_internal_subset then
	      raise(WF_error("Restriction of the internal subset: parameter entity not allowed here"));
	    let en = dtd # par_entity n in
	    let (x, extref) = en # replacement_text in
	    contains_external_references <-
	      contains_external_references or extref;
	    x ^ scan_and_expand()
	| Eof ->
	    ""
	| _ ->
	    assert false
    in
    is_open <- true;
    replacement_text <- scan_and_expand();
    is_open <- false;
    normalize_newline <- false;
    counts_as_external <- false;


    method process_xmldecl (pl:prolog_token list) =
      raise(Validation_error("The encoding cannot be changed in internal entities"))


    method process_missing_xmldecl =
      ()


    method private set_encoding e =
      (* Ignored if e = "" *)
      assert(e = "");


    method open_entity force_parsing init_lex_id =
      if is_open then
	raise(Validation_error("Recursive reference to entity `" ^ name ^ "'"));

      p_parsed_actually <- force_parsing;
      lexbuf  <- Lexing.from_string 
	           (if is_parameter_entity then
		      (" " ^ replacement_text ^ " ")
		    else
		      replacement_text);
      prolog  <- None;
      lex_id  <- init_lex_id;
      state <- At_beginning;
      is_open <- true;
      line <- 1;
      column <- 0;
      pos <- 0;
      last_token <- Eof;


    method private handle_bof tok =
      (* This hook is only called if the stream is not empty. *)
      if p_parsed_actually then begin
	deferred_token <- Some [ tok ];
	state <- Inserted_begin_entity;
	Begin_entity
      end
      else begin
	state <- At_end;
	tok
      end


    method private handle_eof =
      (* This hook is called if the end of  the stream is reached *)
      match state with
	  At_beginning ->
	    (* This is only possible if the stream is empty. *)
	    if p_parsed_actually then begin
	      (* Insert Begin_entity / End_entity *)
	      deferred_token <- Some [ End_entity ];
	      state <- At_end;
	      Begin_entity;
	      (* After these two token have been processed, the lexer
	       * is called again, and it will return another Eof.
	       *)
	    end
	    else begin
	      (* Continue immediately with the next token *)
	      state <- At_end;
	      super # handle_eof
	    end
	| Inserted_begin_entity ->
	    (* Insert End_entity, too. *)
	    state <- At_end;
	    End_entity;
	| At_end ->
	    (* Continue with the next token: *)
	    super # handle_eof


    method close_entity =
      if not is_open then
	failwith ("Internal entity " ^ name ^ " not open");
      is_open <- false;
      lex_id


    method replacement_text =
      if is_open then
	raise(Validation_error("Recursive reference to entity `" ^ name ^ "'"));
      replacement_text, contains_external_references
  end
;;

(**********************************************************************)

(* An 'entity_manager' is a stack of entities, where the topmost entity
 * is the currently active entity, the second entity is the entity that
 * referred to the active entity, and so on.
 *
 * The entity_manager can communicate with the currently active entity.
 *
 * The entity_manager provides an interface for the parser; the functions
 * returning the current token and the next token are exported.
 *)

class entity_manager (init_entity : entity) =
  object (self)
    val mutable entity_stack = Stack.create()
    val mutable current_entity = init_entity
    val mutable current_entity's_full_name = lazy (init_entity # full_name)
				   
    val mutable yy_get_next_ref = ref (fun () -> assert false)

    initializer
      init_entity # set_manager (self :> 
				 < current_entity : entity; 
				   pop_entity : unit;
				   push_entity : entity -> unit >
				);
      yy_get_next_ref := (fun () -> init_entity # next_token)

    method push_entity e =
      e # set_manager (self :> 
		       < current_entity : entity; 
		         pop_entity : unit;
			 push_entity : entity -> unit >
		      );
      Stack.push (current_entity, current_entity's_full_name) entity_stack;
      current_entity <- e;
      current_entity's_full_name <- lazy (e # full_name);
      yy_get_next_ref := (fun () -> e # next_token);

    method pop_entity =
      (* May raise Stack.Empty *)
      let e, e_name = Stack.pop entity_stack in
      current_entity <- e;
      current_entity's_full_name <- e_name;
      yy_get_next_ref := (fun () -> e # next_token);



    method position_string =
      (* Gets a string describing the position of the last token;
       * includes an entity backtrace
       *)
      let b = Buffer.create 200 in
      Buffer.add_string b
	("In entity " ^ current_entity # full_name
	 ^ ", at line " ^ string_of_int (current_entity # line)
	 ^ ", column " ^ string_of_int (current_entity # column)
	 ^ ": ");
      Stack.iter
	(fun (e, e_name) ->
	   Buffer.add_string b 
	     ("\nCalled from entity " ^ Lazy.force e_name
	      ^ ", line " ^ string_of_int (e # line)
	      ^  ", column " ^ string_of_int (e # column)
	      ^ ":");
	)
	entity_stack;
      Buffer.contents b


    method position =
      (* Returns the triple (full_name, line, column) of the last token *)
      Lazy.force current_entity's_full_name, 
      current_entity # line,
      current_entity # column


    method current_entity_counts_as_external =
      (* Whether the current entity counts as external to the main
       * document for the purpose of stand-alone checks.
       *)
      (* TODO: improve performance *)
      let is_external = ref false in
      let check (e, _) =
	if e # counts_as_external then begin
	  is_external := true;
	end;
      in
      check (current_entity,());
      Stack.iter check entity_stack;
      !is_external


    method current_entity  = current_entity

    method yy_get_next_ref = yy_get_next_ref

  end
;;

      

(* ======================================================================
 * History:
 *
 * $Log: pxp_entity.ml,v $
 * Revision 1.5  2000/07/09 17:51:50  gerd
 * 	Entities return now the beginning of a token as its
 * position.
 * 	New method 'position' for entity_manager.
 *
 * Revision 1.4  2000/07/09 01:05:04  gerd
 * 	Exported methods 'ext_id' and 'notation' anyway.
 *
 * Revision 1.3  2000/07/08 16:28:05  gerd
 * 	Updated: Exception 'Not_resolvable' is taken into account.
 *
 * Revision 1.2  2000/07/04 22:12:47  gerd
 * 	Update: Case ext_id = Anonymous.
 * 	Update: Handling of the exception Not_competent when reading
 * from a resolver.
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
 * Old logs from markup_entity.ml:
 *
 * Revision 1.27  2000/05/29 21:14:57  gerd
 * 	Changed the type 'encoding' into a polymorphic variant.
 *
 * Revision 1.26  2000/05/28 17:24:55  gerd
 * 	Bugfixes.
 *
 * Revision 1.25  2000/05/27 19:23:32  gerd
 * 	The entities store whether they count as external with
 * respect to the standalone check: New methods counts_as_external
 * and set_counts_as_external.
 * 	The entity manager can find out whether the current
 * entity counts as external: method current_entity_counts_as_external.
 *
 * Revision 1.24  2000/05/20 20:31:40  gerd
 * 	Big change: Added support for various encodings of the
 * internal representation.
 *
 * Revision 1.23  2000/05/14 21:51:24  gerd
 * 	Change: Whitespace is handled by the grammar, and no longer
 * by the entity.
 *
 * Revision 1.22  2000/05/14 17:50:54  gerd
 * 	Updates because of changes in the token type.
 *
 * Revision 1.21  2000/05/09 00:02:44  gerd
 * 	Conditional sections are now recognized by the parser.
 * There seem some open questions; see the TODO comments!
 *
 * Revision 1.20  2000/05/08 21:58:22  gerd
 * 	Introduced entity_manager as communication object between
 * the parser and the currently active entity.
 * 	New hooks handle_bof and handle_eof.
 * 	Removed "delegated entities". The entity manager contains
 * the stack of open entities.
 * 	Changed the way Begin_entity and End_entity are inserted.
 * This is now done by handle_bof and handle_eof.
 * 	The XML declaration is no longer detected by the entity.
 * This is now done by the parser.
 *
 * Revision 1.19  2000/05/01 15:18:44  gerd
 * 	Improved CRLF handling in the replacement text of entities.
 * 	Changed one error message.
 *
 * Revision 1.18  2000/04/30 18:18:39  gerd
 * 	Bugfixes: The conversion of CR and CRLF to LF is now hopefully
 * done right. The new variable "normalize_newline" indicates whether
 * normalization must happen for that type of entity. The normalization
 * if actually carried out separately for every token that needs it.
 *
 * Revision 1.17  2000/03/13 23:42:38  gerd
 * 	Removed the resolver classes, and put them into their
 * own module (Markup_reader).
 *
 * Revision 1.16  2000/02/22 01:06:58  gerd
 * 	Bugfix: Resolvers are properly re-initialized. This bug caused
 * that entities could not be referenced twice in the same document.
 *
 * Revision 1.15  2000/01/20 20:54:11  gerd
 * 	New config.errors_with_line_numbers.
 *
 * Revision 1.14  2000/01/08 18:59:03  gerd
 * 	Corrected the string resolver.
 *
 * Revision 1.13  1999/09/01 22:58:23  gerd
 * 	Method warn_not_latin1 raises Illegal_character if the character
 * does not match the Char production.
 * 	External entities that are not document entities check if the
 * <?xml...?> declaration at the beginning matches the TextDecl production.
 * 	Method xml_declaration has type ... list option, not ... list.
 * 	Tag_beg and Tag_end now carry an entity_id with them.
 * 	The code to check empty entities has changed. That the Begin_entity/
 * End_entity pair is not to be added must be explicitly turned on. See the
 * description of empty entity handling in design.txt.
 * 	In internal subsets entity declarations are not allowed to refer
 * to parameter entities. The internal_entity class can do this now.
 * 	The p_parsed parameter of internal_entity has gone. It was simply
 * superflous.
 *
 * Revision 1.12  1999/09/01 16:24:13  gerd
 * 	The method replacement_text returns the text as described for
 * "included in literal". The former behaviour has been dropped to include
 * a leading and a trailing space character for parameter entities.
 * 	Bugfix: When general entities are included, they are always parsed.
 *
 * Revision 1.11  1999/08/31 19:13:31  gerd
 * 	Added checks on proper PE nesting. The idea is that tokens such
 * as Decl_element and Decl_rangle carry an entity ID with them. This ID
 * is simply an object of type < >, i.e. you can only test on identity.
 * The lexer always produces tokens with a dummy ID because it does not
 * know which entity is the current one. The entity layer replaces the dummy
 * ID with the actual ID. The parser checks that the IDs of pairs such as
 * Decl_element and Decl_rangle are the same; otherwise a Validation_error
 * is produced.
 *
 * Revision 1.10  1999/08/19 01:06:41  gerd
 * 	Improved error messages: external entities print their
 * ext id, too
 *
 * Revision 1.9  1999/08/15 20:35:48  gerd
 * 	Improved error messages.
 * 	Before the tokens Plus, Star, Qmark space is not allowed any longer.
 * 	Detection of recursive entity references is a bit cleaner.
 *
 * Revision 1.8  1999/08/15 15:33:44  gerd
 * 	Revised whitespace checking: At certain positions there must be
 * white space. These checks cannot be part of the lexer, as %entity; counts
 * as white space. They cannot be part of the yacc parser because one look-ahead
 * token would not suffice if we did that. So these checks must be done by the
 * entity layer. Luckily, the rules are simple: There are simply a number of
 * token pairs between which white space must occur independently of where
 * these token have been found. Two variables, "space_seen", and "last_token"
 * have been added in order to check these rules.
 *
 * Revision 1.7  1999/08/15 00:41:06  gerd
 * 	The [ token of conditional sections is now allowed to occur
 * in a different entity.
 *
 * Revision 1.6  1999/08/15 00:29:02  gerd
 * 	The method "attlist_replacement_text" has gone. There is now a
 * more general "replacement_text" method that computes the replacement
 * text for both internal and external entities. Additionally, this method
 * returns whether references to external entities have been resolved;
 * this is checked in the cases where formerly "attlist_replacement_text"
 * was used as it is not allowed everywhere.
 * 	Entities have a new slot "need_spaces" that indicates that the
 * next token must be white space or a parameter reference. The problem
 * was that "<!ATTLIST%e;" is legal because when including parameter
 * entities white space is added implicitly. Formerly, the white space
 * was expected by the underlying lexer; now the lexer does not check
 * anymore that "<!ATTLIST" is followed by white space because the lexer
 * cannot handle parameter references. Because of this, the check on
 * white space must be done by the entity.
 *
 * Revision 1.5  1999/08/14 22:57:19  gerd
 * 	It is allowed that external entities are empty because the
 * empty string is well-parsed for both declarations and contents. Empty
 * entities can be referenced anywhere because the references are replaced
 * by nothing. Because of this, the Begin_entity...End_entity brace is only
 * inserted if the entity is non-empty. (Otherwise references to empty
 * entities would not be allowed anywhere.)
 * 	As a consequence, the grammar has been changed such that a
 * single Eof is equivalent to Begin_entity,End_entity without content.
 *
 * Revision 1.4  1999/08/14 22:11:19  gerd
 *         Several objects have now a "warner" as argument which is
 * an object with a "warn" method. This is used to warn about characters
 * that cannot be represented in the Latin 1 alphabet.
 * 	Previously, the resolvers had features in order to warn about
 * such characters; this has been removed.
 * 	UTF-8 streams can be read even if they contain characters
 * that cannot be represented by 16 bits.
 * 	The buffering used in the resolvers is now solved in a
 * cleaner way; the number of characters that are expected to be read
 * from a source can be limited. This removes a bug with UTF-16 streams
 * that previously lead to wrong exceptions; and the buffering is more
 * efficient, too.
 *
 * Revision 1.3  1999/08/11 14:58:53  gerd
 * 	Some more names for encodings are allowed, such as "utf8" instead
 * of the standard name "UTF-8".
 * 	'resolve_as_file' interprets relative file names as relative to
 * the "parent" resolver.
 *
 * Revision 1.2  1999/08/10 21:35:07  gerd
 * 	The XML/encoding declaration at the beginning of entities is
 * evaluated. In particular, entities have now a method "xml_declaration"
 * which returns the name/value pairs of such a declaration. The "encoding"
 * setting is interpreted by the entity itself; "version", and "standalone"
 * are interpreted by Markup_yacc.parse_document_entity. Other settings
 * are ignored (this does not conform to the standard; the standard prescribes
 * that "version" MUST be given in the declaration of document; "standalone"
 * and "encoding" CAN be declared; no other settings are allowed).
 * 	TODO: The user should be warned if the standard is not exactly
 * fulfilled. -- The "standalone" property is not checked yet.
 *
 * Revision 1.1  1999/08/10 00:35:51  gerd
 * 	Initial revision.
 *
 *
 *)
