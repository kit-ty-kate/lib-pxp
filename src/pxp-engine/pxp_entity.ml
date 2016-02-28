(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


open Pxp_core_types.I
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

class type ['entity] preliminary_dtd =
object
  method standalone_declaration : bool
  method gen_entity : string -> ('entity * bool)
  method par_entity : string -> 'entity
end
;;


(* Instead of many instance variables we have many record components of
 * one instance variable. Speeds the entity methods up.
 *)

type 'entity entity_variables = 
    { mutable dtd : 'entity preliminary_dtd;
      mutable name : string;
      mutable swarner : symbolic_warnings option;
      mutable warner : collect_warnings;
      
      mutable encoding : rep_encoding;
      mutable lfactory : lexer_factory;

      mutable l_scan_document : unit -> (token * lexers);
      mutable l_scan_document_type : unit -> (token * lexers);
      mutable l_scan_content : unit -> (token * lexers);
      mutable l_scan_within_tag : unit -> (token * lexers);
      mutable l_scan_declaration : unit -> (token * lexers);
      mutable l_scan_comment : unit -> lexers -> (token * lexers);
      mutable l_scan_tag_eb : unit -> (token * lexers);
      mutable l_scan_tag_eb_att : unit -> bool -> (token * lexers);
      mutable lexobj : lexer_obj;
        (* The lexical buffer currently used as character source. *)

      (* Caution: whenever lexobj is changed, the l_* functions must be
       * updated, too (update_lexobj)
       *)

      mutable prolog : prolog_token list option;
        (* Stores the initial <?xml ...?> token as PI_xml *)
      mutable prolog_pairs : (string * string) list;
        (* If prolog <> None, these are the (name,value) pairs of the
	 * processing instruction.
	 *)

      mutable lex_id : lexers;
        (* The name of the lexer that should be used for the next token *)

      mutable force_parameter_entity_parsing : bool;
        (* 'true' forces that inner entities will always be embraced by
	 *        Begin_entity and End_entity.
	 * 'false': the inner entity itself decides this
	 *)

      mutable check_text_declaration : bool;
        (* 'true': It is checked that the <?xml..?> declaration matches the
         *         production TextDecl.
         *)

      mutable normalize_newline : bool;
        (* Whether this entity converts CRLF or CR to LF, or not *)

      mutable generate_attribute_events : bool;
        (* Whether attribute events are generated or not *)

      mutable line : int;          (* current line *)
      mutable column : int;        (* current column *)

      mutable p_line : int;        (* previous line *)
      mutable p_column : int;      (* previous column *)

      mutable linecount : linecount;   (* aux component for line counting *)

      mutable counts_as_external : bool;
        (* Whether the entity counts as external (for the standalone check). *)

      mutable at_bof : bool;

      mutable deferred_token : token list option;
        (* If you set this to Some tl, the next invocations of 
	 * next_token_from_entity will return the tokens in tl.
	 * This makes it possible to insert tokens into the stream.
	 *)

      mutable debug : bool;
    }
;;


let make_variables the_dtd the_name the_swarner the_warner init_encoding =
  let lf = Pxp_lexers.get_lexer_factory init_encoding in
  let lobj = lf # open_string "" in
  { dtd = (the_dtd : 'entity #preliminary_dtd :> 'entity preliminary_dtd);
    name = the_name;
    warner = the_warner;
    swarner = the_swarner;
    
    encoding = init_encoding;
    lfactory = lf;

    l_scan_document         = lobj#scan_document;
    l_scan_document_type    = lobj#scan_document_type;
    l_scan_content          = lobj#scan_content;
    l_scan_within_tag       = lobj#scan_within_tag;
    l_scan_declaration      = lobj#scan_declaration;
    l_scan_comment          = lobj#scan_comment;
    l_scan_tag_eb           = lobj#scan_tag_eb;
    l_scan_tag_eb_att       = lobj#scan_tag_eb_att;
    
    lexobj = lobj;
    
    prolog = None;
    prolog_pairs = [];
    
    lex_id = Document;
    
    force_parameter_entity_parsing = false;
    check_text_declaration = true;
    
    normalize_newline = true;
    generate_attribute_events = false;

    line = 1;
    column = 0;

    p_line = 1;
    p_column = 1;

    linecount = { lines = 0; columns = 0 };
    
    counts_as_external = false;
    
    at_bof = true;
    deferred_token = None;
    
    debug = false;
  }
;;

let update_lexobj v lobj =
  v.lexobj <- lobj;
  v.l_scan_document         <- lobj#scan_document;
  v.l_scan_document_type    <- lobj#scan_document_type;
  v.l_scan_content          <- lobj#scan_content;
  v.l_scan_within_tag       <- lobj#scan_within_tag;
  v.l_scan_declaration      <- lobj#scan_declaration;
  v.l_scan_comment          <- lobj#scan_comment;
  v.l_scan_tag_eb           <- lobj#scan_tag_eb;
  v.l_scan_tag_eb_att       <- lobj#scan_tag_eb_att;
;;


let update_lines v =
  let n_lines = v.linecount.lines in
  let n_columns = v.linecount.columns in
  v.line <- v.line + n_lines;
  v.column <- if n_lines = 0 then v.column + n_columns else n_columns
;;

let update_content_lines v tok =
  match tok with
      LineEnd _ -> 
	v.line <- v.line + 1;
	v.column <- 0;
    | (PI(_,_,_)|PI_xml _|Cdata _) ->
	count_lines v.linecount v.lexobj#lexeme;
	update_lines v;
    | _ -> 
	v.column <- v.column + v.lexobj#lexeme_strlen
;;

let update_lines_within_tag v tok =
  match tok with
      Attval av -> 
	(* count av + delimiting quotes *)
	count_lines v.linecount av;
	if v.linecount.lines = 0 then 
	  v.column <- v.column + v.linecount.columns + 2
	else begin
	  update_lines v;
	  v.column <- v.column + 1;
	end
    | IgnoreLineEnd ->
	v.line <- v.line + 1;
	v.column <- 0;
    | _ -> 
	v.column <- v.column + v.lexobj#lexeme_strlen
;;

let update_other_lines v tok =
  count_lines v.linecount v.lexobj#lexeme;
  update_lines v;
;;


class type entity =
object
  method pxp_magic_coercion : unit -> unit
  method is_ndata : bool
  method name : string
  method lex_id : lexers
  method set_lex_id : lexers -> unit
  method line : int
  method column : int
  method set_line_column : int -> int -> unit
  method encoding : rep_encoding
  method set_manager : prelim_entity_manager -> unit
  method counts_as_external : bool
  method set_counts_as_external : unit
  method lexer_obj : lexer_obj
  method resolver : resolver option
  method open_entity : ?gen_att_events:bool -> bool -> lexers -> unit
  method close_entity : lexers
  method is_open : bool
  method replacement_text : (string * bool)
  method xml_declaration : (string * string) list option
  method set_debugging_mode : bool -> unit
  method full_name : string
  method next_token : token
  method next_ignored_token : token
  method process_xmldecl : prolog_token list -> unit
  method process_missing_xmldecl : unit
  method ext_id : ext_id
  method resolver_id : resolver_id
  method notation : string
end

and prelim_entity_manager =
object
  method current_entity : entity
  method pop_entity : unit -> unit
  method push_entity : entity -> unit
end


class type v_entity =
object
  inherit entity

  val v : entity entity_variables
end


exception Coerced_entity of entity


class virtual entity_base the_dtd the_name the_swarner the_warner init_encoding =
  object (self)
    (* This class prescribes the type of all entity objects. Furthermore,
     * the default 'next_token' mechanism is implemented.
     *)

    val v = make_variables 
	      the_dtd the_name the_swarner the_warner init_encoding

    method virtual pxp_magic_coercion : unit -> unit

    method is_ndata = false
      (* Returns if this entity is an NDATA (unparsed) entity *)

    method name = v.name

    method lex_id = v.lex_id

    method set_lex_id id = v.lex_id <- id

    method line = v.p_line
    method column = v.p_column

    method set_line_column l c =
      v.line <- l;
      v.column <- c

    method encoding = v.encoding
    (* method lexerset = lexerset *)

    val mutable manager = None
      (* The current entity_manager, see below *)

    method private manager = 
      ( match manager with
	    None -> assert false
	  | Some m -> m
      : prelim_entity_manager
      )

    method set_manager m = manager <- Some m

    method counts_as_external = v.counts_as_external

    method set_counts_as_external =
      v.counts_as_external <- true

    method lexer_obj = v.lexobj

    method virtual resolver : resolver option

    method virtual open_entity : 
        ?gen_att_events:bool -> bool -> lexers -> unit
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

    method close_entity =
	(* close_entity:
	 * closes the entity and returns the name of the lexer that must
	 * be used to scan the next token.
	 *)
      let current_lex_id = v.lex_id in
      v.deferred_token <- None;
      v.lex_id <- Closed;
      current_lex_id

    method virtual is_open : bool
      (* Whether the entity is currently open *)


    method virtual replacement_text : (string * bool)
	(* replacement_text:
	 * returns the replacement text of the entity, and as second value,
	 * whether the replacement text was constructed by referencing
	 * external entities (directly or indirectly).
	 * This method implements the inclusion method "Included in Literal".
	 *)


    method xml_declaration =
      (* return the (name,value) pairs of the initial <?xml name=value ...?>
       * processing instruction.
       *)
      match v.prolog with
	  None ->
	    None
	| Some p ->
	    Some v.prolog_pairs


    method set_debugging_mode m =
      v.debug <- m

    method private virtual set_encoding : string -> unit


    method full_name =
      v.name

    method next_token =
      (* read next token from this entity *)

      let v = v in   (* Lookup the instance variable only once *)
      let debug = v.debug in

      match v.deferred_token with
	  Some toklist ->
	    ( match toklist with
		  [] -> 
		    v.deferred_token <- None;
		    self # next_token
		| tok :: toklist' ->
		    v.deferred_token <- Some toklist';
		    if debug then
		      prerr_endline ("- Entity " ^ v.name ^ ": " ^ string_of_tok tok ^ " (deferred)");
		    tok
	    )
	| None -> begin
	    v.p_line <- v.line;
	    v.p_column <- v.column;
	    (* Read the next token from the appropriate lexer lex_id, and get the
	     * name lex_id' of the next lexer to be used.
	     *)
	    let update_fn = ref update_content_lines in
	    let tok, lex_id' =
	      match v.lex_id with
		  Document         -> update_fn := update_other_lines;
                                      v.l_scan_document () 
		| Document_type    -> update_fn := update_other_lines;
                                      v.l_scan_document_type ()
		| Content          -> v.l_scan_content ()
		| Within_tag       -> update_fn := update_lines_within_tag;
                                      v.l_scan_within_tag ()
		| Within_tag_entry -> if v.generate_attribute_events then (
		                        (* like Tag_eb: *)
		                        update_fn := update_lines_within_tag;
                                        v.l_scan_tag_eb ()
                                      ) else (
		                        (* like Within_tag: *)
		                        update_fn := update_lines_within_tag;
                                        v.l_scan_within_tag ()
		                      )
		| Declaration      -> update_fn := update_other_lines;
                                      v.l_scan_declaration ()
		| Comment flw_id   -> update_fn := update_other_lines;
		                      v.l_scan_comment () flw_id
		| Tag_eb           -> update_fn := update_lines_within_tag;
                                      v.l_scan_tag_eb ()
		| Tag_eb_att b     -> (* keep update_content_lines! *)
                                      v.l_scan_tag_eb_att () b
		| Ignored_section  -> assert false
  	                           (* only used by method next_ignored_token *)
		| Closed           -> (Eof, Closed)
	    in

	    if debug then (
	      prerr_endline ("- Entity " ^ v.name ^ ": " ^ string_of_tok tok);
	      prerr_endline ("         Transition: " ^ 
			     string_of_lexers v.lex_id ^ " -> " ^ 
			     string_of_lexers lex_id')
	    );
	    
	    (* Find out the number of lines and characters of the last line: *)
	    !update_fn v tok;
	    v.lex_id <- lex_id';

	    (* Throw Ignore and Comment away; Interpret entity references: *)
	    (* NOTE: Of course, references to general entities are not allowed
	     * everywhere; parameter references, too. This is already done by the
	     * lexers, i.e. &name; and %name; are recognized only where they
	     * are allowed.
	     *)

	    let tok' =
	      match tok with

          (* Entity references: *)

		| ERef n    -> 
                    let en, extdecl = v.dtd # gen_entity n in
		    if v.dtd # standalone_declaration && extdecl then
		      raise
			(Validation_error
			   ("Reference to entity `" ^ n ^ 
			    "' violates standalone declaration"));
		    en # set_debugging_mode debug;
	            en # open_entity 
                      ?gen_att_events:(Some v.generate_attribute_events) 
                      true v.lex_id;
		    self # manager # push_entity en;
		    en # next_token;
		| PERef n   -> 
		    let en = v.dtd # par_entity n in
		    en # set_debugging_mode debug;
	            en # open_entity 
		      ?gen_att_events:(Some v.generate_attribute_events)
                      v.force_parameter_entity_parsing v.lex_id;
		    self # manager # push_entity en;
		    en # next_token;

          (* Convert LineEnd to CharData *)
		| LineEnd s -> 
		    if v.normalize_newline then 
		      CharData "\n"
		    else
		      CharData s

          (* Convert LineEnd_att to CharData *)
		| LineEnd_att s -> 
		    if v.normalize_newline then 
		      CharData " "
		    else
		      CharData s

          (* Also normalize CDATA sections *)
		| Cdata value as cd ->
		    if v.normalize_newline then 
		      Cdata(normalize_line_separators v.lfactory value)
		    else
		      cd

          (* If there are CRLF sequences in a PI value, normalize them, too *)
		| PI(name,value,_) ->
		    if v.normalize_newline then
		      PI(name, 
			 normalize_line_separators v.lfactory value,
			 (self :> entity_id))
		    else
		      PI(name, value, (self :> entity_id))
         
          (* Attribute values: If they are already normalized, they are turned
	   * into Attval_nl_normalized. This is detected by other code.
	   *)
		| Attval value as av ->
		    if v.normalize_newline then
		      av
		    else
		      Attval_nl_normalized value

          (* Another CRLF normalization case: Unparsed_string *)
		| Unparsed_string value as ustr ->
		    if v.normalize_newline then
		      Unparsed_string(normalize_line_separators v.lfactory value)
		    else
		      ustr

          (* Turn IgnoreLineEnd into Ignore *)
		| IgnoreLineEnd -> Ignore
		      
          (* These tokens require that the entity_id parameter is set: *)
		| Comment_begin _ -> Comment_begin(self :> entity_id)
		| Comment_end _   -> Comment_end  (self :> entity_id)
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
		      prerr_endline ("- Entity " ^ v.name ^ " # handle_eof");
		      let tok = self # handle_eof in
		      prerr_endline ("- Entity " ^ v.name ^ " # handle_eof: returns " ^ string_of_tok tok);
		      tok
		    end
		    else
		      self # handle_eof;
		    
          (* The default case. *)

		| _         -> 
                    tok

	    in
	    if v.at_bof then begin
	      v.at_bof <- false;
	      if tok <> Eof then begin
		if debug then
		  prerr_endline ("- Entity " ^ v.name ^ " # handle_bof");
		self # handle_bof tok'
	      end
	      else tok'
	    end
	    else tok'
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
	mng # pop_entity();
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

        if v.lex_id = Closed then
	  Eof
	else
	  let tok, lex_id' = v.lexobj#scan_ignored_section() in
	  if v.debug then
	    prerr_endline ("- Entity " ^ v.name ^ ": " ^ string_of_tok tok ^ " (Ignored)");
	  update_other_lines v tok;
	  match tok with
	    | Conditional_begin _ -> Conditional_begin (self :> entity_id)
	    | Conditional_end _   -> Conditional_end   (self :> entity_id)
	    | _                   -> tok


    method process_xmldecl pl =
      (* The parser calls this method just after the XML declaration
       * <?xml ...?> has been detected.
       * 'pl': This is the argument of the PI_xml token.
       *)
      if v.debug then
	prerr_endline ("- Entity " ^ v.name ^ " # process_xmldecl");
      v.prolog <- Some pl;
      v.prolog_pairs <- decode_xml_pi pl;
      if v.check_text_declaration then
	check_text_xml_pi v.prolog_pairs;
      begin
	try
	  let e = List.assoc "encoding" v.prolog_pairs in
	  self # set_encoding e
	with
	    Not_found ->
	      self # set_encoding ""
      end;


    method process_missing_xmldecl =
      (* The parser calls this method if the XML declaration is missing *)
      if v.debug then
	prerr_endline ("- Entity " ^ v.name ^ " # process_missing_xmldecl");
      self # set_encoding ""


    method ext_id = 
      (* Returns the external ID for external and NDATA entities. Raises
       * Not_found for internal entities
       *)
      (raise Not_found : ext_id)


    method resolver_id =
      (* Returns the resolver ID for external entities. Raises Not_found
       * for other types of entities.
       *)
      (raise Not_found : resolver_id)


    (* Methods for NDATA entities only: *)
    method notation = (assert false : string)

  end
;;


class ndata_entity the_name the_ext_id the_notation init_encoding : entity =
  object (self)
    (* An NDATA entity is very restricted; more or less you can only find out
     * its external ID and its notation.
     *)

    val mutable name = the_name
    val mutable ext_id = the_ext_id
    val mutable notation = the_notation
    val encoding = (init_encoding : rep_encoding)

    method pxp_magic_coercion() =
      raise (Coerced_entity (self :> entity))

    method name = (name : string)
    method ext_id = (ext_id : ext_id)
    method notation = (notation : string)
    method resolver_id = (raise Not_found : resolver_id)

    method is_ndata = true

    method encoding = encoding


    val mutable counts_as_external = false

    method counts_as_external = counts_as_external
        (* Whether the entity counts as external (for the standalone check). *)

    method set_counts_as_external =
      counts_as_external <- true


    method set_manager (m : < current_entity : entity; 
			      pop_entity : unit -> unit;
			      push_entity : entity -> unit >) = 
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method resolver = (None : resolver option)

    method lex_id =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : lexers)

    method set_lex_id (id : lexers) =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method line =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : int )

    method column =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : int )

    method set_line_column (_:int) (_:int) =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method full_name =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : string )

    method private set_encoding (_:string) =
      assert false

    method xml_declaration = (None : (string*string) list option)

    method set_debugging_mode (_:bool) = ()

    method lexer_obj =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : lexer_obj )

    method open_entity ?(gen_att_events:bool option) (_:bool) (_:lexers) =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : unit )

    method close_entity =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : lexers )

    method is_open = false  (* NDATA entities cannot be opened *)

    method replacement_text =
      ( raise (Validation_error ("Invalid reference to NDATA entity " ^ name))
	  : (string * bool) )

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

class external_entity
         the_resolver the_dtd the_name the_swarner the_warner 
         the_ext_id the_system_base the_p_special_empty_entities
         init_encoding : v_entity
  =
  object (self)
    inherit entity_base
              the_dtd the_name the_swarner the_warner 
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
    val ext_id = 
      match (the_ext_id : ext_id) with
	  Public(pubid,sysid) ->
	    (* normalize pubid: *)
	    Public(Pxp_aux.normalize_public_id pubid,sysid)
	| other ->
	    other

    val system_base = (the_system_base : string option)

    val p_special_empty_entities = (the_p_special_empty_entities : bool)

    val mutable resolver_is_open = false
      (* Track if the resolver is open. This is also used to find recursive
       * references of entities.
       *)

    val mutable state = At_beginning

    initializer
      v.counts_as_external <- true;


    method pxp_magic_coercion() =
      raise (Coerced_entity (self :> entity))

    method private set_encoding e =
      assert resolver_is_open;
      resolver # change_encoding e


    method full_name =
      v.name ^
      match ext_id with
	  System s    -> " = SYSTEM \"" ^ s ^ "\""
	| Public(p,s) -> " = PUBLIC \"" ^ p ^ "\" \"" ^ s ^ "\""
	| Anonymous   -> " = ANONYMOUS"
	| Private _   -> " = PRIVATE"

    method ext_id = ext_id

    method resolver = Some resolver

    method resolver_id = 
      let rid = resolver_id_of_ext_id ext_id in
      { rid with rid_system_base = system_base }

    method open_entity ?(gen_att_events=false) force_parsing init_lex_id =
      (* Note that external entities are always parsed, i.e. Begin_entity
       * and End_entity tokens embrace the inner tokens to force that
       * the entity is only called where the syntax allows it.
       *)
      if resolver_is_open then
	raise(Validation_error("Recursive reference to entity `" ^ v.name ^ "'"));
      let lex_src = 
	try
	  resolver # open_rid (self # resolver_id)
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

      let lexobj = v.lfactory#open_source lex_src in
      update_lexobj v lexobj;
      resolver_is_open <- true;
      v.prolog  <- None;
      v.lex_id  <- init_lex_id;
      state <- At_beginning;
      v.line <- 1;
      v.column <- 0;
      v.at_bof <- true;
      v.normalize_newline <- true;
      v.generate_attribute_events <- gen_att_events;


    method private handle_bof tok =
      (* This hook is only called if the stream is not empty. *)
      v.deferred_token <- Some [ tok ];
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
	      v.deferred_token <- Some [ End_entity ];
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
	failwith ("External entity " ^ v.name ^ " not open");
      resolver # close_in;
      resolver_is_open <- false;
      super # close_entity


    method is_open = resolver_is_open


    method replacement_text =
      (* Return the replacement text of the entity. The method used for this
       * is more or less the same as for internal entities; i.e. character
       * and parameter entities are resolved immediately. In addition to that,
       * external entities may begin with an "xml" processing instruction
       * which is considered not to be part of the replacement text.
       *)
      if resolver_is_open then
	raise(Validation_error("Recursive reference to entity `" ^ v.name ^ "'"));
      let lex_src = 
	try
	  resolver # open_rid (self # resolver_id)
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

      let lexobj = v.lfactory#open_source lex_src in
      update_lexobj v lexobj;
      resolver_is_open <- true;
      v.prolog  <- None;
      (* arbitrary:    lex_id  <- init_lex_id; *)
      state <- At_beginning;
      v.line <- 1;
      v.column <- 0;
      v.at_bof <- true;
      (* First check if the first token of 'lex' is <?xml...?> *)
      if lexobj#detect_xml_pi() then begin
	(* detect_xml_pi scans "<?xml" ws+. Read the rest of the XML
	 * declaration.
	 *)
	match lexobj#scan_pi_string() with
	    Some pi ->
	      begin match
		Pxp_lex_aux.scan_pi ("xml " ^ pi) v.lfactory
	      with
		  PI_xml pl ->
		    self # process_xmldecl pl
		| _ ->
		    assert false   (* cannot happen *)
	      end
	  | None ->
	      raise(WF_error("Bad XML declaration"))
      end
      else
	(* This only means that the first token was not <?xml...?>;
	 * the "Eof" token represents the empty string.
	 *)
	self # process_missing_xmldecl;
      (* Then create the replacement text. *)
      let rec scan_and_expand () =
	match lexobj#scan_dtd_string() with
	    ERef n -> "&" ^ n ^ ";" ^ scan_and_expand()
	  | CRef(-1) -> "\n" ^ scan_and_expand()
	  | CRef(-2) -> "\n" ^ scan_and_expand()
	  | CRef(-3) -> "\n" ^ scan_and_expand()
	  | CRef k -> 
	      character 
	        ?swarner:v.swarner v.encoding v.warner k ^ scan_and_expand()
	  | CharData x -> x ^ scan_and_expand()
	  | PERef n ->
	      let en = v.dtd # par_entity n in
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

(*
class external_entity the_resolver the_dtd the_name the_swarner the_warner 
                      the_ext_id
                      the_system_base
                      the_p_special_empty_entities
		      init_encoding : entity
  = 
object (self)
  inherit external_entity_base 
       the_resolver the_dtd the_name the_swarner the_warner 
       the_ext_id the_system_base the_p_special_empty_entities
       init_encoding

end
;;
 *)

class document_entity  the_resolver the_dtd the_name the_swarner the_warner
                       the_ext_id
                       the_system_base
		       init_encoding : entity
  =
  object (self)
    inherit external_entity  the_resolver the_dtd the_name the_swarner 
                             the_warner
                             the_ext_id the_system_base false 
			     init_encoding

    (* A document entity is an external entity that does not allow
     * conditional sections, and that forces that internal parameter entities
     * are properly nested.
     *)

    initializer
    v.force_parameter_entity_parsing <- true;
    v.check_text_declaration <- false;

    method counts_as_external = false
      (* Document entities count never as external! *)

    method pxp_magic_coercion() =
      raise (Coerced_entity (self :> entity))
  end
;;


class internal_entity the_dtd the_name the_swarner the_warner the_literal_value
                      the_p_internal_subset 
                      init_is_parameter_entity
		      init_encoding : entity
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
    inherit entity_base
              the_dtd the_name the_swarner the_warner 
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
    let lexobj = v.lfactory#open_string the_literal_value in
    let rec scan_and_expand () =
      match lexobj#scan_dtd_string() with
	  ERef n -> "&" ^ n ^ ";" ^ scan_and_expand()
	| CRef(-1) -> "\r\n" ^ scan_and_expand()
	| CRef(-2) -> "\r" ^ scan_and_expand()
	| CRef(-3) -> "\n" ^ scan_and_expand()
	| CRef k -> 
	    character 
	      ?swarner:v.swarner v.encoding v.warner k ^ scan_and_expand()
	| CharData x -> x ^ scan_and_expand()
	| PERef n ->
	    if p_internal_subset then
	      raise(WF_error("Restriction of the internal subset: parameter entity not allowed here"));
	    let en = v.dtd # par_entity n in
	    let (x, extref) = en # replacement_text in
	    contains_external_references <-
	      contains_external_references || extref;
	    x ^ scan_and_expand()
	| Eof ->
	    ""
	| _ ->
	    assert false
    in
    is_open <- true;
    replacement_text <- scan_and_expand();
    is_open <- false;
    v.normalize_newline <- false;
    v.counts_as_external <- false;


    method process_xmldecl (pl:prolog_token list) =
      raise(Validation_error("The encoding cannot be changed in internal entities"))


    method process_missing_xmldecl =
      ()


    method private set_encoding e =
      (* Ignored if e = "" *)
      assert(e = "");


    method open_entity ?(gen_att_events = false) force_parsing init_lex_id =
      if is_open then
	raise(Validation_error("Recursive reference to entity `" ^ v.name ^ "'"));

      p_parsed_actually <- force_parsing;
      let lexobj = v.lfactory#open_string
	             (if is_parameter_entity then
			(" " ^ replacement_text ^ " ")
		      else
			replacement_text) in
      update_lexobj v lexobj;
      v.prolog  <- None;
      v.lex_id  <- init_lex_id;
      state <- At_beginning;
      is_open <- true;
      v.line <- 1;
      v.column <- 0;
      v.at_bof <- true;       (* CHECK: Is this right? *)
      v.generate_attribute_events <- gen_att_events;


    method private handle_bof tok =
      (* This hook is only called if the stream is not empty. *)
      if p_parsed_actually then begin
	v.deferred_token <- Some [ tok ];
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
	      v.deferred_token <- Some [ End_entity ];
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
	failwith ("Internal entity " ^ v.name ^ " not open");
      is_open <- false;
      super # close_entity


    method is_open = is_open


    method replacement_text =
      if is_open then
	raise(Validation_error("Recursive reference to entity `" ^ v.name ^ "'"));
      replacement_text, contains_external_references


    method resolver = (None : resolver option)

    method pxp_magic_coercion() =
      raise (Coerced_entity (self :> entity))

  end
;;


(* An entity_section is an object that reads a section from an underlying
 * entity as if this section was an entity of its own. In detail, the
 * following rules apply:
 * - If a token is read from the entity_section, it is actually read from
 *   the underlying entity (except the first and the last token). I.e.
 *   the token stream of the entity_section and the underlying entity is
 *   essentially the same.
 * - However, the entity_section has its own lexical context. The method
 *   set_lex_id changes only the lexer ID of the entity_section, and not
 *   of the underlying entity.
 * - The first token is always Begin_entity, and the last token is always
 *   End_entity. These tokens are not taken from the underlying entity,
 *   but simpy pretended at the beginning and at the end of the section.
 * - The section begins at the current position of the (open) underlying
 *   entity when the method open_entity of the section is called. It is 
 *   an error if the underlying entity is at the beginning itself.
 *   [TODO: The latter condition is currently not checked.]
 * - The section ends when the method close_entity is called. The next
 *   token will be End_token, and then an endless sequence of Eof.
 * - A section cannot be opened a second time.
 * - Changes of encodings are ignored. (The underlying entity must do that.)
 *)

type section_state = P_bof | P_normal of int | P_pre_eof | P_eof
  (* P_normal n: The number n is the number of open inner entities *)

class entity_section (init_ent:entity) : entity =
object (self) 
  val ent = init_ent
  val mutable state = P_bof
  val mutable is_open = false
  val mutable saved_lex_id = Closed

  method is_ndata = ent # is_ndata
  method name = ent # name
  method lex_id = ent # lex_id
  method set_lex_id = 
    if not is_open then
      failwith "Pxp_entity.entity_section#set_lex_id: not open";
    ent # set_lex_id
  method line = ent # line
  method column = ent # column
  method set_line_column = 
    if not is_open then
      failwith "Pxp_entity.entity_section#set_line_column: not open";
    ent # set_line_column
  method encoding = ent # encoding
  method set_manager (_ : < current_entity : entity; 
		            pop_entity : unit -> unit;
			    push_entity : entity -> unit >) = ()
  method counts_as_external = ent # counts_as_external
  method set_counts_as_external : unit = 
    failwith "Pxp_entity.entity_section#set_counts_as_external: not possible";
  method lexer_obj = ent # lexer_obj
  method resolver = ent # resolver
  method resolver_id = ent # resolver_id
  method open_entity ?(gen_att_events:bool option) (_:bool) (lid:lexers) = 
    if is_open then
      failwith "Pxp_entity.entity_section#open_entity: already open";
    if not ent#is_open then
      failwith "Pxp_entity.entity_section#open_entity: Underlying entity is not open";
    saved_lex_id <- ent # lex_id;
    state <- P_bof;
    is_open <- true;
    ent # set_lex_id lid;
  method close_entity =
    if not is_open then
      failwith "Pxp_entity.entity_section#close_entity: not open";
    is_open <- false;
    ent # set_lex_id saved_lex_id;
    assert (match state with P_bof | P_normal _ -> true | _ -> false);
    (* CHECK: P_normal n when n>0 *)
    state <- P_pre_eof;
    saved_lex_id
  method is_open = is_open
  method replacement_text : (string * bool) =
    failwith "Pxp_entity.entity_section#replacement_text: not possible"
  method xml_declaration = ent # xml_declaration
  method set_debugging_mode = ent # set_debugging_mode
  method full_name = ent # full_name
  method next_token =
    match state with
      | P_bof ->
	  state <- P_normal 0;
	  Begin_entity
      | P_normal n -> 
	  let tok = ent # next_token in
	  (* Notes:
	   * - [tok = Begin_entity] can have two reasons: [ent] has produced
	   *   [Begin_entity], or [ent] has just found an entity reference
	   *   whose entity has been opened. Because the latter is possible
	   *   we do not catch [Begin_entity] here.
	   * - [tok = End_entity]: This token is always produced by [ent],
	   *   and so we can signal an error 
	   *)
	  ( match tok with
	      | (End_entity | Eof) ->
		  raise(Error "Cannot end entity here")
	      | _ ->
		  tok
	  )
      | P_pre_eof -> 
	  state <- P_eof; 
	  End_entity
      | P_eof -> 
	  Eof
  method next_ignored_token =
    (* We can ignore End_entity and Eof because the caller already signals
     * an error when the entity ends in an IGNORE section
     *)
    ent # next_ignored_token
  method process_xmldecl (_:prolog_token list) = ()
  method process_missing_xmldecl = ()
  method ext_id = ent # ext_id
  method notation = ent # notation

  method pxp_magic_coercion() =
    raise (Coerced_entity (self :> entity))

end
;;
