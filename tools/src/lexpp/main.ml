(* $Id: main.ml,v 1.1 2002/08/28 23:48:28 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Types
open Ucs2_to_utf8
open Printf

type config =
    { mutable char_classes_file : string;
      mutable encoding : Netconversion.encoding;
      mutable encoding_name : string;
      mutable lex_src_file : string;
      mutable link_src_file : string;
      mutable out_multiple : bool;
      mutable out_lex_prefix : string;
      mutable out_link_prefix : string;
    }

type char_classes =
    CC_generic of (definition list * string)
      (* regexp definitions, section LET *)
  | CC_wlex of (string * string)           
      (* section CLASSES, section LETS *)

let get_char_classes cfg =
  let filename =  cfg.char_classes_file in
  let cc_sections = File.read_sections filename in
  let type_generic = List.mem_assoc "TYPE_GENERIC" cc_sections in
  let type_wlex = List.mem_assoc "TYPE_WLEX" cc_sections in
  if not type_generic && not type_wlex then
    failwith ("File " ^ filename ^ ": type indicator is missing");
  if type_generic && type_wlex then
    failwith ("File " ^ filename ^ ": type indicator is ambiguous");

  let let_section =
    try List.assoc "LET" cc_sections
    with Not_found -> failwith ("File " ^ filename ^ ": no LET section") in
  
  if type_generic then begin
    let let_unicode_section =
      try List.assoc "LET_UNICODE" cc_sections
      with Not_found -> failwith ("File " ^ filename ^ ": no LET_UNICODE section") in

    CC_generic(File.parse_char_classes let_unicode_section, let_section);
  end
  else begin
    assert(type_wlex);
    let classes_section =
      try List.assoc "CLASSES" cc_sections
      with Not_found -> failwith ("File " ^ filename ^ ": no CLASSES section") 
    in
    let let_section =
      try List.assoc "LET" cc_sections
      with Not_found -> failwith ("File " ^ filename ^ ": no LET section") in
    CC_wlex(classes_section, let_section)
  end
;;


let recode_char_classes_8bit cfg cc =
  (* Works only for 8 bit character encodings. *)
  let out_enc = cfg.encoding in

  let recode_char n =
    try
      let s = Netconversion.makechar out_enc n in (* or Not_found *)
      if String.length s <> 1 then
	failwith("Character " ^  string_of_int n ^ 
		 " has a multibyte representation");
      Some(Char.code s.[0])
    with
	Not_found -> 
	  None
  in

  let rec recode_interval cur_ival cur_code last_code =
    if cur_code <= last_code then begin
      match recode_char cur_code with
	  None ->
	    recode_interval cur_ival (cur_code+1) last_code
	| Some p ->
	    ( match cur_ival with
		  None ->
		    recode_interval (Some(p,p)) (cur_code+1) last_code
		| Some(m,n) ->
		    if n+1 = p then
		      recode_interval (Some(m,p)) (cur_code+1) last_code
		    else
		      (m,n) :: recode_interval (Some(p,p)) (cur_code+1) last_code
	    )
    end
    else
      match cur_ival with
	  None ->
	    []
	| Some(m,n) ->
	    [m,n]
  in

  let rec recode_regexp re =
    (* recodes a regexp to a regexp list *)
    match re with
	Char n -> 
	  ( match recode_char n with
		Some p -> [Char p]
	      | None   -> []
	  )
      | Interval(m,n) ->
	  List.map
	    (fun (m,n) ->
	       if m=n then Char m else Interval(m,n)
	    )
	    (recode_interval None m n)
      | Identifier id ->
	  [Identifier id]
      | Concat _ ->
	  assert false      (* not used *)
  in

  let recode_def { id = id; rel = rel } =
    { id = id; rel = List.flatten (List.map recode_regexp rel) }
  in

  match cc with
      CC_wlex(_,_) ->
	cc    (* no recoding necessary *)
    | CC_generic(defs, let_section) ->
	CC_generic(List.map recode_def defs, let_section)
;;


let recode_char_classes cfg cc =
  (* FUTURE: Use Netconversion.is_ascii_compatible and is_single_byte *)
  printf 
    "[Recoding character classes to %s]\n" 
    (Netconversion.string_of_encoding cfg.encoding);
  flush stdout;
  match cfg.encoding with
      `Enc_utf8 -> 
	( match cc with
	      CC_generic(defs, let_section) ->
		CC_generic(List.map ucs2_to_utf8 defs, let_section)
	    | CC_wlex(_,_) ->
		cc
	)
    | (`Enc_java | `Enc_utf16 | `Enc_utf16_le | `Enc_utf16_be) ->
	failwith "This character encoding is not supported!"
    | _ ->
	(* May not work... *)
	recode_char_classes_8bit cfg cc
;;


let space_re =
  Netstring_str.regexp "[ \t\r\n]+" ;;


let name_of_rule rule_str =
  (* Extract the name of the rule definition. The name is the first word
   * in [rule_str] outside of comments.
   * FIXME: Comments containing inner comments or code are not supported.
   *)
  let rec skip_comments l =
    match l with
	"*)" :: l' -> l'
      | _ :: l' -> skip_comments l'
      | [] -> failwith ("Unfinished comment in: " ^ rule_str)
  and find_first_word l = 
    match l with
	"(*" :: l' -> find_first_word (skip_comments l')
      | w :: l' -> w
      | [] -> failwith ("Cannot determine rule name in: " ^ rule_str)
  in

  let words = Netstring_str.split space_re rule_str in
  find_first_word words
;;


let subst_re = Netstring_str.regexp "[$][{]\\([a-zA-Z0-9_]+\\)[}]";;

let subst_link_pattern lookup link_str =
  (* Substitutes the pattern ${name} by the value returned by the function
   * [lookup], applied on the name.
   *)
  Netstring_str.global_substitute
    subst_re
    (fun r s ->
       let name = Netstring_str.matched_group r 1 s in
       lookup name
    )
    link_str
;;


let open_out_ann name =
  printf "[writing %s]\n" name;
  flush stdout;
  let f = open_out name in
  output_string f 
    "(* THIS FILE IS GENERATED BY LEXPP. DO NOT EDIT MANUALLY! *)\n\n";
  f
;;


let write_output_files cfg cc =
  let lex_src  = File.read_sections cfg.lex_src_file in
  let link_src = File.read_sections cfg.link_src_file in

  let write_header out =
    match cc with
	CC_generic(defs,let_str) ->
	  if List.mem_assoc "HEADER" lex_src then
	    output_string out (List.assoc "HEADER" lex_src);
	  List.iter (File.print_definition out) defs;
	  output_string out let_str;
	  if List.mem_assoc "LET" lex_src then
	    output_string out (List.assoc "LET" lex_src);
      | CC_wlex(classes_str,let_str) ->
	  output_string out classes_str;
	  if List.mem_assoc "HEADER" lex_src then
	    output_string out (List.assoc "HEADER" lex_src);
	  output_string out let_str;
	  if List.mem_assoc "LET" lex_src then
	    output_string out (List.assoc "LET" lex_src);
  in

  let write_rule out is_first_rule rule_str =
    if is_first_rule then
      output_string out "rule "
    else
      output_string out "and ";
    output_string out rule_str
  in

  let module_of_rule = Hashtbl.create 10 in

  if cfg.out_multiple then begin
    let n = ref 1 in
    List.iter
      (fun (_,rule_str) ->
	 let rule_name = name_of_rule rule_str in
	 let mod_name = sprintf "%s_%02d" cfg.out_lex_prefix !n in
	 Hashtbl.add module_of_rule rule_name mod_name;
	 let out = open_out_ann (mod_name ^ ".mll") in
	 write_header out;
	 write_rule out true rule_str;
	 close_out out;
	 incr n;
      )
      (List.filter (fun (name,str) -> name = "RULE") lex_src);
  end
  else begin
    let out = open_out_ann (cfg.out_lex_prefix ^ "_01.mll") in
    write_header out;
    let is_first = ref true in
    List.iter
      (fun (_,rule_str) ->
	 let rule_name = name_of_rule rule_str in
	 Hashtbl.add module_of_rule rule_name (cfg.out_lex_prefix ^ "_01");
	 write_rule out !is_first rule_str;
	 is_first := false
      )
      (List.filter (fun (name,str) -> name = "RULE") lex_src);
    close_out out
  end;

  (*
    Hashtbl.iter
    (fun r m ->
    printf "rule %s => module %s\n" r m)
    module_of_rule;
  *)

  let link_str = 
    try List.assoc "LINK" link_src
    with Not_found ->
      failwith ("Section LINK is missing in " ^ cfg.link_src_file) in

  let lookup name =
    if name = "encoding" then
      cfg.encoding_name
    else
      let filename = 
	try Hashtbl.find module_of_rule name
	with Not_found ->
	  failwith ("No such rule: " ^  name)
      in
      String.capitalize (Filename.basename filename)
  in

  let link_str' = subst_link_pattern lookup link_str in
  let out = open_out_ann (cfg.out_link_prefix ^ ".ml") in
  output_string out link_str';
  close_out out
;;


let main() =
  let cfg = 
    { char_classes_file = "cc.def";
      encoding = `Enc_iso88591;
      encoding_name = "iso88591";
      lex_src_file = "lex.src";
      link_src_file = "link.src";
      out_multiple = false;
      out_lex_prefix = "out";
      out_link_prefix = "out_link";
    }
  in

  Arg.parse
      [ "-charclasses",  Arg.String (fun s -> cfg.char_classes_file <- s),
	             "<file>     The name of the character classes input file";
	"-lexsrc", Arg.String (fun s -> cfg.lex_src_file <- s),
	        "<file>          The name of the lex source input file";
	"-linksrc", Arg.String (fun s -> cfg.link_src_file <- s),
	         "<file>         The name of the link source input file";
	"-encoding", Arg.String (fun s -> 
				   cfg.encoding <-
				     Netconversion.encoding_of_string s;
				   cfg.encoding_name <- s;
				),
	          "<name>        The character encoding";
	"-outlexprefix", Arg.String (fun s -> cfg.out_lex_prefix <- s),
	              "<prefix>  The common prefix of the lexer files";
	"-outlinkprefix", Arg.String (fun s -> cfg.out_link_prefix <- s),
	               "<prefix> The prefix of the link file";
	"-multiple", Arg.Unit (fun () -> cfg.out_multiple <- true),
	          "              Generate multiple output files";
      ]
      (fun _ -> raise(Arg.Bad("Bad usage!")))
      "usage: lexpp <options>";
  
  let cc = get_char_classes cfg in
  let cc' = recode_char_classes cfg cc in
  write_output_files cfg cc'
;;


main();;

(* ======================================================================
 * History:
 * 
 * $Log: main.ml,v $
 * Revision 1.1  2002/08/28 23:48:28  gerd
 * 	Initial revision.
 *
 * 
 *)
