#! /bin/sh
# (*
exec ocamlfattop "$0" "$@"
*) directory ".";;

#require "netstring";;

(*
 * <ID:id-string>
 * <TYPE:type>
 * <CALL> obj # [name]
 *    note: put the name of the method/function/type in brackets
 * <SIG> method name : type
 * <DESCR> Whatever it means to call [name]
 *    note: put expressions etc. of the object language in brackets
 * <DOMAIN> Applicable to every object
 * <SEE> other-id-string
 * </ID>
 * 
 * Note: You can leave out any portion of the description except
 * <ID:id-string> and </ID>. Every part must begin on a new line.
 * The <TYPE:type> annotation can be left out; in this case the
 * type of the previous description applies.
 *
 * Types: val, fun, method, class, type, classtype, module, moduletype.
 *
 * Signatures: <SIG> AUTO tries to automate this clause. The material
 * is taken from the lines immediately preceding the structured comment.
 *
 * How to write enumerations:
 * <DESCR> The value [x] can have one of the following formats:
 *  - ["abc"] means ...
 *  - ["123"] means ...
 *  --
 * The format specification is mandatory.
 *
 * Here, every "-" begins one item. "--" terminates the enumeration.
 *)

type ftype =
    F_val | F_fun | F_method | F_class | F_type | F_classtype 
  | F_module | F_moduletype | F_exception
;;

type feature =
    { id : string;
      ftype : ftype;
      call : para list;
      fsig : string;
      descr : para list;
      domain : para list;
      see : string list;
    }

and para =
    Text of text list

and text =
    Meta of string
  | Obj of string
  | Enum of para list list
;;

type token =
    ID of string
  | ID_end
  | TYPE of ftype
  | CALL
  | SIG
  | DESCR
  | DOMAIN
  | SEE
  | NONE
;;


let ftypes =
  [ "val", F_val; "fun", F_fun; "method", F_method; "class", F_class;
    "type", F_type; "classtype", F_classtype; "module", F_module;
    "moduletype", F_moduletype; "exception", F_exception
  ];;

let pr_ftypes =
  [ F_val, "Value";
    F_fun, "Function";
    F_method, "Method";
    F_class, "Class";
    F_type, "Type";
    F_classtype, "Class type";
    F_module, "Module";
    F_moduletype, "Module type";
    F_exception, "Exception";
  ]
;;

let id_re = Str.regexp "<ID:\([-a-zA-Z0-9_]+\)>";;
let id_end_re = Str.regexp "</ID>";;
let type_re = Str.regexp "<TYPE:\([-a-zA-Z0-9_]+\)>";;
let call_re = Str.regexp "<CALL>";;
let sig_re = Str.regexp "<SIG>";;
let descr_re = Str.regexp "<DESCR>";;
let domain_re = Str.regexp "<DOMAIN>";;
let see_re = Str.regexp "<SEE>";;

let bracket_re = Str.regexp "\(\[\[\([^]]\|\(\][^]]\)\)+\]\]\)\|\(\[[^]]+\]\)";;
let star_re = Str.regexp "^[ \t]*\*?[ \t]*";;
let trailing_re = Str.regexp "[ \t]*$";;
let token_re = Str.regexp "^<[A-Z]+>[ \t]*";;


let get_token s =
  (* Check whether there is one of the tokens in the line s *)
  let check re f =
    try
      f (Str.search_forward re s 0) s
    with
	Not_found -> NONE
  in
  let rec check_list l =
    match l with
	[] -> NONE
      | (re,f) :: l' ->
	  let t = check re f in
	  if t = NONE then check_list l' else t
  in
  check_list
    [ id_re,     (fun k s -> ID (Str.matched_group 1 s));
      id_end_re, (fun k s -> ID_end);
      type_re,   (fun k s -> 
		    let t = Str.matched_group 1 s in
		    try TYPE(List.assoc t ftypes)
		    with Not_found ->
		      prerr_endline("Bad type: " ^ s);
		      NONE
		 );
      call_re,   (fun k s -> CALL);
      sig_re,    (fun k s -> SIG);
      descr_re,  (fun k s -> DESCR);
      domain_re, (fun k s -> DOMAIN);
      see_re,    (fun k s -> SEE)
    ]
;;


let keywords = ["\\<val\\>"; 
		"\\<class[ \t]+type\\>"; 
		"\\<module[ \t]+type\\>";
		"\\<type\\>"; 
		"\\<class\\>";
		"\\<module\\>";
		"\\<method\\>"; 
		"\\<exception\\>";
	       ];;

let keywords_re = List.map Str.regexp keywords;;


let exists_keyword s =
  (* Check whether there is one of the keywords on line s *)
  let check re =
    try
      ignore(Str.search_forward re s 0);
      true
    with
	Not_found -> false
  in
  List.exists check keywords_re
;;


let strip_line s =
  (* Removes '*' at the left side, and trailing spaces *)
  let s' = Str.replace_first star_re "" s in
  Str.replace_first trailing_re "" s'
;;


let norm_line s =
  (* Removes '*' and optionally a token at the left side, and
   * trailing spaces
   *)
  let s' = strip_line s in
  Str.replace_first token_re "" s'
;;


let split_see s =
  (* Split a SEE spec *)
  Str.split (Str.regexp "[ \t]+") s
;;


let encode = Netencoding.Html.encode_from_latin1;;

exception Feature of feature;;


let rec parse_features inch default_type = (
  (* Read lines from inch, parse the features contained in them, and
   * return them as list
   *)
  (* Search the ID token: *)
  let sig_string = ref "" in
  let sig_mode = ref false in
  try
    while true do
      let s = input_line inch in
      match get_token s with
	  ID id ->
	    let ft = parse_feature inch default_type s id !sig_string in
	    raise(Feature ft)
	| NONE ->
	    if exists_keyword s then begin
	      sig_string := s;
	      sig_mode := true;
	    end
	    else
	      if !sig_mode then sig_string := !sig_string ^ "\n" ^ s;
	    ()
	| _ ->
	    prerr_endline ("WARNING! Unexpected token: " ^ s)
    done;
    assert false
  with
      End_of_file ->
	[]
    | Feature ft ->
	ft :: parse_features inch (ft.ftype)
)
and parse_feature inch default_type s id sig_auto_string = (
  (* In line s there is a token <ID:id>. This function parses the record
   * until </ID> and returns it.
   *)
  let type_spec = ref default_type in
  let call_text = ref [] in
  let sig_str = ref "" in
  let descr_text = ref [] in
  let domain_text = ref [] in
  let see_list = ref [] in
  let cont = ref true in
  let line = ref (input_line inch) in

  while !cont do
    let t = get_token !line in
    match t with
	ID id' ->
	  prerr_endline("Unexpected token: " ^ !line);
	  line := input_line inch
      | CALL ->
	  let tx,line' = parse_para_list inch !line in
	  call_text := tx;
	  line := line'
      | SIG ->
	  let s,line' = parse_string inch !line in
	  if s = "AUTO" then
	    sig_str := sig_auto_string
	  else
	    sig_str := s;
	  line := line'
      | DESCR ->
	  let tx,line' = parse_para_list inch !line in
	  descr_text := tx;
	  line := line'
      | DOMAIN ->
	  let tx,line' = parse_para_list inch !line in
	  domain_text := tx;
	  line := line'
      | SEE ->
	  let s,line' = parse_string inch !line in
	  see_list := split_see s;
	  line := line'
      | ID_end ->
	  cont := false
      | TYPE t ->
	  type_spec := t;
	  line := input_line inch
      | NONE ->
	  prerr_endline("Ignored material: " ^ !line)
  done;
  { id = id;
    ftype = !type_spec;
    fsig = !sig_str;
    call = !call_text;
    descr = !descr_text;
    domain = !domain_text;
    see = !see_list;
  }
)
and parse_string inch s = (
  (* Parses a <SEE> or <SIG> specification. *)
  let str = ref (norm_line s) in
  let cont = ref true in
  let line = ref (input_line inch) in

  while !cont do
    match get_token !line with
	NONE ->
	  str := !str ^ (strip_line !line);
	  line := input_line inch
      | _ ->
	  cont := false
  done;
  !str, !line
)
and parse_para_list ?(inner=false) inch s = (
  (* Parses a list of paragraphs. Paragraphs are separated by blank lines.
   * The list stops if either:
   * - a token is found (but not if the token is on the very first line)
   * - ~inner=true, and a line begins with a '-'
   * The function returns (pl, line') where pl is the list of paras,
   * and line' is the line that caused the parser stopping.
   *)
  let paras = ref [] in
  let line = ref (norm_line s) in
  let cont = ref true in

  while !cont do
    if !line = "" then 
      line := (strip_line (input_line inch))
    else
      if inner && !line.[0] = '-' then
	cont := false
      else
	match get_token !line with
	    NONE ->
	      let p, line' = parse_para ~inner inch !line in
	      paras := !paras @ [p];
	      line := line'
	  | _ ->
	      cont := false
  done;

  !paras, !line
)
and parse_para ?(inner = false) inch s = (
  (* Parses a single paragraph. The paragraph ends with a blank line,
   * or with a token. Furthermore, if ~inner=true, the paragraph ends
   * with a line beginning with a '-'.
   *)
  let text = ref [] in
  let line = ref s in
  let cont = ref true in

  while !cont do
    if !line = "" then 
      cont := false
    else begin
      if !line.[0] = '-' then begin
	if inner then
	  cont := false
	else begin
	  let e, line' = parse_enum inch !line in
	  text := !text @ [Enum e];
	  if line' = "--" then
	    line := strip_line (input_line inch)
	  else
	    line := line'
	end
      end
      else begin
	if get_token !line = NONE then begin
	  let l = Str.full_split bracket_re !line in
	  List.iter
	    (function
		 Str.Text s ->
		   text := !text @ [Meta s]
	       | Str.Delim s ->
		   let s' =
		     if s.[1] = '[' then
		       String.sub s 2 (String.length s - 4)
		     else
		       String.sub s 1 (String.length s - 2)
		   in
		   text := !text @ [Obj s'];
	    )
	    l;
	  line := strip_line (input_line inch)
	end
	else
	  cont := false
      end
    end
  done;
  Text !text, !line
)
and parse_enum inch s = (
  (* Parses an enumeration *)
  let line = ref s in
  let enum = ref [] in
  while !line <> "" && !line.[0] = '-' && !line <> "--" do
    let l1 = String.sub !line 1 (String.length !line - 1) in
    let pl, line' = parse_para_list ~inner:true inch l1 in
    enum := !enum @ [pl];
    line := line'
  done;
  !enum, !line
)
;;


let rec print_features masterch prefix fl =
  let n = ref 1 in
  List.iter
    (fun ft ->
       let filename = prefix ^ string_of_int !n in
       let outch = open_out filename in
       print_feature outch ft;
       close_out outch;
       output_string masterch 
	 ("<!ENTITY " ^ ft.id ^ " SYSTEM \"" ^ filename ^ "\">\n");
       incr n
    )
    fl

and print_feature outch ft =
  begin 
    match ft.call with
	h :: t ->
	  let p = List.assoc ft.ftype pr_ftypes in
	  output_string outch ("<FORMALPARA>\n");
	  output_string outch ("<TITLE>" ^ p ^ ":</TITLE>\n");
	  print_para ~call:true outch h;
	  output_string outch ("</FORMALPARA>\n");
	  print_para_list ~call:true outch t
      | [] ->
	  ()
  end;
  
  begin
    match ft.fsig with
	"" ->
	  ()
      | s ->
	  output_string outch ("<FORMALPARA>\n");
	  output_string outch ("<TITLE>Signature:</TITLE>\n");
	  output_string outch "<PARA>\n";
	  output_string outch "<PROGRAMLISTING>\n";
	  output_string outch (encode s);
	  output_string outch "</PROGRAMLISTING>\n";
	  output_string outch "</PARA>\n";
	  output_string outch ("</FORMALPARA>\n");
  end;

  begin
    match ft.descr with
	h :: t ->
	  output_string outch ("<FORMALPARA>\n");
	  output_string outch ("<TITLE>Description:</TITLE>\n");
	  print_para outch h;
	  output_string outch ("</FORMALPARA>\n");
	  print_para_list outch t;
      | [] ->
	  ()
  end;

  begin
    match ft.domain with
	h :: t ->
	  output_string outch ("<FORMALPARA>\n");
	  output_string outch ("<TITLE>Domain:</TITLE>\n");
	  print_para outch h;
	  output_string outch ("</FORMALPARA>\n");
	  print_para_list outch t
      | [] ->
	  ()
  end;
  
  if ft.see <> [] then begin
    output_string outch ("<FORMALPARA>\n");
    output_string outch ("<TITLE>See also:</TITLE>\n");
    output_string outch ("<PARA>\n");
    List.iter
      (fun link ->
	 output_string outch ("<XREF LINKEND=\"" ^ link ^ "\" ENDTERM=\"" ^ link ^ "\">\n")
      )
      ft.see;
    output_string outch ("</PARA>\n");
    output_string outch ("</FORMALPARA>\n");
  end

and print_para_list ?(call=false) outch pl =
  List.iter (print_para ~call outch) pl

and print_para ?(call=false) outch (Text p) =
  output_string outch ("<PARA>\n");
  if call then output_string outch "<LITERAL>";
  List.iter
    (function
	 Meta s ->
	   output_string outch (encode s ^ "\n")
       | Obj s ->
	   if call then begin
	     output_string outch "<COMMAND>";
	     output_string outch (encode s);
	     output_string outch "</COMMAND>\n";
	   end
	   else begin
	     output_string outch "<LITERAL>";
	     output_string outch (encode s);
	     output_string outch "</LITERAL>\n";
	   end
       | Enum e ->
	   output_string outch "<ITEMIZEDLIST>\n";
	   List.iter
	     (fun item ->
		output_string outch "<LISTITEM>\n";
		print_para_list outch item;
		output_string outch "</LISTITEM>\n";
	     )
	     e;
	   output_string outch "</ITEMIZEDLIST>\n";
    )
    p;
  if call then output_string outch "</LITERAL>";
  output_string outch ("</PARA>\n")
;;


let main() =
  let flist = ref [] in
  
  (* Parse everything: *)
  
  let skip = ref true in
  Array.iter
    (fun file ->
       if not !skip then begin
	 prerr_endline ("Opening " ^ file);
	 let ch = open_in file in
	 let fl = parse_features ch F_val in
	 close_in ch;
	 flist := fl @ !flist
       end;
       skip := false
    )
    Sys.argv;

  (* Print everything: *)

  let master = open_out "extracted.ent" in
  print_features master "extracted/f" !flist;
  close_out master
;;


main()
;;
