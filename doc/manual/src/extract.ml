#! /bin/sh
# (*
exec ocamlfattop "$0" "$@"
*) directory ".";;

#require "netstring";;

type feature =
    { id : string;
      call : string;
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
  | CALL
  | DESCR
  | DOMAIN
  | SEE
  | NONE
;;

let id_re = Str.regexp "<ID:\([-a-zA-Z0-9_]+\)>";;
let id_end_re = Str.regexp "</ID>";;
let call_re = Str.regexp "<CALL>";;
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
      call_re,   (fun k s -> CALL);
      descr_re,  (fun k s -> DESCR);
      domain_re, (fun k s -> DOMAIN);
      see_re,    (fun k s -> SEE)
    ]
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


let rec parse_features inch = (
  (* Read lines from inch, parse the features contained in them, and
   * return them as list
   *)
  (* Search the ID token: *)
  try
    while true do
      let s = input_line inch in
      match get_token s with
	  ID id ->
	    let ft = parse_feature inch s id in
	    raise(Feature ft)
	| NONE ->
	    ()
	| _ ->
	    prerr_endline ("WARNING! Unexpected token: " ^ s)
    done;
    assert false
  with
      End_of_file ->
	[]
    | Feature ft ->
	ft :: parse_features inch
)
and parse_feature inch s id = (
  (* In line s there is a token <ID:id>. This function parses the record
   * until </ID> and returns it.
   *)
  let call_string = ref "" in
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
	  let s,line' = parse_call inch !line in
	  call_string := s;
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
	  let l,line' = parse_see inch !line in
	  see_list := l;
	  line := line'
      | ID_end ->
	  cont := false
      | NONE ->
	  prerr_endline("Ignored material: " ^ !line)
  done;
  { id = id;
    call = !call_string;
    descr = !descr_text;
    domain = !domain_text;
    see = !see_list;
  }
)
and parse_call inch s = (
  (* Parses a <CALL> specification. *)
  let call = ref (norm_line s) in
  let cont = ref true in
  let line = ref (input_line inch) in

  while !cont do
    match get_token !line with
	NONE ->
	  call := !call ^ " " ^ (norm_line !line);
	  line := input_line inch
      | _ ->
	  cont := false
  done;
  !call, !line
)
and parse_see inch s = (
  (* Parses a <SEE> specification. *)
  let see = ref (split_see(norm_line s)) in
  let cont = ref true in
  let line = ref (input_line inch) in

  while !cont do
    match get_token !line with
	NONE ->
	  see := !see @ split_see(strip_line !line);
	  line := input_line inch
      | _ ->
	  cont := false
  done;
  !see, !line
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
  if ft.call <> "" then begin
    output_string outch ("<FORMALPARA>\n");
    output_string outch ("<TITLE>Method</TITLE>\n");
    output_string outch ("<PARA>\n");
    output_string outch ("<LITERAL>\n");
    output_string outch (encode ft.call);
    output_string outch ("</LITERAL>\n");
    output_string outch ("</PARA>\n");
    output_string outch ("</FORMALPARA>\n");
  end;
  
  begin
    match ft.descr with
	h :: t ->
	  output_string outch ("<FORMALPARA>\n");
	  output_string outch ("<TITLE>Description</TITLE>\n");
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
	  output_string outch ("<TITLE>Domain</TITLE>\n");
	  print_para outch h;
	  output_string outch ("</FORMALPARA>\n");
	  print_para_list outch t
      | [] ->
	  ()
  end;
  
  if ft.see <> [] then begin
    output_string outch ("<FORMALPARA>\n");
    output_string outch ("<TITLE>See also</TITLE>\n");
    output_string outch ("<PARA>\n");
    List.iter
      (fun link ->
	 output_string outch ("<LINK LINKEND=\"" ^ link ^ "\">\n")
      )
      ft.see;
    output_string outch ("</PARA>\n");
    output_string outch ("</FORMALPARA>\n");
  end

and print_para_list outch pl =
  List.iter (print_para outch) pl

and print_para outch (Text p) =
  output_string outch ("<PARA>\n");
  List.iter
    (function
	 Meta s ->
	   output_string outch (encode s ^ "\n")
       | Obj s ->
	   output_string outch "<LITERAL>";
	   output_string outch (encode s);
	   output_string outch "</LITERAL>\n";
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
	 let fl = parse_features ch in
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
