(* $Id: to_html.ml,v 1.1 1999/08/22 22:29:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Markup_types
open Markup_document


class type footnote_printer =
  object
    method footnote_to_html : store_type -> out_channel -> unit
  end

and store_type =
  object
    method alloc_footnote : footnote_printer -> int
    method print_footnotes : out_channel -> unit
  end
;;


class store =
  object (self)

    val mutable footnotes = ( [] : (int * footnote_printer) list )
    val mutable next_footnote_number = 1

    method alloc_footnote n =
      let number = next_footnote_number in
      next_footnote_number <- number+1;
      footnotes <- footnotes @ [ number, n ];
      number

    method print_footnotes ch =
      if footnotes <> [] then begin
	output_string ch "<hr align=left noshade=noshade width=\"30%\">\n";
	output_string ch "<dl>\n";
	List.iter
	  (fun (_,n) -> 
	     n # footnote_to_html (self : #store_type :> store_type) ch)
	  footnotes;
	output_string ch "</dl>\n";
      end

  end
;;


let escape_html s =
  Str.global_substitute
    (Str.regexp "<\\|>\\|&\\|\"")
    (fun s ->
      match Str.matched_string s with
        "<" -> "&lt;"
      | ">" -> "&gt;"
      | "&" -> "&amp;"
      | "\"" -> "&quot;"
      | _ -> assert false)
    s
;;


class virtual shared =
  object (self)

    (* --- default_ext --- *)

    val mutable node = (None : shared node option)

    method clone = {< >} 
    method node =
      match node with
          None ->
            assert false
        | Some n -> n
    method set_node n =
      node <- Some n

    (* --- virtual --- *)

    method virtual to_html : store -> out_channel -> unit

  end
;;


class only_data =
  object (self)
    inherit shared

    method to_html store ch =
      output_string ch (escape_html (self # node # data))
  end
;;


class no_markup =
  object (self)
    inherit shared

    method to_html store ch =
      List.iter
	(fun n -> n # extension # to_html store ch)
	(self # node # sub_nodes)
  end
;;


class readme =
  object (self)
    inherit shared

    method to_html store ch =
      (* output header *)
      output_string 
	ch "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">";
      output_string
	ch "<!-- WARNING! This is a generated file, do not edit! -->\n";
      let title = 
	match self # node # attribute "title" with
	    Value s -> s
	  | _ -> assert false
      in
      output_string ch "<html><header><title>\n";
      output_string ch (escape_html title);
      output_string ch "</title></header>\n";
      output_string ch "<body bgcolor=\"white\">\n";
      output_string ch "<h1>";
      output_string ch (escape_html title);
      output_string ch "</h1>\n";
      (* process main content: *)
      List.iter
	(fun n -> n # extension # to_html store ch)
	(self # node # sub_nodes);
      (* now process footnotes *)
      store # print_footnotes ch;
      (* trailer *)
      output_string ch "</html>\n";

  end
;;


class section the_tag =
  object (self)
    inherit shared

    val tag = the_tag

    method to_html store ch =
      let sub_nodes = self # node # sub_nodes in
      match sub_nodes with
	  title_node :: rest ->
	    output_string ch ("<" ^ tag ^ ">\n");
	    title_node # extension # to_html store ch;
	    output_string ch ("\n</" ^ tag ^ ">");
	    List.iter
	      (fun n -> n # extension # to_html store ch)
	      rest
	| _ ->
	    assert false
  end
;;

class sect1 = section "h1";;
class sect2 = section "h3";;
class sect3 = section "h4";;


class map_tag the_target_tag =
  object (self)
    inherit shared

    val target_tag = the_target_tag

    method to_html store ch =
      output_string ch ("<" ^ target_tag ^ ">\n");
      List.iter
	(fun n -> n # extension # to_html store ch)
	(self # node # sub_nodes);
      output_string ch ("\n</" ^ target_tag ^ ">");
  end
;;

class p = map_tag "p";;
class em = map_tag "b";;
class ul = map_tag "ul";;
class li = map_tag "li";;


class br =
  object (self)
    inherit shared

    method to_html store ch =
      output_string ch "<br>\n";
      List.iter
	(fun n -> n # extension # to_html store ch)
	(self # node # sub_nodes);
  end
;;


class code =
  object (self)
    inherit shared

    method to_html store ch =
      let data = self # node # data in
      (* convert tabs *)
      let l = String.length data in
      let rec preprocess i column =
	(* this is very ineffective but comprehensive: *)
	if i < l then
	  match data.[i] with
	      '\t' ->
		let n = 8 - (column mod 8) in
		String.make n ' ' ^ preprocess (i+1) (column + n)
	    | '\n' ->
		"\n" ^ preprocess (i+1) 0
	    | c ->
		String.make 1 c ^ preprocess (i+1) (column + 1)
	else
	  ""
      in
      output_string ch "<p><pre>";
      output_string ch (escape_html (preprocess 0 0));
      output_string ch "</pre></p>";

  end
;;


class a =
  object (self)
    inherit shared

    method to_html store ch =
      output_string ch "<a ";
      let href =
	match self # node # attribute "href" with
	    Value v -> escape_html v
	  | Valuelist _ -> assert false
	  | Implied_value ->
	      begin match self # node # attribute "readmeref" with
		  Value v -> escape_html v ^ ".html"
		| Valuelist _ -> assert false
		| Implied_value ->
		    ""
	      end
      in
      if href <> "" then
	output_string ch ("href=\""  ^ href ^ "\"");
      output_string ch ">";
      output_string ch (escape_html (self # node # data));
      output_string ch "</a>";
	
  end
;;


class footnote =
  object (self)
    inherit shared

    val mutable footnote_number = 0

    method to_html store ch =
      let number = 
	store # alloc_footnote (self : #shared :> footnote_printer) in
      let foot_anchor = 
	"footnote" ^ string_of_int number in
      let text_anchor =
	"textnote" ^ string_of_int number in
      footnote_number <- number;
      output_string ch ( "<a name=\"" ^ text_anchor ^ "\" href=\"#" ^ 
			 foot_anchor ^ "\">[" ^ string_of_int number ^ 
			 "]</a>" )

    method footnote_to_html store ch =
      (* prerequisite: we are in a definition list <dl>...</dl> *)
      let foot_anchor = 
	"footnote" ^ string_of_int footnote_number in
      let text_anchor =
	"textnote" ^ string_of_int footnote_number in
      output_string ch ("<dt><a name=\"" ^ foot_anchor ^ "\" href=\"#" ^ 
			text_anchor ^ "\">[" ^ string_of_int footnote_number ^ 
			"]</a></dt>\n<dd>");
      List.iter
	(fun n -> n # extension # to_html store ch)
	(self # node # sub_nodes);
      output_string ch ("\n</dd>")
 
  end
;;

(**********************************************************************)

open Markup_yacc

let tag_map =
  { map =
      (let m = Hashtbl.create 50 in
       Hashtbl.add m (T_element "readme")
	             (new element_impl (new readme));
       Hashtbl.add m (T_element "sect1")
	             (new element_impl (new sect1));
       Hashtbl.add m (T_element "sect2")
	             (new element_impl (new sect2));
       Hashtbl.add m (T_element "sect3")
	             (new element_impl (new sect3));
       Hashtbl.add m (T_element "title")
	             (new element_impl (new no_markup));
       Hashtbl.add m (T_element "p")
	             (new element_impl (new p));
       Hashtbl.add m (T_element "br")
	             (new element_impl (new br));
       Hashtbl.add m (T_element "code")
	             (new element_impl (new code));
       Hashtbl.add m (T_element "em")
	             (new element_impl (new em));
       Hashtbl.add m (T_element "ul")
	             (new element_impl (new ul));
       Hashtbl.add m (T_element "li")
	             (new element_impl (new li));
       Hashtbl.add m (T_element "footnote")
	             (new element_impl (new footnote : #shared :> shared));
       Hashtbl.add m (T_element "a")
	             (new element_impl (new a));
       Hashtbl.add m T_data
	             (new data_impl (new only_data) "");
       m);

    default_element = new element_impl (new no_markup);
  }
;;

(* ======================================================================
 * History:
 * 
 * $Log: to_html.ml,v $
 * Revision 1.1  1999/08/22 22:29:32  gerd
 * 	Initial revision.
 *
 * 
 *)
