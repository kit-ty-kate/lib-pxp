(* $Id: to_text.ml,v 1.3 2000/07/08 17:58:17 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_document


(**********************************************************************)
(* The box class represents formatted text                            *)
(**********************************************************************)

class type formatted_text =
  object
    method output : int -> int -> out_channel -> unit
	(* output initial_indent indent ch:
	 * 'initial_indent' is how far the first line should be indented;
	 * 'indent' how far the rest. 'ch' is the channel on which the lines
	 * are to be printed.
	 *)

    method multiline : bool
        (* whether the box occupies multiple lines *)

    method width_of_last_line : int
        (* returns the width of the last line *)
  end
;;


type text =
    Text of string
  | Box of formatted_text
;;


let textwidth tl =
  let rec compute tl r =
    match tl with
	[] -> r
      | t :: tl' ->
	  begin match t with
	      Text s ->
		 compute tl' (r + String.length s)
	    | Box b ->
		if b # multiline then
		  compute tl' (b # width_of_last_line)
		else
		  compute tl' (r + b # width_of_last_line)
	  end
  in
  compute (List.rev tl) 0
;;


class box the_initial_width the_width =
  object (self)

    (* The 'initial_width' is the width that is available on the first
     * line of output; the 'width' is the width that is available in the
     * rest.
     *)

    val initial_width = the_initial_width
    val width = the_width

    (* state: *)

    val mutable space_added = false
    val mutable linefeed_added = false
    val mutable is_first_line = true
    val mutable lines = []
        (* lines in reverse order (first line = last element) *)
    val mutable current_line = []
        (* not member of 'lines'; again reverse order *)
    val mutable current_indent = 0

    method add_space =
      if not space_added then begin
	space_added <- true;
	linefeed_added <- true;
	current_line <- Text " " :: current_line
      end

    method ignore_space =
      space_added <- true;
      linefeed_added <- true

    method add_linefeed =
      if not linefeed_added then begin
	linefeed_added <- true;
	if not space_added then
	  current_line <- Text " " :: current_line
      end

    method ignore_linefeed =
      linefeed_added <- true

    method add_newline =
      lines <- current_line :: lines;
      current_line <- [];
      space_added <- true;
      linefeed_added <- true;
      is_first_line <- false;
      current_indent <- 0;

    method add_word s =
      (* first try to add 's' to 'current_line' *)
      let current_line' = Text s :: current_line in
      let current_width =
	if is_first_line then initial_width else width in
      if textwidth current_line' + current_indent <= current_width then begin
	(* ok, the line does not become too long *)
	current_line <- current_line';
	space_added <- false;
	linefeed_added <- false
      end
      else begin
	(* The line would be too long. *)
	lines <- current_line :: lines;
	current_line <- [Text s];
	space_added <- false;
	linefeed_added <- false;
	is_first_line <- false;
	current_indent <- 0;
      end

    method add_box b =
      current_line <- Box b :: current_line;
      space_added <- false;
      linefeed_added <- false;
 

    method width_of_last_line =
      textwidth current_line + current_indent


    method available_width =
      let current_width =
	if is_first_line then initial_width else width in
      current_width - textwidth current_line - current_indent
  

    method multiline =
      lines <> [] or
      (List.exists 
	 (function 
	      Text _ -> false
	    | Box b -> b # multiline) 
	 current_line)

    method output initial_indent indent ch =
      let eff_lines =
	List.rev
	  (current_line :: lines) in
      let rec out_lines cur_indent ll =
	match ll with
	    [] ->  ()
	  | l :: ll' ->
	      output_string ch (String.make cur_indent ' ');
	      List.iter
		(function
		     Text s ->
		       output_string ch s
		   | Box b ->
		       b # output 0 indent ch
		)
		(List.rev l);
	      if ll' <> [] then 
		output_string ch "\n";
	      out_lines indent ll'
      in
      out_lines initial_indent eff_lines
  end
;;


class listitem_box listmark indent totalwidth =
  let initial_newline = String.length listmark >= indent in
  object (self)
    inherit box totalwidth (totalwidth - indent) as super

    val extra_indent = indent

    initializer
    self # add_word listmark;
    if initial_newline then
      self # add_newline
    else begin
      current_line <- Text (String.make (indent - String.length listmark) ' ')
                      :: current_line;
      space_added <- true;
      linefeed_added <- true;
    end


    method output initial_indent indent ch =
      super # output initial_indent (indent + extra_indent) ch
  end
;;
      

(**********************************************************************)
(* Footnotes etc.                                                     *)
(**********************************************************************)


class type footnote_printer =
  object
    method footnote_to_box : store_type -> box -> unit
  end

and store_type =
  object
    method alloc_footnote : footnote_printer -> int
    method print_footnotes : box -> unit
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

    method print_footnotes (b : box) =
      if footnotes <> [] then begin
	b # add_newline;
	b # add_newline;
	let w = b # available_width in
	b # add_word (String.make (w/3) '-');
	b # add_newline;
	b # add_newline;
	List.iter
	  (fun (_,n) -> 
	     n # footnote_to_box (self : #store_type :> store_type) b)
	  footnotes;
	b # add_newline;
      end
  end
;;



(**********************************************************************)
(* The extension objects                                              *)
(**********************************************************************)


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

    method virtual to_box : store -> box -> unit
      (* to_box store b:
       * formats the element using box 'b' 
       *)
  end
;;


class only_data =
  object (self)
    inherit shared

    val white_space_re = Str.regexp "[ \t]+\\|\n"

    method to_box store b =
      let s = self # node # data in
      let splitted = Str.full_split white_space_re s in
      List.iter
	(function
	     Str.Delim "\n" ->
	       b # add_linefeed
	   | Str.Delim _ ->
	       b # add_space
	   | Str.Text s ->
	       b # add_word s)
	splitted
  end
;;


class no_markup =
  object (self)
    inherit shared

    method to_box store b =
      List.iter
	(fun n -> n # extension # to_box store b)
	(self # node # sub_nodes)
  end
;;


class readme =
  object (self)
    inherit shared

    method to_box store b =
      let title = 
	match self # node # attribute "title" with
	    Value s -> s
	  | _ -> assert false
      in
      let w = b # available_width in
      let line = String.make (w-1) '*' in
      b # add_word line;
      b # add_newline;
      b # add_word title;
      b # add_newline;
      b # add_word line;
      b # add_newline;
      b # add_newline;
      (* process main content: *)
      List.iter
	(fun n -> n # extension # to_box store b)
	(self # node # sub_nodes);
      (* now process footnotes *)
      store # print_footnotes b;
      (* trailer *)
      b # add_newline;
  end
;;


class section the_tag =
  object (self)
    inherit shared

    val tag = the_tag

    method to_box store b =
      let sub_nodes = self # node # sub_nodes in
      match sub_nodes with
	  title_node :: rest ->
	    b # add_newline;
	    let w = b # available_width in
	    let line = String.make (w-1) tag in
	    b # add_word line;
	    b # add_newline;
	    b # add_word (title_node # data);
	    b # add_newline;
	    b # add_word line;
	    b # add_newline;
	    List.iter
	      (fun n -> 
		 n # extension # to_box store b)
	      rest;
	| _ ->
	    assert false
  end
;;

class sect1 = section '=';;
class sect2 = section '-';;
class sect3 = section ':';;


class p =
  object (self)
    inherit shared
  
    method to_box store b =
      let within_list = 
	match self # node # parent # node_type with
	    T_element "li" -> true
	  | T_element _    -> false 
	  | _ -> assert false
      in
      if not within_list then
	b # add_newline;
      let w = b # available_width in
      let b' = new box w w in
      b' # ignore_space;
      List.iter
	(fun n -> n # extension # to_box store b')
	(self # node # sub_nodes);
      b # add_box (b' :> formatted_text);
      b # add_newline;
  end
;;


class li =
  object (self)
    inherit shared
  
    method to_box store b =
      b # add_newline;
      let w = b # available_width in
      let b' = new listitem_box "-" 3 w in
      b' # ignore_space;
      List.iter
	(fun n -> n # extension # to_box store b')
	(self # node # sub_nodes);
      b # add_box (b' :> formatted_text);
  end
;;


class code =
  object (self)
    inherit shared
  
    method to_box store b =
      b # add_newline;
      let w = b # available_width in
      let b' = new box w w in
      b' # ignore_space;
      let data = self # node # data in
      (* convert tabs *)
      let l = String.length data in
      let rec add s i column =
	(* this is very ineffective but comprehensive: *)
	if i < l then
	  match data.[i] with
	      '\t' ->
		let n = 8 - (column mod 8) in
		add (s ^ String.make n ' ') (i+1) (column + n)
	    | '\n' ->
		b' # add_word s;
		b' # add_newline;
		add "" (i+1) 0
	    | c ->
		add (s ^ String.make 1 c) (i+1) (column + 1)
	else
	  if s <> "" then begin
	    b' # add_word s;
	    b' # add_newline;
	  end
      in
      add "" 0 0;
      b # add_box (b' :> formatted_text);
      b # add_newline;
  end
;;


class br =
  object (self)
    inherit shared

    method to_box store b =
      b # add_newline;
  end
;;


class footnote =
  object (self)
    inherit shared

    val mutable footnote_number = 0

    method to_box store b =
      let number = 
	store # alloc_footnote (self : #shared :> footnote_printer) in
      footnote_number <- number;
      b # add_space;
      b # add_word ("[" ^ string_of_int number ^ "]");

    method footnote_to_box store b =
      let w = b # available_width in
      let n = "[" ^ string_of_int footnote_number ^ "]" in
      let b' = new listitem_box n 6 w in
      b' # ignore_space;
      List.iter
	(fun n -> n # extension # to_box store b')
	(self # node # sub_nodes);
      b # add_box (b' :> formatted_text);
      b # add_newline;
      b # add_newline;
 
  end
;;


class a =
  object (self)
    inherit shared

    val mutable footnote_number = 0
    val mutable a_href = ""

    method to_box store b =
      let href =
	match self # node # attribute "href" with
	    Value v -> "see " ^ v
	  | Valuelist _ -> assert false
	  | Implied_value ->
	      begin match self # node # attribute "readmeref" with
		  Value v -> "see file " ^ v 
		| Valuelist _ -> assert false
		| Implied_value ->
		    ""
	      end
      in
      a_href <- href;
      List.iter
	(fun n -> n # extension # to_box store b)
	(self # node # sub_nodes);
      if href <> "" then begin
	let number = 
	  store # alloc_footnote (self : #shared :> footnote_printer) in
	footnote_number <- number;
	b # add_space;
	b # add_word ("[" ^ string_of_int number ^ "]");
      end

    method footnote_to_box store b =
      if a_href <> "" then begin
	let w = b # available_width in
	let n = "[" ^ string_of_int footnote_number ^ "]" in
	let b' = new listitem_box n 6 w in
	b' # ignore_space;
	b' # add_word a_href;
	b # add_box (b' :> formatted_text);
	b # add_newline;
	b # add_newline;
      end      
  end
;;

(**********************************************************************)

open Pxp_yacc

let tag_map =
  make_spec_from_mapping
    ~data_exemplar:(new data_impl (new only_data))
    ~default_element_exemplar:(new element_impl (new no_markup))
    ~element_mapping:
       (let m = Hashtbl.create 50 in
	Hashtbl.add m "readme"
	              (new element_impl (new readme));
	Hashtbl.add m "sect1"
	              (new element_impl (new sect1));
	Hashtbl.add m "sect2"
	              (new element_impl (new sect2));
	Hashtbl.add m "sect3"
	              (new element_impl (new sect3));
	Hashtbl.add m "title"
	              (new element_impl (new no_markup));
	Hashtbl.add m "p"
	              (new element_impl (new p));
	Hashtbl.add m "br"
	              (new element_impl (new br));
	Hashtbl.add m "code"
	              (new element_impl (new code));
	Hashtbl.add m "em"
	              (new element_impl (new no_markup));
	Hashtbl.add m "ul"
	              (new element_impl (new no_markup));
	Hashtbl.add m "li"
	              (new element_impl (new li));
	Hashtbl.add m "footnote"
	              (new element_impl (new footnote : #shared :> shared));
	Hashtbl.add m "a"
	              (new element_impl (new a : #shared :> shared));
	m);
;;


    

(* ======================================================================
 * History:
 * 
 * $Log: to_text.ml,v $
 * Revision 1.3  2000/07/08 17:58:17  gerd
 * 	Updated because of PXP API changes.
 *
 * Revision 1.2  2000/06/04 20:25:38  gerd
 * 	Updates because of renamed PXP modules.
 *
 * Revision 1.1  1999/08/22 22:29:32  gerd
 * 	Initial revision.
 *
 * 
 *)
