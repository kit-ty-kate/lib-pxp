(* $Id$ *)

open Pxp_types
open Pxp_document
open Netcgi
open Netcgi_types

(* Configuration: At least the [encoding] should be the same for all
 * XML data the program processes. Furthermore, it is a good idea
 * to use only one namespace manager.
 *
 * We declare the following normprefixes:
 * l: The namespace for library data
 * h: The namespace for XHTML
 *)

let mng = Pxp_dtd.create_namespace_manager();;
mng # add_namespace "l" "http://sample/library/ns";;
mng # add_namespace "h" "http://www.w3.org/1999/xhtml";;

let config =
  { default_config with
      encoding = `Enc_utf8;
      enable_namespace_processing = Some mng
  } ;;

(* The [scope] is only needed for the generated output.
 * It is sufficient to mention the XHTML namespace. It is
 * bound to the default prefix.
 *)

let scope = Pxp_dtd.create_namespace_scope 
	      ~decl:[ "", "http://www.w3.org/1999/xhtml" ]  
	      mng;;


(* We use UTF-8 as representation, tell pxp-pp: *)
<:pxp_charset< representation="UTF-8" >>;;

(* Our library has books, and other media, which are either video
 * or audio. This DTD describes only the overall structure.
 * It does not (and cannot) enforce certain formats of data fields
 * (we would need schemas for that).
 *
 * The elements are part of the namespace http://sample/library,
 * and so we are using the namespace prefix "l".
 *)

let library_dtd_string =
  <:pxp_text<
    <!-- This DTD has the SYSTEM URI http://sample/library/dtd -->

    <!ELEMENT l:library (l:book|l:video|l:audio)*>

    <!ELEMENT l:book (l:author+,l:title,l:year?,l:publisher?,l:location?)>
    <!ELEMENT l:video (l:director,l:actor*,l:genre,l:title,l:year?,l:medium?)>
    <!ELEMENT l:audio (l:artist+,l:title,l:year?,l:medium?)>

    <!ELEMENT l:author (#PCDATA)>
    <!ELEMENT l:title (#PCDATA)>
    <!ELEMENT l:year (#PCDATA)>
    <!ELEMENT l:publisher (#PCDATA)>
    <!ELEMENT l:location (#PCDATA)>
    <!ELEMENT l:director (#PCDATA)>
    <!ELEMENT l:actor (#PCDATA)>
    <!ELEMENT l:genre (#PCDATA)>
    <!ELEMENT l:medium (#PCDATA)>
    <!ELEMENT l:artist (#PCDATA)>

  >>;;

(* Create the DTD object: *)
let library_dtd = 
  Pxp_dtd_parser.parse_dtd_entity config (from_string library_dtd_string) ;;

(* The catalog defines the meaning of the SYSTEM IDs: *)
let catalog = 
  new Pxp_reader.lookup_system_id_as_string
    [ "http://sample/library/dtd", library_dtd_string ] ;;

(* Parse library data:
 *
 * We decide to read the data into O'Caml values first, and to transform
 * them in a second step. Alternatively, it is also possible to carry
 * out the transformation in only one step, but in general I do not
 * recommend this.
 *)

let parse_library src =
  (* Parse the library data found in [src]. The data are immediately
   * validated. Note, however, that we do not know yet which DTD was
   * used for validation, and whether the right root element was
   * used. These checks must be programmed manually.
   *
   * The XML tree is passed over to [factory]
   *)
  let spec = Pxp_tree_parser.default_namespace_spec in
  let lib = Pxp_tree_parser.parse_document_entity config src spec in

  (* Check whether [lib] refers to the right DTD: *)
  ( match lib # dtd # id with
	Some(External(System "http://sample/library/dtd")) ->
	  () (* accepted *)
      | Some(External _) ->
	  failwith "Library data file refers to the wrong DTD"
      | Some(Derived _) ->
	  failwith "Library data file refers to the wrong DTD or extends the DTD"
      | Some Internal ->
	  failwith "Library data file must refer to external DTD"
      | None ->
	  failwith "DTD name completely missing in library data file"
  );

  (* Check whether the root element is acceptable: For this test,
   * we take the name of the real root element, because the namespace
   * has been processed, and not the name declared in the DTD.
   *)
  ( match lib # root # node_type with
	T_element "l:library" ->
	  () (* accepted *)
      | _ ->
	  failwith "Wrong root element"
  );

  lib
;;


(* Internal representation of the library data: 
 *
 * The objects are created by
 *   [ new <class> xml]
 * where [xml] is the corresponding part of the XML tree.
 *
 * The following classes are later extended by the method [transform]
 * which is only declared for now.
 *)

class virtual ['ext] book (xml : 'ext node) = object
  val mutable authors = []
  val mutable title = ""
  val mutable year = None
  val mutable publisher = None
  val mutable location = None

  initializer (
    (* Fill the instance variables from the XML tree [xml]: 
     * The first method is to scan the children, and to match the element
     * names.
     *)
    assert(xml # node_type = T_element "l:book");  (* assumption *)
    xml # iter_nodes
      (fun child ->
	 match child # node_type with
	     T_element "l:author" ->
	       authors <- authors @ [ child # data ]
	   | T_element "l:title" ->
	       title <- child # data
	   | T_element "l:year" ->
	       year <- Some (child # data)
	   | T_element "l:publisher" ->
	       publisher <- Some (child # data)
	   | T_element "l:location" ->
	       location <- Some (child # data)
	   | _ ->
	       assert false
	       (* This cannot happen because the tree is validated *)
      );
  )

  method virtual transform : unit -> 'ext node
end


class virtual ['ext] video (xml : 'ext node) = object
  val mutable director = ""
  val mutable actors = []
  val mutable genre = ""
  val mutable title = ""
  val mutable year = None
  val mutable medium = None

  initializer (
    (* Fill the instance variables from the XML tree [xml]: 
     * The second method is a stream parser. It is a bit overkill for this
     * purpose, but a very good choice for complex structures.
     *)
    assert(xml # node_type = T_element "l:video");  (* assumption *)

    (* Note that this grammar performs the actions from right to left! *)
    let rec parse_node =
      parser
	  [< '(E_start_tag("l:video",_,_,_));
	     _ = parse_children;
	     '(E_end_tag(_,_));
	     (* 'E_end_of_stream  *)
	  >] -> ()

    and parse_children =
      parser
	  [< '(E_start_tag("l:director",_,_,_));
	     data = parse_optional_char_data;
	     '(E_end_tag(_,_));
	     rest = parse_children;
	  >] ->
	    director <- data
	| [< '(E_start_tag("l:actor",_,_,_));
	     data = parse_optional_char_data;
	     '(E_end_tag(_,_));
	     rest = parse_children;
	  >] ->
	    actors <- data :: actors
	| [< '(E_start_tag("l:genre",_,_,_));
	     data = parse_optional_char_data;
	     '(E_end_tag(_,_));
	     rest = parse_children;
	  >] ->
	    genre <- data
	| [< '(E_start_tag("l:title",_,_,_));
	     data = parse_optional_char_data;
	     '(E_end_tag(_,_));
	     rest = parse_children;
	  >] ->
	    title <- data
	| [< '(E_start_tag("l:year",_,_,_));
	     data = parse_optional_char_data;
	     '(E_end_tag(_,_));
	     rest = parse_children;
	  >] ->
	    year <- Some data
	| [< '(E_start_tag("l:medium",_,_,_));
	     data = parse_optional_char_data;
	     '(E_end_tag(_,_));
	     rest = parse_children;
	  >] ->
	    medium <- Some data
        | [< >] ->
	    ()

    and parse_optional_char_data =
      parser
	  [< 'E_char_data data >] -> data
	| [< >]                   -> ""

    in
    parse_node (Stream.from (liquefy ~omit_positions:true (`Node xml)))
  )

  method virtual transform : unit -> 'ext node
end


class virtual ['ext] audio (xml : 'ext node) = object
  val mutable artists = []
  val mutable title = ""
  val mutable year = None
  val mutable medium = None

  initializer (
    (* Fill the instance variables from the XML tree [xml]: 
     * The third method is similar to the first, but uses an
     * alist instead of direct pattern matching.
     *)
    assert(xml # node_type = T_element "l:audio");  (* assumption *)
    let actions =
      [ "l:artist", (fun data ->  artists <- artists @ [data]);
	"l:title",  (fun data ->  title <- data);
	"l:year",   (fun data ->  year <- Some data);
	"l:medium", (fun data ->  medium <- Some data)
      ] in

    xml # iter_nodes
      (fun child ->
	 match child # node_type with
	     T_element name ->
	       let action =
		 try List.assoc name actions
		 with Not_found -> assert false in
	       action child#data
	   | _ ->
	       assert false
	       (* This cannot happen because the tree is validated *)
      );
  )

  method virtual transform : unit -> 'ext node
end


class virtual ['ext] library (xml : 'ext node) factory = object
  val mutable items = []

  initializer (
    assert(xml # node_type = T_element "l:library");  (* assumption *)
    xml # iter_nodes
      (fun child ->
	 match child # node_type with
	     T_element name ->
	       let creator = 
		 try List.assoc name factory
		 with Not_found -> assert false in
	       let item = creator child in
	       items <- items @ [item]
	   | _ ->
	       assert false
      )
  )

  method virtual transform : unit -> 'ext node
end


(* Transformation of libary data to XHTML:
 *
 * These classes extend the former by the missing [transform]
 * method. This method returns XHTML trees.
 *)

class ['ext] book_xhtml dtd xml = object
  inherit ['ext] book xml

  method transform() =
    (* Verbose style: Every field is transformed by its own
     * rule.
     *)
    let spec = Pxp_tree_parser.default_namespace_spec in
    (fun ~year_nodes ~publisher_nodes ~location_nodes ->
      <:pxp_tree<
	<:scope>
	  <h:table>
	    ( [ <h:tr> <h:th colspan="2" class="category"> "Book"
                <h:tr> [ <h:td class="label">"Authors:"
		         <h:td class="data">
			   <*>(: String.concat ", " authors :)
		     ]
		<h:tr> [ <h:td class="label">"Title:"
		         <h:td class="data"><*>title
		     ]
	      ]
	      @ year_nodes
	      @ publisher_nodes
	      @ location_nodes
	    )
      >>
    )
    ~year_nodes:(
      match year with
	  Some y ->
	    <:pxp_tree<
	      [ <:scope>
		  <h:tr> [ <h:td class="label">"Year:"
			   <h:td class="data"><*>y
			 ]
	      ]
            >>
	| None ->
	    []
    )
    ~publisher_nodes:(
      match publisher with
	  Some p ->
	    <:pxp_tree<
	      [ <:scope>
                  <h:tr> [ <h:td class="label">"Published by:"
			   <h:td class="data"><*>p
		         ]
	      ]
            >>
	| None ->
	    []
    )
    ~location_nodes:(
      match location with
	  Some l ->
	    <:pxp_tree<
	      [ <:scope>
                  <h:tr> [ <h:td class="label">"Location:"
			   <h:td class="data"><*>l
		         ]
	      ]
            >>
	| None ->
	    []
    )

end ;;

				 
class ['ext] video_xhtml dtd xml = object
  inherit ['ext] video xml

  (* When all fields can be transformed in the same way, this method
   * is much more effective. The field transformation has been factored
   * out, and is now its own function.
   *)

  method transform() =
    let spec = Pxp_tree_parser.default_namespace_spec in
    let fields =
      ("Director", director) ::
      ("Actors", String.concat ", " actors) ::
      ("Genre", genre) ::
      ("Title", title) ::
      ( ( match year with
	      Some y -> [ "Year", y ]
	    | None   -> []
	) @
	( match medium with
	      Some m -> [ "Medium", m ]
	    | None   -> []
	)
      ) in

    let transform_field (label, data) =
      <:pxp_tree<
        <:scope>
          <h:tr> [ <h:td class="label"><*>label ^ ":"
                   <h:td class="data"><*>data
                 ]
      >>
    in

    <:pxp_tree<
      <:scope>
	<h:table>
          [ <h:tr> <h:th colspan="2" class="category"> "Video" ] @
          (: List.map transform_field fields :)
    >>

end ;;


class ['ext] audio_xhtml dtd xml = object
  inherit ['ext] audio xml

  (* Same method as for video *)

  method transform() =
    let spec = Pxp_tree_parser.default_namespace_spec in
    let fields =
      ("Artists", String.concat ", " artists) ::
      ("Title", title) ::
      ( ( match year with
	      Some y -> [ "Year", y ]
	    | None   -> []
	) @
	( match medium with
	      Some m -> [ "Medium", m ]
	    | None   -> []
	)
      ) in

    let transform_field (label, data) =
      <:pxp_tree<
        <:scope>
          <h:tr> [ <h:td class="label"><*>label ^ ":"
                   <h:td class="data"><*>data
                 ]
      >>
    in

    <:pxp_tree<
      <:scope>
	<h:table>
          [ <h:tr> <h:th colspan="2" class="category"> "Audio" ] @
          (: List.map transform_field fields :)
    >>

end


class ['ext] library_xhtml dtd xml = object
  inherit ['ext] library xml [ "l:book", new book_xhtml dtd;
			       "l:video", new video_xhtml dtd;
			       "l:audio", new audio_xhtml dtd ]

  method transform() =
    let spec = Pxp_tree_parser.default_namespace_spec in
    let transform_item item =
      <:pxp_tree<
        <:scope>
          <h:li>[ "Item: " (: item # transform() :) ]
      >>
    in
    <:pxp_tree<
      <:scope>
        <h:ul>
          (: List.map transform_item items :)
    >>
end


let generate_xhtml_page dtd lib =
  let spec = Pxp_tree_parser.default_namespace_spec in
  let css =
    <:pxp_text<
.category {
  text-align: left;
  font-weight: bold;
  background-color: cyan;
}
    >>
  in
  <:pxp_tree<
    <:scope>
      <h:html>
        [ <h:head>
            [ <h:title>"My little library" 
	      <h:style type="text/css"><*>css
	    ]
          <h:body>
            [ <h:h1>"My little library"
	      (: lib # transform() :)
            ]
        ]
  >> ;;


(* Sample contents: This could also be read from an external file. *)

let library_contents =
  <:pxp_text<<?xml version="1.0" encoding="ISO-8859-1"?>

    <!DOCTYPE l:library SYSTEM "http://sample/library/dtd">

    <library xmlns="http://sample/library/ns">
      <book>
        <author>Henning Mankell</author>
        <title>Der Mann, der lächelte</title>
        <year>2003</year>
        <publisher>Deutscher Taschenbuch Verlag GmbH &amp; Co KG</publisher>
        <location>München</location>
      </book>

      <book>
        <author>Frank Schätzing</author>
        <title>Der Schwarm</title>
        <year>2004</year>
        <publisher>Verlag Kiepenheuer &amp; Witsch</publisher>
        <location>Köln</location>
      </book>

      <video>
        <director>James Cameron</director>
        <actor>Leonardo DiCaprio</actor>
        <actor>Kate Winslet</actor>
        <actor>Billy Zane</actor>
        <actor>Kathy Bates</actor>
        <actor>Frances Fisher</actor>
        <actor>Bernard Hill</actor>
        <actor>Jonathan Hyde</actor>
        <actor>Danny Nucci</actor>
        <actor>Gloria Stuart</actor>
        <actor>David Warner</actor>
        <actor>Victor Garber</actor>
        <actor>Bill Paxton</actor>
        <genre>Epos</genre>
        <title>Titanic</title>
	<year>1997</year>
        <medium>DVD</medium>
      </video>

      <video>
        <director>Michael Bully Herbig</director>
	<actor>Michael Bully Herbig</actor>
        <actor>Christian Tramitz</actor>
        <actor>Marie Bäumer</actor>
        <actor>Hilmi Sözer</actor>
        <actor>Rick Kavanian</actor>
        <actor>Sky Du Mont</actor>
        <genre>Comedy</genre>
        <title>Der Schuh des Manitu Extra Large</title>
	<year>2003</year>
        <medium>DVD</medium>
      </video>

      <audio>
        <artist>Pet Shop Boys</artist>
        <title>actually</title>
        <year>1987</year>
        <medium>CD</medium>
      </audio>

    </library>
  >>;;


let main () =
  let cgi =
    new std_activation ~operating_type:buffered_transactional_optype () in

  try
    let dtd =
      Pxp_dtd_parser.create_empty_dtd config in

    dtd # set_namespace_manager mng;

    let src =
      from_string ~alt:[ catalog ] library_contents in
    
    let page =
      generate_xhtml_page
	dtd
	(new library_xhtml
	   dtd
	   (parse_library src) # root) in
    
    cgi # set_header
      ~content_type:"application/xhtml+xml; charset=UTF-8"
      ();

    cgi # output # output_string "<?xml version='1.0' encoding='UTF-8'?>\n";
    page # display 
      (`Out_netchannel (cgi#output :> Netchannels.out_obj_channel)) `Enc_utf8;
    cgi # output # commit_work()
  with
      error ->
        cgi # output # rollback_work();
        cgi # set_header
          ~status:`Forbidden                  (* Indicate the error *)
          ~cache:`No_cache
          ~content_type:"text/plain; charset=ISO-8859-1"
          ();
        cgi # output # output_string "Software error:\n\n";
	cgi # output # output_string (string_of_exn error);
	cgi # output # commit_work()
;;


try
  main()
with
    error ->
      prerr_endline (string_of_exn error);
      exit 1
;;

