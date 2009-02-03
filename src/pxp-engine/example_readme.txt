{1 The [readme] processor}

{fixpxpcoretypes true}

The task of the [readme] processor is to convert a document conforming
to the XML DTD "readme.dtd" into an HTML document or a text document.
This example especially demonstrates how to use node extensions to add
custom methods to nodes (see {!Intro_extensions}), and how to use the
object-oriented feature of late binding so that every node type
behaves differently.

Note that the converter code dates back from 1999. Nowadays I would
probably have written it as a purely functional transformer. This
task is now left to the reader...

{2 The [readme] DTD}

The motivation for [readme] was that I often wrote two versions of
files such as README and INSTALL explaining aspects of a distributed
software archive; one version was ASCII-formatted, the other was
written in HTML. Maintaining both versions meant double amount of
work, and changes of one version could be forgotten in the other
version. To improve this situation I invented the [readme] DTD which
allows me to maintain only one source written as XML document, and to
generate both the ASCII and the HTML version from it

In this section, I explain only the DTD. The [readme] DTD is
contained in the PXP distribution together with the two converters to
produce ASCII and HTML.

The documents have a simple structure: There are up to three levels of nested
sections, paragraphs, item lists, footnotes, hyperlinks, and text emphasis. The
outermost element has usually the type [readme], it is
declared by

{[
<!ELEMENT readme (sect1+)>
<!ATTLIST readme
          title CDATA #REQUIRED>
]}

This means that this element contains one or more sections of the first level
(element type [sect1]), and that the element has a required
attribute [title] containing character data ([CDATA]). Note that
[readme] elements must not contain text data.

The three levels of sections are declared as follows:

{[
<!ELEMENT sect1 (title,(sect2|p|ul)+)>

<!ELEMENT sect2 (title,(sect3|p|ul)+)>

<!ELEMENT sect3 (title,(p|ul)+)>
]}

Every section has a [title] element as first subelement. After
the title an arbitrary but non-empty sequence of inner sections, paragraphs and
item lists follows. Note that the inner sections must belong to the next higher
section level; [sect3] elements must not contain inner
sections because there is no next higher level.

Obviously, all three declarations allow paragraphs ([p]) and item
lists ([ul]). The definition can be simplified at this point by using
a parameter entity:

{[
<!ENTITY % p.like "p|ul">

<!ELEMENT sect1 (title,(sect2|%p.like;)+)>

<!ELEMENT sect2 (title,(sect3|%p.like;)+)>

<!ELEMENT sect3 (title,(%p.like;)+)>
]}

Here, the entity [p.like] is nothing but a macro abbreviating the same
sequence of declarations; if new elements on the same level as [p] and
[ul] are later added, it is sufficient only to change the entity
definition. Note that there are some restrictions on the usage of
entities in this context; most important, entities containing a left
parenthesis must also contain the corresponding right parenthesis.

Note that the entity [p.like] is a {b parameter} entity, i.e. the
[ENTITY] declaration contains a percent sign, and the entity is
referred to by [%p.like;]. This kind of entity must be used to
abbreviate parts of the DTD; the {b general} entities declared without
percent sign and referred to as [&amp;name;] are not allowed in this
context.

The [title] element specifies the title of the section in
which it occurs. The title is given as character data, optionally interspersed
with line breaks ([br]):

{[
<!ELEMENT title (#PCDATA|br)*>
]}

Compared with the [title] {b attribute} of the [readme] element, this
element allows inner markup (i.e. [br]) while attribute values do not:
It is an error if an attribute value contains the left angle bracket <
literally such that it is impossible to include inner elements.

The paragraph element [p] has a structure similar to
[title], but it allows more inner elements:

{[
<!ENTITY % text "br|code|em|footnote|a">

<!ELEMENT p (#PCDATA|%text;)*>
]}

Line breaks do not have inner structure, so they are declared as being empty:

{[
<!ELEMENT br EMPTY>
]}

This means that really nothing is allowed within [br]; you must always
write [<br></br>] or abbreviated [<br/>].

Code samples should be marked up by the [code] tag; emphasized
text can be indicated by [em]:

{[
<!ELEMENT code (#PCDATA)>

<!ELEMENT em (#PCDATA|%text;)*>
]}

That [code] elements are not allowed to contain further markup
while [em] elements do is a design decision by the author of
the DTD.

Unordered lists simply consists of one or more list items, and a list item may
contain paragraph-level material:

{[
<!ELEMENT ul (li+)>

<!ELEMENT li (%p.like;)*>
]}

Footnotes are described by the text of the note; this text may contain
text-level markup. There is no mechanism to describe the numbering scheme of
footnotes, or to specify how footnote references are printed.

{[
<!ELEMENT footnote (#PCDATA|%text;)*>
]}

Hyperlinks are written as in HTML. The anchor tag contains the text
describing where the link points to, and the [href] attribute is the
pointer (as URL). There is no way to describe locations of "hash
marks". If the link refers to another [readme] document, the attribute
[readmeref] should be used instead of [href].  The reason is that the
converted document has usually a different system identifier (file
name), and the link to a converted document must be converted, too.

{[
<!ELEMENT a (#PCDATA)*>
<!ATTLIST a 
          href      CDATA #IMPLIED
          readmeref CDATA #IMPLIED
>
]}

Note that although it is only sensible to specify one of the two attributes,
the DTD has no means to express this restriction.

So far the DTD. Finally, here is a sample document for it:

{[
<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE readme SYSTEM "readme.dtd">
<readme title="How to use the readme converters">
  <sect1>
    <title>Usage</title>
    <p>
      The <em>readme</em> converter is invoked on the command line by:
    </p>
    <p>
      <code>readme [ -text | -html ] input.xml</code>
    </p>
    <p>
      Here is a list of options:
    </p>
    <ul>
      <li>
	<p>
          <code>-text</code>: 
          specifies that ASCII output should be produced</p>
      </li>
      <li>
	<p>
           <code>-html</code>: 
           specifies that HTML output should be produced</p>
      </li>
    </ul>
    <p>
      The input file must be given on the command line. The converted output is
      printed to <em>stdout</em>.
    </p>
  </sect1>
  <sect1>
    <title>Author</title>
    <p>
      The program has been written by
      <a href="mailto:gerd@gerd-stolpmann.de">Gerd Stolpmann</a>.
    </p>
  </sect1>
</readme>
]}


{2 The [readme] converter to HTML}

The converter from [readme] documents to HTML documents follows
strictly the approach to define one extension class per element
type. The generated HTML code is structurally similar to the [readme]
source, because of this most elements can be converted in the
following straigh-forward way: Given the input element

[<e>content</e>]

the conversion text is the concatenation of a computed prefix, the
recursively converted content, and a computed suffix.

Only one element type cannot be handled by this scheme:
[footnote]. Footnotes are collected while they are found in the input
text, and they are printed after the main text has been converted and
printed.

Now we comment to source code of the [to_html.ml] converter.

{3 Header}

{[
open Pxp_types
open Pxp_document
open Pxp_dtd.Entity
]}

{3 Type declarations}

{[
class type footnote_printer =
  object
    method footnote_to_html : store_type -> out_channel -> unit
  end

and store_type =
  object
    method alloc_footnote : footnote_printer -> int
    method print_footnotes : out_channel -> unit
  end
]}

For comments see the implementations below.

{3 The class [store] of type [store_type] }

The [store] is a container for footnotes. You can add a footnote by
invoking [alloc_footnote]; the argument is an object of the class
[footnote_printer], the method returns the number of the footnote. The
interesting property of a footnote is that it can be converted to
HTML, so a [footnote_printer] is an object with a method
[footnote_to_html]. The class [footnote] which is defined below has a
compatible method [footnote_to_html] such that objects created from it
can be used as [footnote_printer]s.

The other method, [print_footnotes] prints the footnotes as
definition list, and is typically invoked after the main material of the page
has already been printed. Every item of the list is printed by
[footnote_to_html].

{[
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
]}


{3 The function [escape_html]}

This function converts the characters <, >, &, and " to their HTML
representations. For example, 

{[ escape_html "<>" = "&lt;&gt;" ]}

Other characters are left unchanged.

{[
let escape_html s =
  Str.global_substitute
    (Str.regexp "<\\|>\\|&\\|\"\\|@\\|:")
    (fun s ->
      match Str.matched_string s with
        "<" -> "&lt;"
      | ">" -> "&gt;"
      | "&" -> "&amp;"
      | "\"" -> "&quot;"
      | "@" -> "&#64;"
      | ":" -> "&#58;"
      | _ -> assert false)
    s
]}

Note (of 2009): There is also the Ocamlnet function
[Netencoding.Html.encode] one can use. It has a special XML mode.

{3 The virtual class [shared]}

This virtual class is the abstract superclass of the extension classes
shown below. It defines the standard methods [clone], [node], and
[set_node], and declares the type of the virtual method
[to_html]. This method recursively traverses the whole element tree,
and prints the converted HTML code to the output channel passed as
second argument. The first argument is the reference to the global
[store] object which collects the footnotes.

{[
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
]}

For an introduction into extension classes see {!Intro_extensions}.

{3 The class [only_data]}

This class defines [to_html] such that the character data of the
current node is converted to HTML. Note that [self] is an extension
object (of type {!Pxp_document.extension}), [self # node] is the node
object (of type {!Pxp_document.node}), and [self # node # data]
returns the character data of the node (see
{!Pxp_document.node.data}).

{[
class only_data =
  object (self)
    inherit shared

    method to_html store ch =
      output_string ch (escape_html (self # node # data))
  end
]}


{3 The class [readme]}

This class converts elements of type [readme] to HTML. Such an
element is (by definition) always the root element of the document. First, the
HTML header is printed; the [title] attribute of the element
determines the title of the HTML page. Some aspects of the HTML page can be
configured by setting certain parameter entities, for example the background
color, the text color, and link colors. After the header, the
[body] tag, and the headline have been printed, the contents
of the page are converted by invoking [to_html] on all
children of the current node (which is the root node). Then, the footnotes are
appended to this by telling the global [store] object to print
the footnotes. Finally, the end tags of the HTML pages are printed.

{[
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
      let html_header =
        try replacement_text
            (self # node # dtd # par_entity "readme:html:header") 
        with WF_error _ -> "" in
      let html_trailer =
        try replacement_text
            (self # node # dtd # par_entity "readme:html:trailer")
        with WF_error _ -> "" in
      let html_bgcolor =
        try replacement_text
            (self # node # dtd # par_entity "readme:html:bgcolor")
        with WF_error _ -> "white" in
      let html_textcolor =
        try replacement_text
            (self # node # dtd # par_entity "readme:html:textcolor")
        with WF_error _ -> "" in
      let html_alinkcolor =
        try replacement_text
            (self # node # dtd # par_entity "readme:html:alinkcolor")
        with WF_error _ -> "" in
      let html_vlinkcolor =
        try replacement_text
            (self # node # dtd # par_entity "readme:html:vlinkcolor")
        with WF_error _ -> "" in
      let html_linkcolor =
        try replacement_text 
            (self # node # dtd # par_entity "readme:html:linkcolor")
        with WF_error _ -> "" in
      let html_background =
        try replacement_text
            (self # node # dtd # par_entity "readme:html:background")
        with WF_error _ -> "" in

      output_string ch "<html><header><title>\n";
      output_string ch (escape_html title);
      output_string ch "</title></header>\n";
      output_string ch "<body ";
      List.iter
        (fun (name,value) ->
           if value <> "" then 
             output_string ch (name ^ "=\"" ^ escape_html value ^ "\" "))
        [ "bgcolor",    html_bgcolor;
          "text",       html_textcolor;
          "link",       html_linkcolor;
          "alink",      html_alinkcolor;
          "vlink",      html_vlinkcolor;
        ];
      output_string ch ">\n";
      output_string ch html_header;
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
      output_string ch html_trailer;
      output_string ch "</html>\n";
  end
]}

This class is an example how to access the value of an attribute: The
value is determined by invoking [self # node # attribute "title"] (see
{!Pxp_document.node.attribute}). As this attribute has been declared
as CDATA and as being required, the value has always the form [Value
s] where [s] is the string value of the attribute. Attribute values
have type {!Pxp_core_types.S.att_value}.

You can also see how entity contents can be accessed. A parameter
entity object can be looked up by [self # node # dtd # par_entity
"name"] (see {!Pxp_dtd.dtd.par_entity}), and by invoking
{!Pxp_dtd.Entity.replacement_text} the value of the entity is returned
after inner parameter and character entities have been processed. Note
that you must use {!Pxp_dtd.dtd.gen_entity} instead of [par_entity] to
access general entities.


{3 The classes [section], [sect1], [sect2], and [sect3]}

As the conversion process is very similar, the conversion classes of the three
section levels are derived from the more general [section]
class. The HTML code of the section levels only differs in the type of the
headline, and because of this the classes describing the section levels can be
computed by replacing the class argument [the_tag] of
[section] by the HTML name of the headline tag.

{[
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

class sect1 = section "h1"
class sect2 = section "h3"
class sect3 = section "h4"
]}

Section elements are converted to HTML by printing a headline and then
converting the contents of the element recursively. More precisely,
the first sub-element is always a [title] element, and the other
elements are the contents of the section. This structure is declared
in the DTD, and it is guaranteed that the document matches the
DTD. Because of this the title node can be separated from the rest
without any checks.

Both the title node, and the body nodes are then converted to HTML by
calling [to_html] on them.

{3 The classes [map_tag], [p], [em], [ul], and [li] }

Several element types are converted to HTML by simply mapping them to
corresponding HTML element types. The class [map_tag] implements this,
and the class argument [the_target_tag] determines the tag name to map
to. The output consists of the start tag, the recursively converted
inner elements, and the end tag.

{[
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

class p = map_tag "p"
class em = map_tag "b"
class ul = map_tag "ul"
class li = map_tag "li"
]}

{3 The class [br]}

Element of type [br] are mapped to the same HTML type. Note that HTML
forbids the end tag of [br].

{[
class br =
  object (self)
    inherit shared

    method to_html store ch =
      output_string ch "<br>\n";
      List.iter
        (fun n -> n # extension # to_html store ch)
        (self # node # sub_nodes);
  end
]}


{3 The class [code]}

The [code] type is converted to a [pre] section (preformatted
text). As the meaning of tabs is unspecified in HTML, tabs are
expanded to spaces.

{[
class code =
  object (self)
    inherit shared

    method to_html store ch =
      let data = self # node # data in
      (* convert tabs *)
      let l = String.length data in
      let rec preprocess i column =
        (* this is very ineffective but comprehensible: *)
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
]}


{3 The class [a]}

Hyperlinks, expressed by the [a] element type, are converted to the
HTML [a] type. If the target of the hyperlink is given by [href], the
URL of this attribute can be used directly. Alternatively, the target
can be given by [readmeref] in which case the ".html" suffix must be
added to the file name.

Note that within [a] only #PCDATA is allowed, so the contents can be
converted directly by applying [escape_html] to the character data
contents.

{[
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
]}


{3 The class [footnote]}

The [footnote] class has two methods: [to_html] to convert the
footnote reference to HTML, and [footnote_to_html] to convert the
footnote text itself.

The footnote reference is converted to a local hyperlink; more
precisely, to two anchor tags which are connected with each other. The
text anchor points to the footnote anchor, and the footnote anchor
points to the text anchor.

The footnote must be allocated in the [store] object. By allocating
the footnote, you get the number of the footnote, and the text of the
footnote is stored until the end of the HTML page is reached when the
footnotes can be printed. The [to_html] method stores simply the
object itself, such that the [footnote_to_html] method is invoked on
the same object that encountered the footnote.

The [to_html] method only allocates the footnote, and prints the
reference anchor, but it does not print nor convert the contents of the
note. This is deferred until the footnotes actually get printed, i.e. the
recursive call of [to_html] on the sub nodes is done by
[footnote_to_html]

Note that this technique does not work if you make another footnote
within a footnote; the second footnote gets allocated but not printed.

{[
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
]}


{3 The specification of the document model}

This code sets up the hash table that connects element types with the
exemplars of the extension classes that convert the elements to HTML.
See {!Intro_extensions.bindext} for comments, and 
{!Pxp_document.make_spec_from_alist} for the function definition.

{[
let tag_map =
  make_spec_from_alist
    ~data_exemplar:(new data_impl (new only_data))
    ~default_element_exemplar:(new element_impl (new no_markup))
    ~element_alist:
      [ "readme", (new element_impl (new readme));
        "sect1",  (new element_impl (new sect1));
        "sect2",  (new element_impl (new sect2));
        "sect3",  (new element_impl (new sect3));
        "title",  (new element_impl (new no_markup));
        "p",      (new element_impl (new p));
        "br",     (new element_impl (new br));
        "code",   (new element_impl (new code));
        "em",     (new element_impl (new em));
        "ul",     (new element_impl (new ul));
        "li",     (new element_impl (new li));
        "footnote", (new element_impl (new footnote : #shared :> shared));
        "a",      (new element_impl (new a));
      ]
    ()
]}


{2 The [readme] converter to ASCII}

This converter is quite similar to the HTML converter, and not presented
here. The source code is part of the PXP source tarball, however.


{2 The main program}

{[
open Pxp_types
open Pxp_document
open Pxp_tree_parser

let rec print_error e =
  prerr_endline(string_of_exn e)


let run f a =
  try f a with
      e -> print_error e


let convert_to_html filename =
  let document =
    parse_document_entity
      { default_config with encoding = `Enc_iso88591 }
      (from_file filename)
      To_html.tag_map
  in
  let root = document # root in
  let store = new To_html.store in
  root # extension # to_html store stdout


let convert_to_text filename =
  let document =
    parse_document_entity
      default_config
      (from_file filename)
      To_text.tag_map
  in
  let root = document # root in
  let store = new To_text.store in
  let box = new To_text.box 79 79 in
  root # extension # to_box store box;
  box # output 0 0 stdout


let main() =
  let want_html = ref false in
  let want_text = ref false in
  let filename = ref None in
  Arg.parse
      [ "-html", Arg.Set want_html, 
	      "  convert file to html";
	"-text", Arg.Set want_text,
	      "  convert file to text";
      ]
      (fun s -> 
	 match !filename with
	     None -> filename := Some s
	   | Some _ ->
	       raise (Arg.Bad "Multiple arguments not allowed."))
      "usage: readme [ -text | -html ] input.xml >output";
  let fn =
    match !filename with
	None -> 
	  prerr_endline "readme: no input";
	  exit 1
      | Some s -> s
  in
  match !want_html, !want_text with
      true, false ->
	run convert_to_html fn
    | false, true ->
	run convert_to_text fn
    | _ ->
	prerr_endline ("readme: Please select exactly one output format")

let () =
  main()
]}

{fixpxpcoretypes false}
