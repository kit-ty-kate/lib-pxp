(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


(* Convert XHTML-1.0 document into HTML-4.01 document. The input is parsed
 * in well-formedness mode, and is not validated. The input should have one
 * of these DOCTYPEs:
 * - PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
 * - PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
 * - PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN"
 *
 * The input must declare the HTML namespace as http://www.w3.org/1999/xhtml.
 *
 * The XHTML entity sets (Latin 1, Special, Symbol) can be assumed.
 * In output, always numeric entities are printed.
 *
 * Processing instructions and comments are removed from the document.
 *
 * Output encoding is 7 bit ASCII.
 *
 * TODO:
 * - id/name: These attributes are not yet checked, but simply printed as
 *   found in the input
 * - XML stylesheets are not yet transformed to HTML stylesheets
 *   (conversion of <?xml-stylesheet ...?> to <style ...>)
 *)

open Pxp_yacc;;
open Pxp_types;;
open Pxp_document;;

type html_doctype =
  [ `Frameset
  | `Transitional
  | `Strict
  ]
;;


(**********************************************************************)
(* XHTML entities                                                     *)
(**********************************************************************)

let xhtml_lat1_ent = "<?xml version='1.0' encoding='UTF-8'?>
<!ENTITY nbsp   '&#160;'>
<!ENTITY iexcl  '&#161;'>
<!ENTITY cent   '&#162;'>
<!ENTITY pound  '&#163;'>
<!ENTITY curren '&#164;'>
<!ENTITY yen    '&#165;'>
<!ENTITY brvbar '&#166;'>
<!ENTITY sect   '&#167;'>
<!ENTITY uml    '&#168;'>
<!ENTITY copy   '&#169;'>
<!ENTITY ordf   '&#170;'>
<!ENTITY laquo  '&#171;'>
<!ENTITY not    '&#172;'>
<!ENTITY shy    '&#173;'>
<!ENTITY reg    '&#174;'>
<!ENTITY macr   '&#175;'>
<!ENTITY deg    '&#176;'>
<!ENTITY plusmn '&#177;'>
<!ENTITY sup2   '&#178;'>
<!ENTITY sup3   '&#179;'>
<!ENTITY acute  '&#180;'>
<!ENTITY micro  '&#181;'>
<!ENTITY para   '&#182;'>
<!ENTITY middot '&#183;'>
<!ENTITY cedil  '&#184;'>
<!ENTITY sup1   '&#185;'>
<!ENTITY ordm   '&#186;'>
<!ENTITY raquo  '&#187;'>
<!ENTITY frac14 '&#188;'>
<!ENTITY frac12 '&#189;'>
<!ENTITY frac34 '&#190;'>
<!ENTITY iquest '&#191;'>
<!ENTITY Agrave '&#192;'>
<!ENTITY Aacute '&#193;'>
<!ENTITY Acirc  '&#194;'>
<!ENTITY Atilde '&#195;'>
<!ENTITY Auml   '&#196;'>
<!ENTITY Aring  '&#197;'>
<!ENTITY AElig  '&#198;'>
<!ENTITY Ccedil '&#199;'>
<!ENTITY Egrave '&#200;'>
<!ENTITY Eacute '&#201;'>
<!ENTITY Ecirc  '&#202;'>
<!ENTITY Euml   '&#203;'>
<!ENTITY Igrave '&#204;'>
<!ENTITY Iacute '&#205;'>
<!ENTITY Icirc  '&#206;'>
<!ENTITY Iuml   '&#207;'>
<!ENTITY ETH    '&#208;'>
<!ENTITY Ntilde '&#209;'>
<!ENTITY Ograve '&#210;'>
<!ENTITY Oacute '&#211;'>
<!ENTITY Ocirc  '&#212;'>
<!ENTITY Otilde '&#213;'>
<!ENTITY Ouml   '&#214;'>
<!ENTITY times  '&#215;'>
<!ENTITY Oslash '&#216;'>
<!ENTITY Ugrave '&#217;'>
<!ENTITY Uacute '&#218;'>
<!ENTITY Ucirc  '&#219;'>
<!ENTITY Uuml   '&#220;'>
<!ENTITY Yacute '&#221;'>
<!ENTITY THORN  '&#222;'>
<!ENTITY szlig  '&#223;'>
<!ENTITY agrave '&#224;'>
<!ENTITY aacute '&#225;'>
<!ENTITY acirc  '&#226;'>
<!ENTITY atilde '&#227;'>
<!ENTITY auml   '&#228;'>
<!ENTITY aring  '&#229;'>
<!ENTITY aelig  '&#230;'>
<!ENTITY ccedil '&#231;'>
<!ENTITY egrave '&#232;'>
<!ENTITY eacute '&#233;'>
<!ENTITY ecirc  '&#234;'>
<!ENTITY euml   '&#235;'>
<!ENTITY igrave '&#236;'>
<!ENTITY iacute '&#237;'>
<!ENTITY icirc  '&#238;'>
<!ENTITY iuml   '&#239;'>
<!ENTITY eth    '&#240;'>
<!ENTITY ntilde '&#241;'>
<!ENTITY ograve '&#242;'>
<!ENTITY oacute '&#243;'>
<!ENTITY ocirc  '&#244;'>
<!ENTITY otilde '&#245;'>
<!ENTITY ouml   '&#246;'>
<!ENTITY divide '&#247;'>
<!ENTITY oslash '&#248;'>
<!ENTITY ugrave '&#249;'>
<!ENTITY uacute '&#250;'>
<!ENTITY ucirc  '&#251;'>
<!ENTITY uuml   '&#252;'>
<!ENTITY yacute '&#253;'>
<!ENTITY thorn  '&#254;'>
<!ENTITY yuml   '&#255;'>"
;;


let xhtml_special_ent = "<?xml version='1.0' encoding='UTF-8'?>
<!ENTITY OElig   '&#338;'>
<!ENTITY oelig   '&#339;'>
<!ENTITY Scaron  '&#352;'>
<!ENTITY scaron  '&#353;'>
<!ENTITY Yuml    '&#376;'>
<!ENTITY circ    '&#710;'>
<!ENTITY tilde   '&#732;'>
<!ENTITY ensp    '&#8194;'>
<!ENTITY emsp    '&#8195;'>
<!ENTITY thinsp  '&#8201;'>
<!ENTITY zwnj    '&#8204;'>
<!ENTITY zwj     '&#8205;'>
<!ENTITY lrm     '&#8206;'>
<!ENTITY rlm     '&#8207;'>
<!ENTITY ndash   '&#8211;'>
<!ENTITY mdash   '&#8212;'>
<!ENTITY lsquo   '&#8216;'>
<!ENTITY rsquo   '&#8217;'>
<!ENTITY sbquo   '&#8218;'>
<!ENTITY ldquo   '&#8220;'>
<!ENTITY rdquo   '&#8221;'>
<!ENTITY bdquo   '&#8222;'>
<!ENTITY dagger  '&#8224;'>
<!ENTITY Dagger  '&#8225;'>
<!ENTITY permil  '&#8240;'>
<!ENTITY lsaquo  '&#8249;'>
<!ENTITY rsaquo  '&#8250;'>
<!ENTITY euro   '&#8364;'>"
;;


let xhtml_symbol_ent = "<?xml version='1.0' encoding='UTF-8'?>
<!ENTITY fnof     '&#402;'>
<!ENTITY Alpha    '&#913;'>
<!ENTITY Beta     '&#914;'>
<!ENTITY Gamma    '&#915;'>
<!ENTITY Delta    '&#916;'>
<!ENTITY Epsilon  '&#917;'>
<!ENTITY Zeta     '&#918;'>
<!ENTITY Eta      '&#919;'>
<!ENTITY Theta    '&#920;'>
<!ENTITY Iota     '&#921;'>
<!ENTITY Kappa    '&#922;'>
<!ENTITY Lambda   '&#923;'>
<!ENTITY Mu       '&#924;'>
<!ENTITY Nu       '&#925;'>
<!ENTITY Xi       '&#926;'>
<!ENTITY Omicron  '&#927;'>
<!ENTITY Pi       '&#928;'>
<!ENTITY Rho      '&#929;'>
<!ENTITY Sigma    '&#931;'>
<!ENTITY Tau      '&#932;'>
<!ENTITY Upsilon  '&#933;'>
<!ENTITY Phi      '&#934;'>
<!ENTITY Chi      '&#935;'>
<!ENTITY Psi      '&#936;'>
<!ENTITY Omega    '&#937;'>
<!ENTITY alpha    '&#945;'>
<!ENTITY beta     '&#946;'>
<!ENTITY gamma    '&#947;'>
<!ENTITY delta    '&#948;'>
<!ENTITY epsilon  '&#949;'>
<!ENTITY zeta     '&#950;'>
<!ENTITY eta      '&#951;'>
<!ENTITY theta    '&#952;'>
<!ENTITY iota     '&#953;'>
<!ENTITY kappa    '&#954;'>
<!ENTITY lambda   '&#955;'>
<!ENTITY mu       '&#956;'>
<!ENTITY nu       '&#957;'>
<!ENTITY xi       '&#958;'>
<!ENTITY omicron  '&#959;'>
<!ENTITY pi       '&#960;'>
<!ENTITY rho      '&#961;'>
<!ENTITY sigmaf   '&#962;'>
<!ENTITY sigma    '&#963;'>
<!ENTITY tau      '&#964;'>
<!ENTITY upsilon  '&#965;'>
<!ENTITY phi      '&#966;'>
<!ENTITY chi      '&#967;'>
<!ENTITY psi      '&#968;'>
<!ENTITY omega    '&#969;'>
<!ENTITY thetasym '&#977;'>
<!ENTITY upsih    '&#978;'>
<!ENTITY piv      '&#982;'>
<!ENTITY bull     '&#8226;'>
<!ENTITY hellip   '&#8230;'>
<!ENTITY prime    '&#8242;'>
<!ENTITY Prime    '&#8243;'>
<!ENTITY oline    '&#8254;'>
<!ENTITY frasl    '&#8260;'>
<!ENTITY weierp   '&#8472;'>
<!ENTITY image    '&#8465;'>
<!ENTITY real     '&#8476;'>
<!ENTITY trade    '&#8482;'>
<!ENTITY alefsym  '&#8501;'>
<!ENTITY larr     '&#8592;'>
<!ENTITY uarr     '&#8593;'>
<!ENTITY rarr     '&#8594;'>
<!ENTITY darr     '&#8595;'>
<!ENTITY harr     '&#8596;'>
<!ENTITY crarr    '&#8629;'>
<!ENTITY lArr     '&#8656;'>
<!ENTITY uArr     '&#8657;'>
<!ENTITY rArr     '&#8658;'>
<!ENTITY dArr     '&#8659;'>
<!ENTITY hArr     '&#8660;'>
<!ENTITY forall   '&#8704;'>
<!ENTITY part     '&#8706;'>
<!ENTITY exist    '&#8707;'>
<!ENTITY empty    '&#8709;'>
<!ENTITY nabla    '&#8711;'>
<!ENTITY isin     '&#8712;'>
<!ENTITY notin    '&#8713;'>
<!ENTITY ni       '&#8715;'>
<!ENTITY prod     '&#8719;'>
<!ENTITY sum      '&#8721;'>
<!ENTITY minus    '&#8722;'>
<!ENTITY lowast   '&#8727;'>
<!ENTITY radic    '&#8730;'>
<!ENTITY prop     '&#8733;'>
<!ENTITY infin    '&#8734;'>
<!ENTITY ang      '&#8736;'>
<!ENTITY and      '&#8743;'>
<!ENTITY or       '&#8744;'>
<!ENTITY cap      '&#8745;'>
<!ENTITY cup      '&#8746;'>
<!ENTITY int      '&#8747;'>
<!ENTITY there4   '&#8756;'>
<!ENTITY sim      '&#8764;'>
<!ENTITY cong     '&#8773;'>
<!ENTITY asymp    '&#8776;'>
<!ENTITY ne       '&#8800;'>
<!ENTITY equiv    '&#8801;'>
<!ENTITY le       '&#8804;'>
<!ENTITY ge       '&#8805;'>
<!ENTITY sub      '&#8834;'>
<!ENTITY sup      '&#8835;'>
<!ENTITY nsub     '&#8836;'>
<!ENTITY sube     '&#8838;'>
<!ENTITY supe     '&#8839;'>
<!ENTITY oplus    '&#8853;'>
<!ENTITY otimes   '&#8855;'>
<!ENTITY perp     '&#8869;'>
<!ENTITY sdot     '&#8901;'>
<!ENTITY lceil    '&#8968;'>
<!ENTITY rceil    '&#8969;'>
<!ENTITY lfloor   '&#8970;'>
<!ENTITY rfloor   '&#8971;'>
<!ENTITY lang     '&#9001;'>
<!ENTITY rang     '&#9002;'>
<!ENTITY loz      '&#9674;'>
<!ENTITY spades   '&#9824;'>
<!ENTITY clubs    '&#9827;'>
<!ENTITY hearts   '&#9829;'>
<!ENTITY diams    '&#9830;'>"
;;


let xhtml_doctype = "<?xml version='1.0' encoding='UTF-8'?>
<!ENTITY % HTMLlat1 PUBLIC '-//W3C//ENTITIES Latin 1 for XHTML//EN' ''>
<!ENTITY % HTMLspecial PUBLIC '-//W3C//ENTITIES Special for XHTML//EN' ''>
<!ENTITY % HTMLsymbol PUBLIC '-//W3C//ENTITIES Symbols for XHTML//EN' ''>
%HTMLlat1;
%HTMLspecial;
%HTMLsymbol;"
;;
  

let parse filename =
  let namespace_manager = new Pxp_dtd.namespace_manager in
  namespace_manager # add_namespace "html" "http://www.w3.org/1999/xhtml";
  (* Force that internally the namespace prefix "html" is used for html
   * namespace. PXP converts the namespace prefixes found in the input
   * document to "html".
   *)

  let catalog =
    Pxp_reader.lookup_public_id_as_string
      [ "-//W3C//ENTITIES Latin 1 for XHTML//EN", xhtml_lat1_ent;
	"-//W3C//ENTITIES Special for XHTML//EN", xhtml_special_ent;
	"-//W3C//ENTITIES Symbols for XHTML//EN", xhtml_symbol_ent;
	"-//W3C//DTD XHTML 1.0 Strict//EN", xhtml_doctype;
	"-//W3C//DTD XHTML 1.0 Transitional//EN", xhtml_doctype;
	"-//W3C//DTD XHTML 1.0 Frameset//EN", xhtml_doctype;
      ]
  in

  let reader = 
    new Pxp_reader.combine
      [ new Pxp_reader.resolve_as_file();
	catalog;
      ]
  in

  let config = 
    { default_namespace_config with
	encoding = `Enc_utf8;
	enable_namespace_processing = Some namespace_manager;
    } in

  let filename_url = Pxp_reader.make_file_url filename in
  let filename_url_string = Neturl.string_of_url filename_url in

  parse_wfdocument_entity
    config
    (ExtID ((System filename_url_string), reader))
    default_namespace_spec
;;


let recognize_xhtml_doctype doc =
  match doc # dtd # id with
      Some(External(Public("-//W3C//DTD XHTML 1.0 Strict//EN",_))) ->
	`Strict
    | Some(Derived(Public("-//W3C//DTD XHTML 1.0 Strict//EN",_))) ->
	`Strict
    | Some(External(Public("-//W3C//DTD XHTML 1.0 Transitional//EN",_))) ->
	`Transitional
    | Some(Derived(Public("-//W3C//DTD XHTML 1.0 Transitional//EN",_))) ->
	`Transitional
    | Some(External(Public("-//W3C//DTD XHTML 1.0 Frameset//EN",_))) ->
	`Frameset
    | Some(Derived(Public("-//W3C//DTD XHTML 1.0 Frameset//EN",_))) ->
	`Frameset
    | _ ->
	prerr_endline("Warning: DOCTYPE not recognized, assuming HTML-4.01-transitional");
	`Transitional   (* Just a default... *)
;;


let html401_doctype_string =
  [ `Frameset, 
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">";

    `Transitional,
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">";

    `Strict,
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">";
  ]
;;


let element_declared_as_empty name =
  try
    snd(List.assoc name Nethtml.html40_dtd) = `Empty
  with
      Not_found -> false
;;


let convert_and_print ?(remove = "") (doc : 'ext document) =
  (* Only "html" prefix elements are printed, other namespaces are dropped.
   * 
   *)

  let escape_html =
    let in_enc = `Enc_utf8 in
    let out_enc = `Enc_subset(`Enc_usascii, 
			      function
				  64 -> false   (* Anti-SPAM *)
				| 58 -> false   (* Anti-SPAM *)
				| _ -> true) in
    Netencoding.Html.encode ~in_enc ~out_enc ~prefer_name:false () in

  let print_data s =
    print_string (escape_html s)
  in

  let print_url s =
    let p = String.length remove in
    let l = String.length s in
    if p < l & String.sub s 0 p = remove then
      print_data(String.sub s p (l-p))
    else
      print_data s
  in

  let rec print_attlist (node : 'ext node) =
    List.iter
      (fun (name,value) ->
	 print_string name;
	 print_string "=\"";
	 ( match value with
	       Value s -> 
		 if name="href" || name="src" then
		   print_url s 
		 else
		   print_data s
	     | _ -> assert false
		 (* Impossible in wf mode *)
	 );
	 print_string "\" ";
      )
      node#attributes
  in

  let rec print (node : 'ext node) =
    match node # node_type with
	T_element _ ->
	  if node # normprefix = "html" then begin
	    let htmlname = node # localname in
	    let empty = element_declared_as_empty htmlname in
	    print_string "<";
	    print_string htmlname;
	    print_string " ";
	    print_attlist node;
	    print_string ">";
	    node # iter_nodes print;
	    if not empty then begin
	      print_string "</";
	      print_string htmlname;
	      print_string ">";
	    end
	  end
      | T_data ->
	  print_data node#data
      | _ ->
	  ()
	  (* Drop... *)
  in

  (* Detect DOCTYPE: *)
  let html_doctype = recognize_xhtml_doctype doc in

  (* Print DOCTYPE: *)
  let doctype_string = List.assoc html_doctype html401_doctype_string in
  print_endline doctype_string;

  (* Convert and print document: *)
  print doc#root;

  print_newline();

  flush stdout
;;


let main() =
  let file = ref "" in
  let remove = ref "" in
  Arg.parse 
      [ "-remove", Arg.String (fun s -> remove := s),
                "<prefix>   Remove this prefix from all URLs";
      ]
      (fun s -> file := s)
      "Usage: xhtml2html file.xml >file.html";

  if !file = "" then 
    failwith "No input file! See xhtml2html -help for usage information.";

  let doc = parse !file in
  convert_and_print ~remove:!remove doc
;;


try
  main()
with
    error ->
      prerr_endline("*** xhtml2html stopped:");
      prerr_endline(Pxp_types.string_of_exn error);
      exit 1
;;


