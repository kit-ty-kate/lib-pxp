(* Our custom HTML generator *)

(* Define

     {picture file.png Caption Text}

     so images can be easily included into ocamldoc documentation

   Define

     {directinclude <true>|<false>}

     changing the bahviour of "include Module". If direct include is enabled,
     the included stuff is directly shown.

   Define

     {fixpxpcoretypes <true>|<false>}

     If enabled, this mode changes all clickable references 
     - of Pxp_core_types.I.<id> into Pxp_types.<id>, and
     - of Pxp_core_types.S.<id> into Pxp_types.<id>

   Define
     {knowntype identifier}
     {knownclass identifier}

     to enter additional names into the tables of type and class names
     for which links are generated

 *)

open Printf
open Odoc_info
open Module

module StringSet = Odoc_html.StringSet


let word_re = Str.regexp "[ \t\r\n]+"

let split_args t =
  match t with
    | [] -> []
    | [Odoc_info.Raw arg] -> Str.split word_re arg
    | _ ->
	failwith "Argument too complicated"



class chtml =
object(self)
  inherit Odoc_html.html as super

  method private html_of_picture b t = 
prerr_endline "picture found";
    let (file, caption) =
      match split_args t with
	| [] ->
	    failwith "{picture ...} needs at least one argument"
	| w ->
	    ( match w with
		| file :: args ->
		    (file, String.concat " " args)
		| [] ->
		    failwith "{picture ...} needs a simple word as first argument"
	    ) in
    bprintf b
      "<div class=\"picture\">\
        <div class=\"picture-caption\">%s</div>\
        <img src=\"%s\">\
       </div>"
      (self#escape caption)
      file

  val mutable enable_direct_include = false

  method private html_of_direct_include b t =
    match split_args t with
      | ["true"] ->
	  enable_direct_include <- true
      | ["false"] ->
	  enable_direct_include <- false
      | _ ->
	  failwith "{directinclude ...} needs one bool argument"


  method html_of_included_module b im =   (* overridden! *)
    super # html_of_included_module b im;
    if enable_direct_include then (
      match im.im_module with
	| None -> ()    (* case module is unknown *)
	| Some (Mod m) ->
	    bprintf b "<div class=\"included-module\">\n";
	    List.iter
              (self#html_of_module_element b (Name.father m.m_name))
              (Module.module_elements m);
	    bprintf b "</div>\n"
	| Some (Modtype mt) ->
	    bprintf b "<div class=\"included-module-type\">\n";
	    List.iter
              (self#html_of_module_element b (Name.father mt.mt_name))
              (Module.module_type_elements mt);
	    bprintf b "</div>\n"
    )


  val mutable enable_fix_pxp_core_types = false

  method private html_of_fix_pxp_core_types b t =
    match split_args t with
      | ["true"] ->
	  enable_fix_pxp_core_types <- true
      | ["false"] ->
	  enable_fix_pxp_core_types <- false
      | _ ->
	  failwith "{fixpxpcoretypes ...} needs one bool argument"


  val pxp_core_types_re = Str.regexp "Pxp_core_types\\.[SI]\\."

  method create_fully_qualified_idents_links m_name s =
    let s' =
      if enable_fix_pxp_core_types then (
	Str.global_replace pxp_core_types_re "Pxp_types." s 
      )
      else
	s in
    super # create_fully_qualified_idents_links m_name s'

  method html_of_Ref b name ref_opt =
    let name' =
      if enable_fix_pxp_core_types then (
	(* prerr_endline ("Ref: " ^ name); *)
	Str.global_replace pxp_core_types_re "Pxp_types." name
      )
      else
	name in
    super # html_of_Ref b name' ref_opt

  method add_known_type t =
    List.iter
      (fun s ->
	 known_types_names <- StringSet.add s known_types_names
      )
      (split_args t)

  method add_known_class t =
    List.iter
      (fun s ->
	 known_classes_names <- StringSet.add s known_classes_names
      )
      (split_args t)

  method html_of_custom_text b s t =
    match s with
      | "{picture"
      | "picture" -> self#html_of_picture b t
      | "{directinclude" 
      | "directinclude" -> self#html_of_direct_include b t
      | "{fixpxpcoretypes" 
      | "fixpxpcoretypes" -> self#html_of_fix_pxp_core_types b t
      | "{knowntype" 
      | "knowntype" -> self#add_known_type t
      | "{knownclass"
      | "knownclass" -> self#add_known_class t
      | _ -> ()
end

let chtml = new chtml
let _ = 
  Odoc_args.set_doc_generator (Some chtml :> Odoc_args.doc_generator option)
