(* Our custom HTML generator *)

(* Define

     {picture file.png Caption Text}

     so images can be easily included into ocamldoc documentation

   Define

     {direct_include <true>|<false>}

     changing the bahviour of "include Module". If direct include is enabled,
     the included stuff is directly shown.
 *)

open Printf
open Odoc_info
open Module

let word_re = Str.regexp "[ \t\r\n]+"

class chtml =
object(self)
  inherit Odoc_html.html as super

  method private html_of_picture b t = 
prerr_endline "picture found";
    let (file, caption) =
      match t with
	| [] ->
	    failwith "{picture ...} needs at least one argument"
	| [Odoc_info.Raw arg] ->
	    let w = Str.split word_re arg in
	    ( match w with
		| file :: args ->
		    (file, String.concat " " args)
		| [] ->
		    failwith "{picture ...} needs a simple word as first argument"
	    )
	| _ :: _ ->
	    failwith "{picture ...} needs a simple word as first argument" in
    bprintf b
      "<div class=\"picture\">\
        <div class=\"picture-caption\">%s</div>\
        <img src=\"%s\">\
       </div>"
      (self#escape caption)
      file

  val mutable enable_direct_include = true

  method private html_of_direct_include b t =
    match t with
      | [Odoc_info.Raw "true"] ->
	  enable_direct_include <- true
      | [Odoc_info.Raw "false"] ->
	  enable_direct_include <- false
      | _ ->
	  failwith "{direct_include ...} needs one bool argument"


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


  method html_of_custom_text b s t =
    match s with
      | "{picture" -> self#html_of_picture b t
      | "{direct_include" -> self#html_of_direct_include b t
      | _ -> ()
end

let chtml = new chtml
let _ = 
  Odoc_args.set_doc_generator (Some chtml :> Odoc_args.doc_generator option)
