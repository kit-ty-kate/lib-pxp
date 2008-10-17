(* Our custom HTML generator *)

(* Define

   {picture file.png Caption Text}

   so images can be easily included into ocamldoc documentation
 *)

open Printf

let word_re = Str.regexp "[ \t\r\n]+"

class chtml =
object(self)
  inherit Odoc_html.html

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

  method html_of_custom_text b s t =
    match s with
      | "{picture" -> self#html_of_picture b t
      | _ -> ()
end

let chtml = new chtml
let _ = 
  Odoc_args.set_doc_generator (Some chtml :> Odoc_args.doc_generator option)
