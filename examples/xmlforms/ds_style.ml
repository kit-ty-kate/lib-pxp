(* $Id: ds_style.ml,v 1.3 2000/07/08 22:03:11 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_types
open Pxp_document
open Ds_context


let get_dimension s =
  let re = Str.regexp "\\([0-9]*\\(.[0-9]+\\)?\\)[ \t\n]*\\(px\\|cm\\|in\\|mm\\|pt\\)" in
  if Str.string_match re s 0 then begin
    let number = Str.matched_group 1 s in
    let dim = Str.matched_group 3 s in
    match dim with
	"px" -> Tk.Pixels (int_of_float (float_of_string number))
      | "cm" -> Tk.Centimeters (float_of_string number)
      | "in" -> Tk.Inches (float_of_string number)
      | "mm" -> Tk.Millimeters (float_of_string number)
      | "pt" -> Tk.PrinterPoint (float_of_string number)
      | _ -> assert false
  end
  else
    failwith ("Bad dimension: " ^ s)
;;


class virtual shared =
  object(self)

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

    (* --- shared attributes: color & font settings --- *)

    val mutable fgcolor = (None : string option)
    val mutable bgcolor = (None : string option)
    val mutable font = (None : string option)

    method fgcolor =
      (* Get the foreground color: If there is a local value, return it;
       * otherwise ask parent node
       *)
      match fgcolor with
	  Some c -> c
	| None   -> try self # node # parent # extension # fgcolor with
	            Not_found -> failwith "#fgcolor"

    method bgcolor =
      (* Get the background color: If there is a local value, return it;
       * otherwise ask parent node
       *)
      match bgcolor with
	  Some c -> c
	| None   -> try self # node # parent # extension # bgcolor with
	            Not_found -> failwith "#bgcolor"

    method font =
      (* Get the current font: If there is a local value, return it;
       * otherwise ask parent node
       *)
      match font with
	  Some c -> c
	| None   -> try self # node # parent # extension # font with
	            Not_found -> failwith "#font"

    method private init_color_and_font =
      let get_color n =
      	try
	  match self # node # attribute n with
	      Value v -> Some v
	    | Implied_value -> None
	    | _ -> assert false
      	with Not_found -> None in
      fgcolor <- get_color "fgcolor";
      bgcolor <- get_color "bgcolor";
      font    <- get_color "font";      (* sic! *)


    method private bg_color_opt =
      [ Tk.Background (Tk.NamedColor (self # bgcolor)) ]

    method private fg_color_opt =
      [ Tk.Foreground (Tk.NamedColor (self # fgcolor)) ]

    method private font_opt =
      [ Tk.Font (self # font) ]

    (* --- virtual --- *)

    method virtual prepare : unit
    method virtual create_widget : Widget.widget -> context -> Widget.widget

    method pack_opts = ( [] : Tk.options list )
    method xstretchable = false
    method ystretchable = false

    method accept (c:context) = ()

    method private get_mask =
      (* find parent which is a mask *)
      let rec search n =
	match n # node_type with
	    T_element "mask" ->
	      n # extension
	  | T_element _ ->
	      search (n # parent)
	  | _ ->
	      assert false
      in
      search (self # node)


    method private accept_mask (c:context) =
      let rec iterate n =
	n # extension # accept c;
	List.iter iterate (n # sub_nodes)
      in
      iterate (self # get_mask # node)


    method start_node_name =
      (failwith "#start_node_name" : string)

    (* --- debug --- *)

    method private name =
      let nt = self # node # node_type in
      match nt with
	  T_element n -> n
	| T_data      -> "#PCDATA"

  end
;;


class default =
  object (self)
    inherit shared

    method prepare =
      self # init_color_and_font

    method create_widget w c =
      failwith "default # create_widget"
  end
;;


let dummy_node = new element_impl (new default);;

class application =
  object (self)
    inherit shared

    val mutable start_node = dummy_node

    method prepare =
      (* prepare this node *)
      self # init_color_and_font;
      if fgcolor = None then fgcolor <- Some "black";
      if bgcolor = None then bgcolor <- Some "white";
      if font = None then font <- Some "fixed";
      let start =
	match self # node # attribute "start" with
	    Value v -> v
	  | _       -> assert false in
      start_node <- (try self # node # find start with
	  Not_found -> failwith "Start node not found");
      (* iterate over the subtree *)
      let rec iterate n =
	n # extension # prepare;
	List.iter iterate (n # sub_nodes)
      in
      List.iter iterate (self # node # sub_nodes)


    method start_node_name =
      match self # node # attribute "start" with
	  Value v -> v
	| _       -> assert false

    method create_widget w c =
      start_node # extension # create_widget w c

    method pack_opts =
      start_node # extension # pack_opts
  end
;;


class sequence =
  object (self)
    inherit shared

    method prepare =
      self # init_color_and_font;

    method create_widget w c =
      let node = List.hd (self # node # sub_nodes) in
      node # extension # create_widget w c

    method pack_opts =
      let node = List.hd (self # node # sub_nodes) in
      node # extension # pack_opts
  end
;;


class vbox =
  object (self)
    inherit shared

    val mutable att_halign = "left"

    method prepare =
      self # init_color_and_font;
      match self # node # attribute "halign" with
	  Value v -> att_halign <- v
	| _ -> assert false

    method create_widget w c =
      let f = Frame.create w (self # bg_color_opt) in
      let nodes = self # node # sub_nodes in
      let options =
	match att_halign with
	    "left"     -> [ Tk.Anchor Tk.W ]
	  | "right"    -> [ Tk.Anchor Tk.E ]
	  | "center"   -> [ Tk.Anchor Tk.Center ]
	  | _ -> assert false
      in
      List.iter
	(fun n ->
	   let opts = n # extension # pack_opts in
	   let wdg = n # extension # create_widget f c in
	   Tk.pack [wdg] (options @ opts);
	)
	nodes;
      f

    method pack_opts =
      match self # xstretchable, self # ystretchable with
	  true, false  -> [ Tk.Fill Tk.Fill_X; (* Tk.Expand true *) ]
	| false, true  -> [ Tk.Fill Tk.Fill_Y;  (* Tk.Expand true *) ]
	| true, true   -> [ Tk.Fill Tk.Fill_Both; (* Tk.Expand true *) ]
	| false, false -> []

    method xstretchable =
      let nodes = self # node # sub_nodes in
      List.exists (fun n -> n # extension # xstretchable) nodes

    method ystretchable =
      let nodes = self # node # sub_nodes in
      List.exists (fun n -> n # extension # ystretchable) nodes

  end

;;


class mask =
  object (self)

    inherit vbox

    method prepare =
      self # init_color_and_font;
      att_halign <- "left"
  end
;;


class hbox =
  object (self)
    inherit shared

    val mutable att_width = None
    val mutable att_halign = "left"
    val mutable att_valign = "top"

    method prepare =
      self # init_color_and_font;
      begin match self # node # attribute "halign" with
	  Value v -> att_halign <- v
	| _ -> assert false
      end;
      begin match self # node # attribute "valign" with
	  Value v -> att_valign <- v
	| _ -> assert false
      end;
      begin match self # node # attribute "width" with
	  Value v       -> att_width <- Some (get_dimension v)
	| Implied_value -> att_width <- None
	| _ -> assert false
      end

    method create_widget w c =
      let f1 = Frame.create w (self # bg_color_opt) in
      let f_extra =
	match att_width with
	    None    -> []
	  | Some wd ->
	      [ Canvas.create f1
		  ( [ Tk.Width wd; Tk.Height (Tk.Pixels 0);
		      Tk.Relief Tk.Flat;
		      Tk.HighlightThickness (Tk.Pixels 0);
		    ] @
		    self # bg_color_opt ) ]
      in
      let f2 = Frame.create f1 (self # bg_color_opt) in
      let nodes = self # node # sub_nodes in

      let outer_pack_opts =
      	match att_halign with
	    "left"     -> [ Tk.Anchor Tk.W ]
	  | "right"    -> [ Tk.Anchor Tk.E ]
	  | "center"   -> [ Tk.Anchor Tk.Center ]
	  | _ -> assert false
      in
      let inner_pack_opts =
	match att_valign with
	    "top"      -> [ Tk.Anchor Tk.N ]
	  | "bottom"   -> [ Tk.Anchor Tk.S ]
	  | "center"   -> [ Tk.Anchor Tk.Center ]
	  | _ -> assert false
      in
      List.iter
	(fun n ->
	   let opts = n # extension # pack_opts in
	   let wdg = n # extension # create_widget f2 c in
	   Tk.pack [wdg] (inner_pack_opts @ [ Tk.Side Tk.Side_Left ] @ opts);
	)
	nodes;
      let extra_opts = self # pack_opts in
      Tk.pack (f_extra @ [f2]) (outer_pack_opts @ extra_opts);
      f1

    method pack_opts =
      match self # xstretchable, self # ystretchable with
	  true, false  -> [ Tk.Fill Tk.Fill_X;  (* Tk.Expand true *) ]
	| false, true  -> [ Tk.Fill Tk.Fill_Y;  (* Tk.Expand true *) ]
	| true, true   -> [ Tk.Fill Tk.Fill_Both;  (* Tk.Expand true *) ]
	| false, false -> []

    method xstretchable =
      let nodes = self # node # sub_nodes in
      List.exists (fun n -> n # extension # xstretchable) nodes

    method ystretchable =
      let nodes = self # node # sub_nodes in
      List.exists (fun n -> n # extension # ystretchable) nodes

  end
;;

class vspace =
  object (self)
    inherit shared

    val mutable att_height = Tk.Pixels 0
    val mutable att_fill  = false

    method prepare =
      self # init_color_and_font;
      begin match self # node # attribute "height" with
	  Value v       -> att_height <- get_dimension v
	| _ -> assert false
      end;
      begin match self # node # attribute "fill" with
	  Value "yes" -> att_fill <- true
	| Value "no"  -> att_fill <- false
	| _ -> assert false
      end


    method create_widget w c =
      let f = Frame.create w ( self # bg_color_opt ) in
      let strut =
      	Canvas.create f
	  ( [ Tk.Height att_height; Tk.Width (Tk.Pixels 0);
	      Tk.Relief Tk.Flat;
	      Tk.HighlightThickness (Tk.Pixels 0);
	    ] @
	    self # bg_color_opt ) in
      if att_fill then
	Tk.pack [strut] [Tk.Fill Tk.Fill_Y; Tk.Expand true]
      else
	Tk.pack [strut] [];
      f

    method pack_opts =
      if att_fill then [ Tk.Fill Tk.Fill_Y; Tk.Expand true ] else []

    method ystretchable = att_fill
  end
;;

class hspace =
  object (self)
    inherit shared


    val mutable att_width = Tk.Pixels 0
    val mutable att_fill  = false

    method prepare =
      self # init_color_and_font;
      begin match self # node # attribute "width" with
	  Value v       -> att_width <- get_dimension v
	| _ -> assert false
      end;
      begin match self # node # attribute "fill" with
	  Value "yes" -> att_fill <- true
	| Value "no"  -> att_fill <- false
	| _ -> assert false
      end


    method create_widget w c =
      let f = Frame.create w ( self # bg_color_opt ) in
      let strut =
      	Canvas.create f
	  ( [ Tk.Width att_width; Tk.Height (Tk.Pixels 0);
	      Tk.Relief Tk.Flat;
	      Tk.HighlightThickness (Tk.Pixels 0);
	    ] @
	    self # bg_color_opt ) in
      if att_fill then
	Tk.pack [strut] [Tk.Fill Tk.Fill_X; Tk.Expand true]
      else
	Tk.pack [strut] [];
      f

    method pack_opts =
      if att_fill then [ Tk.Fill Tk.Fill_X; Tk.Expand true ] else []

    method xstretchable = att_fill
  end
;;

class label =
  object (self)
    inherit shared

    val mutable att_textwidth = (-1)
    val mutable att_halign = "left"

    method prepare =
      self # init_color_and_font;
      att_textwidth <- (match self # node # attribute "textwidth" with
			    Value v ->
			      let w = try int_of_string v
			      with _ -> failwith ("Not an integer: " ^ v) in
			      w
			  | Implied_value ->
			      (-1)
			  | _ -> assert false);
      att_halign <- (match self # node # attribute "halign" with
			 Value v -> v
		       | _ -> assert false);


    method create_widget w c =
      let opts_textwidth = if att_textwidth < 0 then [] else
	                                       [ Tk.TextWidth att_textwidth ] in
      let opts_halign =
	match att_halign with
	    "left"     -> [ Tk.Anchor Tk.W ]
	  | "right"    -> [ Tk.Anchor Tk.E ]
	  | "center"   -> [ Tk.Anchor Tk.Center ]
	  | _ -> assert false
      in
      let opts_content =
	[ Tk.Text (self # node # data) ] in
      let label = Label.create w (opts_textwidth @ opts_halign @
				  opts_content @ self # bg_color_opt @
				  self # fg_color_opt @ self # font_opt) in
      label

  end
;;

class entry =
  object (self)
    inherit shared

    val mutable tv = lazy (Textvariable.create())
    val mutable att_textwidth = (-1)
    val mutable att_slot = ""

    method prepare =
      self # init_color_and_font;
      tv <- lazy (Textvariable.create());
      att_textwidth <- (match self # node # attribute "textwidth" with
			    Value v ->
			      let w = try int_of_string v
			      with _ -> failwith ("Not an integer: " ^ v) in
			      w
			  | Implied_value ->
			      (-1)
			  | _ -> assert false);
      att_slot <- (match self # node # attribute "slot" with
	  Value v -> v
	| _ -> assert false);

    method create_widget w c =
      let opts_textwidth = if att_textwidth < 0 then [] else
	                                       [ Tk.TextWidth att_textwidth ] in
      let e = Entry.create w ( [ Tk.TextVariable (Lazy.force tv) ] @
			       self # fg_color_opt @
			       self # bg_color_opt @
			       self # font_opt @
			       opts_textwidth
			     ) in
      let s =
	try c # get_slot att_slot with
	    Not_found -> self # node # data in
      Textvariable.set (Lazy.force tv) s;
      e

    method accept c =
      c # set_slot att_slot (Textvariable.get (Lazy.force tv))

  end
;;

class textbox =
  object (self)
    inherit shared

    val mutable att_textwidth = (-1)
    val mutable att_textheight = (-1)
    val mutable att_slot = ""
    val mutable last_widget = None

    method prepare =
      self # init_color_and_font;
      att_textwidth <- (match self # node # attribute "textwidth" with
			    Value v ->
			      let w = try int_of_string v
			      with _ -> failwith ("Not an integer: " ^ v) in
			      w
			  | Implied_value ->
			      (-1)
			  | _ -> assert false);
      att_textheight <- (match self # node # attribute "textheight" with
			    Value v ->
			      let w = try int_of_string v
			      with _ -> failwith ("Not an integer: " ^ v) in
			      w
			  | Implied_value ->
			      (-1)
			  | _ -> assert false);
      att_slot <- (match self # node # attribute "slot" with
		       Value v -> v
		     | Implied_value -> ""
		     | _ -> assert false);


    method create_widget w c =
      let opts_textwidth = if att_textwidth < 0 then [] else
	                                       [ Tk.TextWidth att_textwidth ] in
      let opts_textheight = if att_textheight < 0 then [] else
	                                    [ Tk.TextHeight att_textheight ] in
      let f = Frame.create w (self # bg_color_opt) in
      let vscrbar = Scrollbar.create f [ Tk.Orient Tk.Vertical ] in
      let e = Text.create f ( [ ] @
			      self # fg_color_opt @
			      self # bg_color_opt @
			      self # font_opt @
			      opts_textwidth @ opts_textheight
			    ) in
      last_widget <- Some e;
      Scrollbar.configure vscrbar [ Tk.ScrollCommand
				      (fun s -> Text.yview e s);
				    Tk.Width (Tk.Pixels 9) ];
      Text.configure e [ Tk.YScrollCommand
			   (fun a b -> Scrollbar.set vscrbar a b) ];
      let s =
	if att_slot <> "" then
	  try c # get_slot att_slot with
	      Not_found -> self # node # data 
	else 
	  self # node # data 
      in
      (* Text.insert appends always a newline to the last line; so strip 
       * an existing newline first
       *)
      let s' = 
	if s <> "" & s.[String.length s - 1] = '\n' then
	  String.sub s 0 (String.length s - 1)
	else 
	  s in
      Text.insert e (Tk.TextIndex(Tk.End,[])) s' [];
      if att_slot = "" then
	Text.configure e [ Tk.State Tk.Disabled ];
      Tk.pack [e] [ Tk.Side Tk.Side_Left ];
      Tk.pack [vscrbar] [ Tk.Side Tk.Side_Left; Tk.Fill Tk.Fill_Y ];
      f

    method accept c =
      if att_slot <> "" then
	match last_widget with
	    None -> ()
	  | Some w ->
	      let s =
		Text.get
		  w
		  (Tk.TextIndex(Tk.LineChar(1,0),[]))
		  (Tk.TextIndex(Tk.End,[])) in
	      c # set_slot att_slot s

  end
;;

class button =
  object (self)
    inherit shared

    val mutable att_label = ""
    val mutable att_action = ""
    val mutable att_goto = ""

    method prepare =
      self # init_color_and_font;
      att_label <- (match self # node # attribute "label" with
			Value v -> v
		      | _ -> assert false);
      att_action <- (match self # node # attribute "action" with
			 Value v -> v
		       | _ -> assert false);
      att_goto <- (match self # node # attribute "goto" with
		       Value v -> v
		     | Implied_value -> ""
		     | _ -> assert false);
      if att_action = "goto" then begin
	try let _ = self # node # find att_goto in () with
	    Not_found -> failwith ("Target `" ^ att_goto ^ "' not found")
      end;
      if att_action = "list-prev" or att_action = "list-next" then begin
	let m = self # get_mask in
	if m # node # parent # node_type <> T_element "sequence" then
	  failwith ("action " ^ att_action ^ " must not be used out of <sequence>");
      end;


    method create_widget w c =
      let cmd () =
	self # accept_mask c;
	match att_action with
	    "goto" ->
	      c # goto att_goto
	  | "save" ->
	      c # save_obj
	  | "exit" ->
	      Protocol.closeTk()
	  | "save-exit" ->
	      c # save_obj;
	      Protocol.closeTk()
	  | "list-prev" ->
	      let m = self # get_mask # node in
	      let s = m # parent in
	      let rec search l =
		match l with
		    x :: y :: l' ->
		      if y == m then
			match x # attribute "name" with
			    Value s -> c # goto s
			  | _ -> assert false
		      else
			search (y :: l')
		  | _ -> ()
	      in
	      search (s # sub_nodes)
	  | "list-next" ->
	      let m = self # get_mask # node in
	      let s = m # parent in
	      let rec search l =
		match l with
		    x :: y :: l' ->
		      if x == m then
			match y # attribute "name" with
			    Value s -> c # goto s
			  | _ -> assert false
		      else
			search (y :: l')
		  | _ -> ()
	      in
	      search (s # sub_nodes)
	  | "hist-prev" ->
	      (try c # previous with Not_found -> ())
	  | "hist-next" ->
	      (try c # next with Not_found -> ())
	  | _ -> ()
      in
      let b = Button.create w ( [ Tk.Text att_label; Tk.Command cmd ] @
			      	self # fg_color_opt @
			      	self # bg_color_opt @
			      	self # font_opt ) in
      b


  end
;;


(**********************************************************************)

open Pxp_yacc

let tag_map =
  make_spec_from_mapping
    ~data_exemplar:(new data_impl (new default))
    ~default_element_exemplar:(new element_impl (new default))
    ~element_mapping:
       (let m = Hashtbl.create 50 in
	Hashtbl.add m "application"
	 	      (new element_impl (new application));
	Hashtbl.add m "sequence"
		      (new element_impl (new sequence));
	Hashtbl.add m "mask"
		      (new element_impl (new mask));
	Hashtbl.add m "vbox"
	              (new element_impl (new vbox));
	Hashtbl.add m "hbox"
		      (new element_impl (new hbox));
	Hashtbl.add m "vspace"
		      (new element_impl (new vspace));
	Hashtbl.add m "hspace"
		      (new element_impl (new hspace));
	Hashtbl.add m "label"
		      (new element_impl (new label));
	Hashtbl.add m "entry"
		      (new element_impl (new entry));
	Hashtbl.add m "textbox"
		      (new element_impl (new textbox));
	Hashtbl.add m "button"
		      (new element_impl (new button));
	m)
;;

(* ======================================================================
 * History:
 *
 * $Log: ds_style.ml,v $
 * Revision 1.3  2000/07/08 22:03:11  gerd
 * 	Updates because of PXP interface changes.
 *
 * Revision 1.2  2000/06/04 20:29:19  gerd
 * 	Updates because of renamed PXP modules.
 *
 * Revision 1.1  1999/08/21 19:11:05  gerd
 * 	Initial revision.
 *
 *
 *)
