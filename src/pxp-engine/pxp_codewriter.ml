(* $Id: pxp_codewriter.ml,v 1.1 2000/05/29 23:48:38 gerd Exp $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

open Pxp_document
open Pxp_yacc
open Pxp_dtd
open Pxp_types

let write_expr_ext_id out extid =
  match extid with
      System s ->
	output_string out ("(Pxp_types.System\"" ^ String.escaped s ^ "\")")
    | Public(s,t) ->
	output_string out ("(Pxp_types.Public(\"" ^ String.escaped s ^ 
			   "\",\"" ^
			   String.escaped t ^ "\"))")
;;


let rec write_expr_content_model out cm =
  match cm with
      Unspecified -> output_string out "Pxp_types.Unspecified"
    | Empty       -> output_string out "Pxp_types.Empty"
    | Any         -> output_string out "Pxp_types.Any"
    | Mixed msl   -> output_string out "(Pxp_types.Mixed [";
	             List.iter
	               (fun ms ->
			  write_expr_mixed_spec out ms;
			  output_string out "; ";
		       )
		       msl;
		     output_string out "])";
    | Regexp re   -> output_string out "(Pxp_types.Regexp ";
	             write_expr_regexp_spec out re;
		     output_string out ")";

and write_expr_mixed_spec out ms =
  match ms with
      MPCDATA  -> output_string out "Pxp_types.MPCDATA"
    | MChild s -> output_string out ("(Pxp_types.MChild \"" ^
				     String.escaped s ^ "\")")

and write_expr_regexp_spec out re =
  match re with
      Optional re'  -> output_string out "(Pxp_types.Optional ";
	               write_expr_regexp_spec out re';
		       output_string out ")";
    | Repeated re'  -> output_string out "(Pxp_types.Repeated ";
	               write_expr_regexp_spec out re';
		       output_string out ")";
    | Repeated1 re' -> output_string out "(Pxp_types.Repeated1 ";
	               write_expr_regexp_spec out re';
		       output_string out ")";
    | Alt rel       -> output_string out "(Pxp_types.Alt [";
	               List.iter
			 (fun re' ->
			    write_expr_regexp_spec out re';
			    output_string out "; ";
			 )
			 rel;
		       output_string out "])";
    | Seq rel       -> output_string out "(Pxp_types.Seq [";
	               List.iter
			 (fun re' ->
			    write_expr_regexp_spec out re';
			    output_string out "; ";
			 )
			 rel;
		       output_string out "])";
    | Child s       -> output_string out ("(Pxp_types.Child \"" ^ 
					  String.escaped s ^ "\")")
;;


let write_expr_att_type out at =
  match at with
      A_cdata       -> output_string out "Pxp_types.A_cdata"
    | A_id          -> output_string out "Pxp_types.A_id"
    | A_idref       -> output_string out "Pxp_types.A_idref"
    | A_idrefs      -> output_string out "Pxp_types.A_idrefs"
    | A_entity      -> output_string out "Pxp_types.A_entity"
    | A_entities    -> output_string out "Pxp_types.A_entities"
    | A_nmtoken     -> output_string out "Pxp_types.A_nmtoken"
    | A_nmtokens    -> output_string out "Pxp_types.A_nmtokens"
    | A_notation sl -> output_string out "(Pxp_types.A_notation [";
	               List.iter
			 (fun s ->
			    output_string out ("\"" ^ 
					       String.escaped s ^ "\"; "))
			 sl;
		       output_string out "])";
    | A_enum sl     -> output_string out "(Pxp_types.A_enum [";
	               List.iter
			 (fun s ->
			    output_string out ("\"" ^ 
					       String.escaped s ^ "\"; "))
			 sl;
		       output_string out "])";
;;


let write_expr_att_default out ad =
  match ad with
      D_required  -> output_string out "Pxp_types.D_required"
    | D_implied   -> output_string out "Pxp_types.D_implied"
    | D_default s -> output_string out ("(Pxp_types.D_default \"" ^
					String.escaped s ^ "\")")
    | D_fixed s   -> output_string out ("(Pxp_types.D_fixed \"" ^
					String.escaped s ^ "\")")
;;


let write_expr_att_value out av =
  match av with
      Value s       -> output_string out ("(Pxp_types.Value \"" ^
					  String.escaped s ^ "\")")
    | Valuelist sl  -> output_string out ("(Pxp_types.Valuelist [");
	               List.iter
			 (fun s ->
			    output_string out ("\"" ^ String.escaped s ^ 
					       "\"; ")
			 )
			 sl;
		       output_string out "])";
    | Implied_value -> output_string out "Pxp_types.Implied_value"
;;


let write_expr_new_pi out pi =
  output_string out ("(new Pxp_dtd.proc_instruction \"" ^
		     String.escaped(pi # target) ^ "\" \"" ^
		     String.escaped(pi # value) ^ "\")")
;;


let write_expr_node_type out nt =
  match nt with
      T_data      -> output_string out "Pxp_document.T_data"
    | T_element s -> output_string out ("(Pxp_document.T_element \"" ^
					String.escaped s ^ "\")")
;;


let write_local_dtd out (dtd : dtd) =
  (* Outputs "let mkdtd warner = ... in" to 'out' *)
  output_string out "let mkdtd warner =\n";
  output_string out "let dtdobj = new Pxp_dtd.dtd warner in\n";
  
  (* Set the ID: *)
  output_string out "dtdobj # set_id ";
  begin match dtd # id with
      None -> ()
    | Some(External x) -> 
	output_string out "(Pxp_types.External ";
	write_expr_ext_id out x;
	output_string out ");\n"
    | Some(Derived x) ->  
	output_string out "(Pxp_types.Derived ";
	write_expr_ext_id out x;
	output_string out ");\n"
    | Some Internal ->   
	output_string out "Pxp_types.Internal;\n";
  end;

  (* Add elements: *)
  List.iter
    (fun elname ->
       (* Create the element 'el': *)
       let el = dtd # element elname in
       output_string out ("let el = new Pxp_dtd.dtd_element dtdobj \"" ^
			  String.escaped elname ^ "\" in\n");
       output_string out "let cm = ";
       write_expr_content_model out (el # content_model);
       output_string out " in\n";
       output_string out "el # set_content_model cm;\n";
       (* Add attributes: *)
       List.iter
	 (fun attname ->
	    let atttype, attdefault = el # attribute attname in
	    output_string out ("el # add_attribute \"" ^ 
			       String.escaped attname ^ "\" ");
	    write_expr_att_type out atttype;
	    output_string out " ";
	    write_expr_att_default out attdefault;
	    output_string out ";\n";
	 )
	 (el # attribute_names);

       (* Validate: *)
       output_string out "el # validate;\n";
 
       (* Add the element 'el' to 'dtdobj': *)
       output_string out "dtdobj # add_element el;\n";
    )
    (dtd # element_names);

  (* Add notations: *)
  List.iter
    (fun noname ->
       let no = dtd # notation noname in
       output_string out ("let no = new Pxp_dtd.dtd_notation \"" ^
			  String.escaped noname ^ "\" ");
       write_expr_ext_id out (no # ext_id);
       output_string out " in\n";
       output_string out "dtdobj # add_notation no;\n";
    )
    (dtd # notation_names);

  (* Add processing instructions: *)
  List.iter
    (fun target ->
       let pilist = dtd # pinstr target in
       List.iter
	 (fun pi ->
	    output_string out "let pi = ";
	    write_expr_new_pi out pi;
	    output_string out " in\n";
	    output_string out "dtdobj # add_pinstr pi;\n";
	 )
	 pilist;
    )
    (dtd # pinstr_names);

  (* Set the name of the root element: *)
  begin match dtd # root with
      None -> ()
    | Some rootname ->
	output_string out ("dtdobj # set_root \"" ^
			   String.escaped rootname ^ "\";\n")
  end;

  (* Special options: *)
  if dtd # arbitrary_allowed then
    output_string out "dtdobj # allow_arbitrary;\n"
  else
    output_string out "dtdobj # disallow_arbitrary;\n";

  (* Return dtdobj: *)
  output_string out "dtdobj in\n"
;;


let rec write_local_subtree out n =
  (* Outputs "let mktree map dtd =  ... in" *)
  
  output_string out "let mktree map dtd =\n";
  output_string out "let nt = ";
  write_expr_node_type out (n # node_type);
  output_string out " in\n";
  output_string out "let exemplar = try Hashtbl.find map.Pxp_yacc.map nt with Not_found -> map.Pxp_yacc.default_element in\n";
  begin match n # node_type with
      T_data ->
	output_string out ("let t = exemplar # create_data dtd \"" ^ 
			   String.escaped (n # data) ^ "\" in\n")
    | T_element _ ->
	output_string out "let atts = [ ";
	List.iter
	  (fun (name,value) ->
	     begin match value with
		 Value s -> 
		   output_string out ("\"" ^ String.escaped name ^ "\", ");
		   output_string out ("\"" ^ String.escaped s ^ "\"; ")
	       | Valuelist sl ->
		   output_string out ("\"" ^ String.escaped name ^ "\", ");
		   output_string out ("\"" ^ 
				      String.escaped (String.concat " " sl) ^ 
				      "\"; ")
	       | Implied_value ->
		   ()
	     end
	  )
	  (n # attributes);
	output_string out " ] in\n";
	output_string out ("let t = exemplar # create_element dtd nt atts in\n");
  end;

  (* Add processing instructions: *)
  List.iter
    (fun target ->
       let pilist = n # pinstr target in
       List.iter
	 (fun pi ->
	    output_string out "let pi = ";
	    write_expr_new_pi out pi;
	    output_string out " in\n";
	    output_string out "t # add_pinstr pi;\n";
	 )
	 pilist;
    )
    (n # pinstr_names);
       
  (* Add the sub nodes: *)
  n # iter_nodes
    (fun n' ->
       write_local_subtree out n';
       output_string out "t # add_node (mktree map dtd);\n";
    );

  (* Validate: *)
  output_string out "t # local_validate;\n";

  (* Return: *)
  output_string out "t in\n"
;;


let write_local_document out (d : 'ext document) =
  (* Outputs "let mkdoc warner map = ... in" *)
  
  output_string out "let mkdoc warner map =\n";
  output_string out "let doc = new Pxp_document.document warner in\n";
  output_string out ("doc # init_xml_version \"" ^
		     String.escaped (d # xml_version) ^ "\";\n");
  output_string out ("doc # init_xml_standalone " ^
		     (if d # xml_standalone then "true" else "false") ^
		     ";\n");
  write_local_dtd out (d # dtd);
  output_string out "let dtd = mkdtd warner in\n";
  write_local_subtree out (d # root);
  output_string out "let root = mktree map dtd in\n";
  output_string out "doc # init_root root;\n";

  (* Add processing instructions: *)
  List.iter
    (fun target ->
       let pilist = d # pinstr target in
       List.iter
	 (fun pi ->
	    output_string out "let pi = ";
	    write_expr_new_pi out pi;
	    output_string out " in\n";
	    output_string out "doc # add_pinstr pi;\n";
	 )
	 pilist;
    )
    (d # pinstr_names);
  
  (* Return the result: *)
  output_string out "doc in\n"
;;

 
let write_document out d =
  output_string out "let create_document warner map =\n";
  write_local_document out d;
  output_string out "mkdoc warner map;;\n"
;;


let write_dtd out dtd =
  output_string out "let create_dtd warner =\n";
  write_local_dtd out dtd;
  output_string out "mkdtd warner;;\n"
;;


let write_subtree out t =
  output_string out "let create_subtree dtd map =\n";
  write_local_subtree out t;
  output_string out "mktree dtd map;;\n"
;;

(* ======================================================================
 * History:
 * 
 * $Log: pxp_codewriter.ml,v $
 * Revision 1.1  2000/05/29 23:48:38  gerd
 * 	Changed module names:
 * 		Markup_aux          into Pxp_aux
 * 		Markup_codewriter   into Pxp_codewriter
 * 		Markup_document     into Pxp_document
 * 		Markup_dtd          into Pxp_dtd
 * 		Markup_entity       into Pxp_entity
 * 		Markup_lexer_types  into Pxp_lexer_types
 * 		Markup_reader       into Pxp_reader
 * 		Markup_types        into Pxp_types
 * 		Markup_yacc         into Pxp_yacc
 * See directory "compatibility" for (almost) compatible wrappers emulating
 * Markup_document, Markup_dtd, Markup_reader, Markup_types, and Markup_yacc.
 *
 * ======================================================================
 * Old logs from markup_codewriter.ml:
 *
 * Revision 1.1  2000/03/11 22:57:28  gerd
 * 	Initial revision.
 *
 * 
 *)