(* $Id: pxp_entity_manager.ml,v 1.2 2002/10/22 14:22:54 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* An 'entity_manager' is a stack of entities, where the topmost entity
 * is the currently active entity, the second entity is the entity that
 * referred to the active entity, and so on.
 *
 * The entity_manager can communicate with the currently active entity.
 *
 * The entity_manager provides an interface for the parser; the functions
 * returning the current token and the next token are exported.
 *)

open Pxp_entity
open Pxp_dtd
open Pxp_reader

class entity_manager 
      (init_entity : entity) (init_dtd : dtd) =
  let init_r = 
    match init_entity # resolver with
	Some r -> r
      | None -> failwith "Pxp_entity_manager.entity_manager: initial entity does not have a resolver"
  in
  object (self)
    val dtd = init_dtd
    val top_resolver = init_r
    val top_entity = init_entity

    val mutable entity_stack = Stack.create()
    val mutable current_entity = init_entity
    val mutable current_entity's_full_name = lazy (init_entity # full_name)
    val mutable current_resolver = init_r
				   
    val mutable yy_get_next_ref = ref (fun () -> assert false)

    initializer
      init_entity # set_manager (self :> 
				 < current_entity : entity; 
				   pop_entity : unit -> unit;
				   push_entity : entity -> unit >
				);
      yy_get_next_ref := (fun () -> init_entity # next_token)

    method dtd = dtd

    method push_entity e =
      e # set_manager (self :> 
		       < current_entity : entity; 
		         pop_entity : unit -> unit;
			 push_entity : entity -> unit >
		      );
      Stack.push (current_entity, current_entity's_full_name, current_resolver) entity_stack;
      current_entity <- e;
      current_entity's_full_name <- lazy (e # full_name);
      ( match e # resolver with
	    None -> ()   (* e is an internal entity *)
	  | Some r -> current_resolver <- r;
      );
      yy_get_next_ref := (fun () -> e # next_token);

    method pop_entity() =
      (* May raise Stack.Empty *)
      let e, e_name, e_res = Stack.pop entity_stack in
      current_entity <- e;
      current_entity's_full_name <- e_name;
      current_resolver <- e_res;
      yy_get_next_ref := (fun () -> e # next_token);



    method position_string =
      (* Gets a string describing the position of the last token;
       * includes an entity backtrace
       *)
      let b = Buffer.create 200 in
      Buffer.add_string b
	("In entity " ^ current_entity # full_name
	 ^ ", at line " ^ string_of_int (current_entity # line)
	 ^ ", position " ^ string_of_int (current_entity # column)
	 ^ ":\n");
      Stack.iter
	(fun (e, e_name, e_res) ->
	   Buffer.add_string b 
	     ("Called from entity " ^ Lazy.force e_name
	      ^ ", line " ^ string_of_int (e # line)
	      ^  ", position " ^ string_of_int (e # column)
	      ^ ":\n");
	)
	entity_stack;
      Buffer.contents b


    method position =
      (* Returns the triple (full_name, line, column) of the last token *)
      Lazy.force current_entity's_full_name, 
      current_entity # line,
      current_entity # column


    method current_entity_counts_as_external =
      (* Whether the current entity counts as external to the main
       * document for the purpose of stand-alone checks.
       *)
      (* TODO: improve performance *)
      let is_external = ref false in
      let check (e, _, _) =
	if e # counts_as_external then begin
	  is_external := true;
	end;
      in
      check (current_entity,(),());
      Stack.iter check entity_stack;
      !is_external


    method current_entity  = current_entity

    method current_resolver = current_resolver
      (* The resolver of the most recent external entity *)

    method top_entity = top_entity

    method top_resolver = top_resolver

    method yy_get_next_ref = yy_get_next_ref


    (* Methods for out-of-order lexing: *)

    method current_lexbuf = current_entity # lexbuf

    method current_line_column = (current_entity # line, 
				  current_entity # column)

    method update_line_column (l,c) =
      current_entity # set_line_column l c


    method pop_entity_until until_ent =
      (* pops entities from the stack and ensures that they are closed until
       * [until_ent] is the top element. All entities that are taken from
       * the stack are closed. [until_ent] is not closed.
       *)
      try
	while current_entity <> until_ent do
	  if current_entity # is_open then
	    ignore(current_entity # close_entity);
	  self # pop_entity();
	done
      with
	  Stack.Empty -> ()
  end
;;


(* ======================================================================
 * History:
 * 
 * $Log: pxp_entity_manager.ml,v $
 * Revision 1.2  2002/10/22 14:22:54  gerd
 * 	Corrected comment: pop_entity_until does NOT close the topmost
 * entity.
 *
 * Revision 1.1  2002/07/14 23:05:15  gerd
 * 	Initial revision.
 *
 * 
 *)
