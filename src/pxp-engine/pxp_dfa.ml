(* $Id: pxp_dfa.ml,v 1.2 2001/06/27 23:34:35 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Pxp_aux


module Graph = struct
  type vertex =
      { mutable edges_out : (string * vertex) list;
	mutable edges_out_map : vertex StringMap.t;
	mutable edges_in : (vertex * string) list;
	mutable graph : graph;
	mutable id : int;
      }
  and graph =
      { mutable vertexes : vertex list;
	mutable mid : int;   (* maximum id + 1 *)
      }

  exception Edge_not_unique

  let create () =
    { vertexes = [];
      mid = 0;
    }

  let new_vertex g =
    let v =
      { edges_out = [];
	edges_out_map = StringMap.empty;
	edges_in = [];
	graph = g;
	id = g.mid;
      } in
    g.vertexes <- v :: g.vertexes;
    g.mid <- g.mid + 1;
    v

  let new_edge v_from e v_to =
    if v_from.graph != v_to.graph then
      invalid_arg "Pxp_dfa.Graph.new_edge";
    try 
      let v = StringMap.find e v_from.edges_out_map in
      if v != v_to then
	raise Edge_not_unique;
    with
	Not_found ->
	  v_from.edges_out     <- (e, v_to) :: v_from.edges_out;
	  v_from.edges_out_map <- StringMap.add e v_to v_from.edges_out_map;
	  v_to.edges_in        <- (v_from, e) :: v_to.edges_in;
	  ()

  let graph_of_vertex v = v.graph

  let union g1 g2 =
    List.iter
      (fun v ->
	 v.graph <- g1;
	 v.id <- v.id + g1.mid;
      )
      g2.vertexes;
    g1.vertexes <- g2.vertexes @ g1.vertexes;
    g1.mid <- g1.mid + g2.mid;
    g2.vertexes <- [];
    g2.mid <- 0

  let outgoing_edges v =
    v.edges_out

  let ingoing_edges v =
    v.edges_in

  let follow_edge v e =
    StringMap.find e v.edges_out_map  (* or raise Not_found *)
end
;;


module VertexOrd = struct
  type t = Graph.vertex
  let compare v1 v2 =
    if v1.Graph.graph != v2.Graph.graph then
      invalid_arg "Pxp_dfa.VertexOrd.compare";
    compare v1.Graph.id v2.Graph.id
end
;;

module VertexSet = Set.Make(VertexOrd);;


type dfa_definition =
    { dfa_graph : Graph.graph;
      dfa_start : Graph.vertex;
      dfa_stops : VertexSet.t;
      dfa_null  : bool;
    }
;;

(**********************************************************************)

(* Now that we have all the auxiliary data types, it is time for the
 * algorithm that transforms regexps to DFAs.
 *)

open Pxp_types

let dfa_of_regexp_content_model re =
  let rec get_dfa re =
    match re with
	Child e ->
	  let g = Graph.create() in
	  let v1 = Graph.new_vertex g in
	  let v2 = Graph.new_vertex g in
	  Graph.new_edge v1 e v2;
	  { dfa_graph = g;
	    dfa_start = v1;
	    dfa_stops = VertexSet.singleton v2;
	    dfa_null = false;
	  }
	  
      | Seq [] ->
	  invalid_arg "Pxp_dfa.dfa_of_regexp_content_model"
      | Seq [re'] ->
	  get_dfa re'
      | Seq (re1 :: seq2) ->
	  let dfa1 = get_dfa re1 in
	  let dfa2 = get_dfa (Seq seq2) in
	  (* Merge the two graphs. The result is in dfa1.dfa_graph: *)
	  Graph.union dfa1.dfa_graph dfa2.dfa_graph;
	  (* Concatenation I: Add additional edges to the graph such
	   * that if w1 matches dfa1, and w2 matches dfa2, and w2 is not
	   * empty, w1w2 will match the merged DFAs.
	   *)
	  List.iter
	    (fun (e,v') ->
	       VertexSet.iter
		 (fun v ->
		    Graph.new_edge v e v')
		 dfa1.dfa_stops
	    )
	    (Graph.outgoing_edges dfa2.dfa_start);
	  (* Concatenation II: If the emtpy string matches dfa2, the stop
	   * nodes of dfa1 remain stop nodes.
	   *)
	  let stops =
	    if dfa2.dfa_null then
	      VertexSet.union dfa1.dfa_stops dfa2.dfa_stops
	    else
	      dfa2.dfa_stops
	  in
	  (* The resulting DFA: *)
	  { dfa_graph = dfa1.dfa_graph;
	    dfa_start = dfa1.dfa_start;
	    dfa_stops = stops;
	    dfa_null  = dfa1.dfa_null && dfa2.dfa_null;
	  }

      | Alt [] ->
	  invalid_arg "Pxp_dfa.dfa_of_regexp_content_model"
      | Alt [re'] ->
	  get_dfa re'
      | Alt alt ->
	  let dfa_alt = List.map get_dfa alt in
	  (* Merge the graphs. The result is in g: *)
	  let g = (List.hd dfa_alt).dfa_graph in
	  List.iter
	    (fun dfa ->
	       Graph.union g dfa.dfa_graph
	    )
	    (List.tl dfa_alt);
	  (* Get the new start node: *)
	  let start = Graph.new_vertex g in
	  (* Add the new edges starting at 'start': *)
	  List.iter
	    (fun dfa ->
	       List.iter
		 (fun (e, v) ->
		    Graph.new_edge start e v)
		 (Graph.outgoing_edges dfa.dfa_start)
	    )
	    dfa_alt;
	  (* If one of the old start nodes was a stop node, the new start
	   * node will be a stop node, too.
	   *)
	  let null = List.exists (fun dfa -> dfa.dfa_null) dfa_alt in
	  let stops =
	    List.fold_left
	      (fun s dfa -> VertexSet.union s dfa.dfa_stops)
	      VertexSet.empty
	      dfa_alt in
	  let stops' =
	    if null then
	      VertexSet.union stops (VertexSet.singleton start)
	    else
	      stops in
	  (* The resulting DFA: *)
	  { dfa_graph = g;
	    dfa_start = start;
	    dfa_stops = stops';
	    dfa_null  = null;
	  }

      | Optional re' ->
	  let dfa' = get_dfa re' in
	  if dfa'.dfa_null then
	    (* simple case *)
	    dfa'
	  else begin
	    (* Optimization possible: case ingoing_edges dfa_start = [] *)
	    let start = Graph.new_vertex dfa'.dfa_graph in
	    List.iter
	      (fun (e, v) ->
		 Graph.new_edge start e v)
	      (Graph.outgoing_edges dfa'.dfa_start);
	    
	    (* The resulting DFA: *)
	    { dfa_graph = dfa'.dfa_graph;
	      dfa_start = start;
	      dfa_stops = VertexSet.union dfa'.dfa_stops 
			                  (VertexSet.singleton start);
	      dfa_null  = true;
	    }
	  end

      | Repeated1 re' ->
	  let dfa' = get_dfa re' in
	  List.iter
	    (fun (e, v') ->
	       VertexSet.iter
		 (fun v ->
		    Graph.new_edge v e v')
		 dfa'.dfa_stops
	    )
	    (Graph.outgoing_edges dfa'.dfa_start);

	    (* The resulting DFA: *)
	    { dfa_graph = dfa'.dfa_graph;
	      dfa_start = dfa'.dfa_start;
	      dfa_stops = dfa'.dfa_stops;
	      dfa_null  = dfa'.dfa_null;
	    }

      | Repeated re' ->
	  get_dfa (Optional (Repeated1 re'))

  in
  try
    get_dfa re
  with
      Graph.Edge_not_unique -> raise Not_found
;;

(* ======================================================================
 * History:
 * 
 * $Log: pxp_dfa.ml,v $
 * Revision 1.2  2001/06/27 23:34:35  gerd
 * 	Moved module StringMap to Pxp_aux.
 *
 * Revision 1.1  2000/07/23 02:16:08  gerd
 * 	Initial revision.
 *
 * 
 *)
