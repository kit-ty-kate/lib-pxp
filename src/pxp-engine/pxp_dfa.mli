(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

module Graph : sig
  type graph
  type vertex

  (* A directed graph whose edges are marked with strings (= element types)
   * and with the constraint that for a given vertex and a given element
   * type the edge must be unique.
   *)

  exception Edge_not_unique

  val create : unit -> graph
      (* Creates an empty graph *)

  val new_vertex : graph -> vertex
      (* Adds a new vertex to the graph, and returns the vertex *)

  val new_edge : vertex -> string -> vertex -> unit
      (* new_edge v_from etype v_to:
       * Adds a new edge from vertex v_from to vertex v_to, marked with
       * etype.
       * Raises Edge_not_unique if there is already an edge etype starting
       * at v_from to a different vertex than v_to.
       *)

  (* val graph_of_vertex : vertex -> graph *)
      (* Returns the graph the passed vertex is contained in. *)

  val union : graph -> graph -> unit
      (* union g1 g2:
       * Moves the vertexes and edged found in g2 to g1.
       * After that, g2 is empty again.
       *)

  val outgoing_edges : vertex -> (string * vertex) list
      (* Returns the list of outgoing edges starting in the passed vertex *)

  val follow_edge : vertex -> string -> vertex
      (* Follows the edge starting in the passed vertex which is marked
       * with the passed element type.
       * Raises Not_found if there is no such edge.
       *)

  val ingoing_edges : vertex -> (vertex * string) list
      (* Returns the list of ingoing edges ending in the passed vertex *)
end

module VertexSet : Set.S with type elt = Graph.vertex


type dfa_definition =
    { dfa_graph : Graph.graph;
      dfa_start : Graph.vertex;   (* Where the automaton starts *)
      dfa_stops : VertexSet.t;    (* Where the automaton may stop *)
      dfa_null  : bool;           (* Whether dfa_start member of dfa_stops *)
    }

val dfa_of_regexp_content_model : Pxp_core_types.I.regexp_spec -> dfa_definition
  (* Computes the DFA or raises Not_found if it does not exist *)

