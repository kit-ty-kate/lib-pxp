(* $Id: types.ml,v 1.1 2002/08/28 23:48:28 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(******************************************************)
(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
(*                   14/05/2000                       *)
(******************************************************)

type regexp =
   Char of int
 | Interval of int * int      (* lower bound, upper bound *)
 | Identifier of string
 | Concat of regexp list list (* concatenation of disjunctions *)
;;

type definition = { id : string ; rel : regexp list } ;;

(* ======================================================================
 * History:
 * 
 * $Log: types.ml,v $
 * Revision 1.1  2002/08/28 23:48:28  gerd
 * 	Initial revision.
 *
 * 
 *)
