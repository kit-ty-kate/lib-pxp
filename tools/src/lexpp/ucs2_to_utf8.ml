(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(******************************************************)
(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
(*                   14/05/2000                       *)
(******************************************************)

(* [14-Jun-2001] Slightly modified by Gerd Stolpmann *)
(* [24-Aug-2002] Generalized for the code range 0-0x1fffff by gs *)

(* Surrogate Pairs are not accepted in XML files (is it true???) *)
exception SurrogatePairs;;

(* Interval (n,m) where n >m m *)
exception InvalidInterval of int * int;;

(* Given an ucs2 character code, returns it in utf8 *)
(* (as a concatenation of characters)               *)
let char_ucs2_to_utf8 =
 function
    n when n >= 0xD800 && n <= 0xDFFF -> raise SurrogatePairs
  | n when n <= 0x007F -> Uni_types.Char n
  | n when n <= 0x07FF ->
     Uni_types.Concat
      [[Uni_types.Char (n lsr  6 land 0b00011111 lor 0b11000000)] ;
       [Uni_types.Char (n        land 0b00111111 lor 0b10000000)]]
  | n when n <= 0xffff ->
     Uni_types.Concat
      [[Uni_types.Char (n lsr 12 land 0b00001111 lor 0b11100000)] ;
       [Uni_types.Char (n lsr  6 land 0b00111111 lor 0b10000000)] ;
       [Uni_types.Char (n        land 0b00111111 lor 0b10000000)]]
  | n when n <= 0x1fffff ->
     Uni_types.Concat
      [[Uni_types.Char (n lsr 18 land 0b00000111 lor 0b11110000)] ;
       [Uni_types.Char (n lsr 12 land 0b00111111 lor 0b10000000)] ;
       [Uni_types.Char (n lsr  6 land 0b00111111 lor 0b10000000)] ;
       [Uni_types.Char (n        land 0b00111111 lor 0b10000000)]]
  | _ ->
      failwith "Code point is outside the supported range 0..0x1fffff"
;;

(*CSC: Two functions for debugging pourposes only

let char_ucs2_to_utf8 =
 function
    n when n >= 0xD800 && n <= 0xDFFF -> assert false
  | n when n <= 0x007F -> [[n]]
  | n when n <= 0x07FF ->
     [[(n lsr  6 land 0b00011111 lor 0b11000000)] ;
      [(n        land 0b00111111 lor 0b10000000)]]
  | n ->
     [[(n lsr 12 land 0b00001111 lor 0b11100000)] ;
      [(n lsr  6 land 0b00111111 lor 0b10000000)] ;
      [(n        land 0b00111111 lor 0b10000000)]]
;;

let rec bprint =
 function
    0 -> ""
  | n -> bprint (n / 2) ^ string_of_int (n mod 2)
;;
*)

(* A few useful functions *)
let rec mklist e =
 function
    0 -> []
  | n -> e::(mklist e (n - 1))
;;

let sup =
 let t = Uni_types.Char 0b10111111 in
  function
     1 -> t
   | n -> Uni_types.Concat (mklist [t] n)
;;

let rec inf =
 let b = Uni_types.Char 0b10000000 in
  function
     1 -> [[b]]
   | n -> mklist [b] n
;;

let mysucc =
 function
    [Uni_types.Char n] -> n + 1
  | _ -> assert false
;;

let mypred =
 function
    [Uni_types.Char n] -> n - 1
  | _ -> assert false
;;

(* Given two utf8-encoded extremes of an interval character code      *)
(* whose 'length' is the same, it returns the utf8 regular expression *)
(* matching all the characters in the interval                        *)
let rec same_length_ucs2_to_utf8 =
 let module T = Uni_types in
  function

   (* Trivial cases: *)

     (T.Char n, T.Char m) when n = m -> [T.Char n]
   | (T.Char n, T.Char m) -> [T.Interval (n,m)]

   (* Anchors of the recursion for 2-element concatations: *)

   | (T.Concat [hen ; [tln]], T.Concat [hem ; [tlm]]) when hen = hem ->
      [T.Concat [hen ; same_length_ucs2_to_utf8 (tln,tlm)]]
   | (T.Concat [hen ; [tln]], T.Concat ([hem ; [tlm]] as e2)) ->
      (T.Concat [hen ; same_length_ucs2_to_utf8 (tln,sup 1)]) ::
      (let shen = mysucc hen
       and phem = mypred hem in
       let succhen = [T.Char shen] in
        if succhen = hem then
         same_length_ucs2_to_utf8 (T.Concat (succhen::(inf 1)), T.Concat e2)
        else
         (T.Concat [[T.Interval (shen, phem)] ;
          [T.Interval (0b10000000,0b10111111)]])::
           same_length_ucs2_to_utf8 (T.Concat (hem::(inf 1)), T.Concat e2)
      )
    (*same_length_ucs2_to_utf8 (T.Concat ((mysucc hen)::(inf 1)), T.Concat e2)*)
   (* Reducing n-element concationations: *)

   | (T.Concat (hen::tln), T.Concat (hem::tlm)) when hen = hem ->
      [T.Concat [hen ; same_length_ucs2_to_utf8 (T.Concat tln, T.Concat tlm)]]
   | (T.Concat (hen::tln), T.Concat ((hem::tlm) as e2)) ->
      let n = List.length tln in
       (T.Concat
        [hen ; same_length_ucs2_to_utf8 (T.Concat tln,sup n)]) ::
         (let shen = mysucc hen
          and phem = mypred hem in
          let succhen = [T.Char shen] in
           if succhen = hem then
            same_length_ucs2_to_utf8 (T.Concat (succhen::(inf n)), T.Concat e2)
           else
            (T.Concat ([T.Interval (shen, phem)] ::
		       mklist [T.Interval (0b10000000,0b10111111)] n)
            )::
             same_length_ucs2_to_utf8 (T.Concat (hem::(inf n)), T.Concat e2)
       )
     (*same_length_ucs2_to_utf8 (T.Concat ((mysucc hen)::(inf n)),T.Concat e2)*)
   | _ -> assert false
;;

(* Given an interval of ucs2 characters, splits *)
(* the list in subintervals whose extremes has  *)
(* the same utf8 encoding length and, for each  *)
(* extreme, calls same_length_ucs2_to_utf8      *)
let rec seq_ucs2_to_utf8 =
 function
    (n,_) when n >= 0xD800 && n <= 0xDFFF -> raise SurrogatePairs
  | (_,n) when n >= 0xD800 && n <= 0xDFFF -> raise SurrogatePairs
  | (n,m) when n > m -> raise (InvalidInterval (n,m))
  | (n,m) when n = m -> [char_ucs2_to_utf8 n]
  | (n,m) when n <= 0x07F && m > 0x07F ->
      (seq_ucs2_to_utf8 (n,0x07F)) @ (seq_ucs2_to_utf8 (0x080,m))
  | (n,m) when n <= 0x07FF && m > 0x07FF ->
      (seq_ucs2_to_utf8 (n,0x07FF)) @ (seq_ucs2_to_utf8 (0x0800,m))
  | (n,m) when n <= 0xffff && m > 0xffff ->
      (seq_ucs2_to_utf8 (n,0xFFFF)) @ (seq_ucs2_to_utf8 (0x10000,m))
  | (n,m) ->
      let utf8n = char_ucs2_to_utf8 n
      and utf8m = char_ucs2_to_utf8 m in
       same_length_ucs2_to_utf8 (utf8n,utf8m)
;;


(* simplify: For example,
  '\224'('\160'['\128'-'\191'] |
         ['\161'-'\190']['\128'-'\191'] |
         '\191'['\128'-'\191']) |
  can be simplified to
  '\224' ['\160'-'\191'] ['\128'-'\191']
*)


let rec simplify_disjunction =
  function
      Uni_types.Char n1 :: Uni_types.Interval(n2,n3) :: rest when n1+1 = n2 ->
	simplify_disjunction(Uni_types.Interval(n2,n3) :: rest)
    | Uni_types.Interval(n1,n2) :: Uni_types.Interval(n3,n4) :: rest when n2+1 = n3 ->
	simplify_disjunction(Uni_types.Interval(n1,n4) :: rest)
    | Uni_types.Interval(n1,n2) :: Uni_types.Char n3 :: rest when n2+1 = n3 ->
	simplify_disjunction(Uni_types.Interval(n1,n3) :: rest)

    | Uni_types.Concat( [Uni_types.Char n1] :: tail1 ) ::
      Uni_types.Concat( [Uni_types.Interval(n2,n3)] :: tail2 ) ::
      rest when n1+1 = n2 && tail1 = tail2 ->
	simplify_disjunction(
	  Uni_types.Concat( [Uni_types.Interval(n1,n3)] :: tail1 ) :: rest)
    | Uni_types.Concat( [Uni_types.Interval(n1,n2)] :: tail1 ) ::
      Uni_types.Concat( [Uni_types.Interval(n3,n4)] :: tail2 ) ::
      rest when n2+1 = n3 && tail1 = tail2 ->
	simplify_disjunction(
	  Uni_types.Concat( [Uni_types.Interval(n1,n4)] :: tail1 ) :: rest)
    | Uni_types.Concat( [Uni_types.Interval(n1,n2)] :: tail1 ) ::
      Uni_types.Concat( [Uni_types.Char n3] :: tail2 ) ::
      rest when n2+1 = n3 && tail1 = tail2 ->
	simplify_disjunction(
	  Uni_types.Concat( [Uni_types.Interval(n1,n3)] :: tail1 ) :: rest)

    | Uni_types.Concat([[x]]) :: rest ->
	simplify_disjunction(x :: rest)

    | Uni_types.Concat([Uni_types.Concat d] :: d') :: rest ->
	let d'' = List.map simplify_disjunction d' in
	simplify_disjunction(Uni_types.Concat(d @ d'') :: rest)

    (* there are probably missing cases!!! *)

    | Uni_types.Concat l :: rest ->
	let l' = List.map simplify_disjunction l in
	if l = l' then
	  Uni_types.Concat l :: simplify_disjunction rest
	else
	  simplify_disjunction(Uni_types.Concat l' :: simplify_disjunction rest)

    | x :: rest -> x :: (simplify_disjunction rest)
    | [] -> []
;;


let rec multi_simplify_disjunction l =
  let l' = simplify_disjunction l in
  if l = l' then l else multi_simplify_disjunction l'
;;


(* Given an ucs2 regual expression, returns  *)
(* the corresponding utf8 regular expression *)
let ucs2_to_utf8 { Uni_types.id = id ; Uni_types.rel = rel } =
 let rec aux re l2 =
  match re with
     Uni_types.Char i -> char_ucs2_to_utf8 i :: l2
   | Uni_types.Interval (l,u) -> seq_ucs2_to_utf8 (l,u) @ l2
   | Uni_types.Identifier _ as i -> i :: l2
   | Uni_types.Concat rell ->
      let foo rel = List.fold_right aux rel [] in
       Uni_types.Concat (List.map foo rell) :: l2
 in
  { Uni_types.id = id ;
    Uni_types.rel = multi_simplify_disjunction (List.fold_right aux rel []) }
;;

