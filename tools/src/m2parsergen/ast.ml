(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

type declaration =
    D_token of string                         (* D_token name *)
  | D_typed_token of string                   (* D_typed_token name *)
;;

type symbol =
    U_symbol of (string * string option)      (* U_symbol(token, label) *)
  | L_symbol of (string * string list * string option)
                                        (* L_symbol(token, args, label) *)
  | L_indirect of (string * string list * string option)
;;


type modifier =
    Exact
  | Option
  | Repetition
;;


type pattern =
    { pat_symbol : symbol;
      pat_modifier : modifier;
    }


type branch = 
    { branch_selector : symbol;
      branch_early_code : (string * int * int);
      branch_binding_code : (string * int * int);
      branch_pattern : pattern list;
      branch_result_code : (string * int * int);
      branch_error_code : (string * int * int) option;
    }
;;

type rule =
    { rule_name : string;
      rule_arguments : string list;           (* List of names *)
      rule_branches : branch list;
    }
;;

type text =
    { text_decls : declaration list;
      text_rules : rule list;
    }
;;

