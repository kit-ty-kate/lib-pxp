/* $Id: parser.mly,v 1.1 2000/05/06 17:36:17 gerd Exp $
 * ----------------------------------------------------------------------
 *
 */

%{
  open Ast

%}

%token Space
%token Token
%token Type
%token <string> Lname
%token <string> Uname
%token Separator
%token Lparen
%token Rparen
%token Comma
%token Colon
%token <string * int * int> Code
%token Error
%token Alt
%token Loop_plus
%token Loop_star
%token Eof

%start text
%type <Ast.text> text

%%

text:
  declarations rules
    { { text_decls = $1; text_rules = $2; } }

declarations:
  declaration declarations
    { $1 :: $2 }
| Separator
    { [] }

declaration:
  Token Uname
    { D_token $2 }
| Token Type Uname
    { D_typed_token $3 }

rules:
  rule rules
    { $1 :: $2 }
| Separator
    { [] }

rule:
  Lname Lparen formal_arguments Colon branches
    { { rule_name = $1;
	rule_arguments = $3;
	rule_branches = $5;
      }
    }

formal_arguments:
  Rparen
    { [] }
| Lname comma_formal_arguments
    { $1 :: $2 }

comma_formal_arguments:
  Comma Lname comma_formal_arguments
    { $2 :: $3 }
| Rparen
    { [] }

branches:
  branch alt_branches
    { $1 :: $2 }

alt_branches:
  Alt branch alt_branches
    { $2 :: $3 }
|
    { [] }

branch:
  symbol Code patterns Code opt_error_handler
    { { branch_selector = $1;
	branch_binding_code = $2;
	branch_pattern = $3;
	branch_result_code = $4;
	branch_error_code = $5;
      }
    }

patterns:
  pattern patterns
    { $1 :: $2 }
| 
    { [] }

pattern:
  symbol Loop_star
    { { pat_symbol = $1;
	pat_modifier = Repetition;
      }
    }
| symbol Error
    { { pat_symbol = $1;
	pat_modifier = Option;
      }
    }
| symbol
    { { pat_symbol = $1;
	pat_modifier = Exact;
      }
    }

symbol:
  Lname Colon Uname
    { U_symbol($3, Some $1) }
| Lname Colon Lname Lparen actual_arguments 
    { L_symbol($3, $5, Some $1) }
| Uname
    { U_symbol($1, None) }
| Lname Lparen actual_arguments 
    { L_symbol($1, $3, None) }

actual_arguments:
  Rparen
    { [] }
| Lname comma_actual_arguments
    { $1 :: $2 }

comma_actual_arguments:
  Rparen
    { [] }
| Comma Lname comma_actual_arguments
    { $2 :: $3 }

opt_error_handler:
  Error Code
    { Some $2 }
| 
    { None }

%%

(* ======================================================================
 * History:
 * 
 * $Log: parser.mly,v $
 * Revision 1.1  2000/05/06 17:36:17  gerd
 * 	Initial revision.
 *
 * 
 *)
