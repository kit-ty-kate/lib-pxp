
%token Space Newline Stop
%token Add Sub Mult Div LParen RParen
%token<int> Number

%start topexpr
%type<int> topexpr

%left Add Sub
%left Mult Div
%nonassoc Number LParen RParen

%%

topexpr:
  expr Stop { $1 }
;

expr:
  expr Add expr        { $1 + $3 }
| expr Sub expr        { $1 - $3 }
| expr Mult expr       { $1 * $3 }
| expr Div expr        { $1 / $3 }
| LParen expr RParen   { $2 }
| Number               { $1 }
;

