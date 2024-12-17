%{
  open Ast
%}

%token EOF LPAR "(" RPAR ")" LBRC "{" RBRC "}"
%token FN "fn" LET "let" EQ "=" SEP ";" COMMA "," QUOTE
%token PLUS "+" BANG "!"
%token <string> CONST "42"
%token <string> STRING "abc"
%token <string> ID "x"

%start <crate> crate

%left EQ
%left PLUS

%%

crate:
  | c = list(item) EOF { c }

item:
  | f = func { f }

func:
  | "fn" f = ID "(" pars = separated_list(COMMA, ID) ")" "{" s = separated_list(SEP, statement) "}" { FUNDECL (f, pars, s) }

expr:
  | n = CONST { CONST (int_of_string n) }
  | s = "abc" { STRING (s |> String.split_on_char '\"' |> fun s -> List.nth s 1 ) }
  | x = ID { VAR x }
  | e1 = expr "+" e2 = expr { PLUS (e1, e2) }
  | x = ID "=" e = expr { ASSIGN (x,e) }
  | x = ID "!" "(" args = separated_list(COMMA, expr) ")" { CALL (x,args) }
  | "(" e = expr ")" { e }

statement:
  | "let" x = ID "=" e = expr { LET(x, e) }
  | e = expr { EXPR e }
  | "{" ss = separated_list(SEP,statement) "}" { BLOCK (ss, None) } 