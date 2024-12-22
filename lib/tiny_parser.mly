%{
  open Ast
%}

%token EOF LPAR "(" RPAR ")" LBRC "{" RBRC "}"
%token FN "fn" LET "let" EQ "=" SEP ";" COMMA "," MUT "mut"
%token IF "if" ELSE "else"
%token PLUS "+" BANG "!"
%token <string> CONST "42"
%token <string> STRING "abc"
%token <string> ID "x"

%type <expr> block_expr
%start <crate> crate

%left EQ
%left PLUS
%left SEP

%%

crate:
  | c = list(item) EOF { c }

item:
  | f = fun_decl { f }

fun_decl:
  | "fn" f = ID "(" pars = separated_list(COMMA, ID) ")" s = block_expr { FUNDECL (f, pars, s) }

block_expr:
  | "{" s = statement ";" e = option(expr) "}" { BLOCK (s, e) }

expr:
  | n = CONST { CONST (int_of_string n) }
  | s = "abc" { STRING (s |> String.split_on_char '\"' |> fun s -> List.nth s 1 ) }
  | x = ID { VAR x }
  | e1 = expr "+" e2 = expr { PLUS (e1, e2) }
  | x = ID "=" e = expr { ASSIGN (x,e) }
  | x = ID option("!") "(" args = separated_list(COMMA, expr) ")" { CALL (x,args) }
  | b = block_expr { b }
  | "(" e = expr ")" { e }
  | "if" e0 = expr e1 = block_expr "else" e2 = block_expr { IFE(e0, e1, e2) }

statement:
  | "let" x = ID "=" e = expr { LET(x, false, e) }
  | "let" "mut" x = ID "=" e = expr { LET(x, true, e) }
  | e = expr { EXPR e }
  | i = item { i }
  | s1 = statement ";" s2 = statement { SEQ (s1, s2) }