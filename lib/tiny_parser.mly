%{
  open Ast
%}

%token EOF LPAR "(" RPAR ")" LBRC "{" RBRC "}"
%token FN "fn" LET "let" MUT "mut"
%token IF "if" ELSE "else" LOOP "loop" BREAK "break" TRUE "true" FALSE "false"
%token I32 "i32"
%token SEP ";" COMMA "," COLON ":" ASSIGN "=" DOT "." ARROW "->"
%token PLUS "+" MINUS "-" TIMES "*" DIVIDE "/" BANG "!" EQ "==" LEQ "<=" AMPERSAND "&" PERC "%"
%token <string> CONST
%token <string> STRING
%token <string> ID

%type <expr> block_expr
%start <crate> crate

%left EQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE

%left SEP

%%

crate:
  | c = list(item) EOF { c }

item:
  | f = fun_decl { f }

fun_decl:
  | "fn" f = ID "(" pars = separated_list(COMMA, fun_parameter) ")" option(fun_return_type) s = block_expr { FUNDECL (f, pars, s) }

fun_type:
  | "i32" {  }

fun_return_type:
  | "->" fun_type {  }

fun_parameter:
  | x = ID ":" fun_type { x }

block_expr:
  | "{" s = statement ";" e = option(expr) "}" { BLOCK (s, e) }

expr:
  | "true" { TRUE }
  | "false" { FALSE }
  | "break" { BREAK }
  | n = CONST { CONST (int_of_string n) }
  | s = STRING { STRING (s |> String.split_on_char '\"' |> fun s -> List.nth s 1 ) }
  | x = ID { VAR x }
  | e1 = expr op = binop e2 = expr { ARITH2 (op, e1, e2) }
  | x = ID "=" e = expr { ASSIGN (x,e) }
  | x = ID "." f = ID "(" args = separated_list(COMMA, expr) ")" { CALL(f, (VAR x) :: args) }
  | x = ID option("!") "(" args = separated_list(COMMA, expr) ")" { CALL (x,args) }
  | b = block_expr { b }
  | "(" e = expr ")" { e }
  | "if" e0 = expr e1 = block_expr "else" e2 = block_expr { IFE(e0, e1, e2) }
  | "loop" e = block_expr { LOOP e }

statement:
  | "let" x = ID "=" e = expr { LET(x, false, e) }
  | "let" "mut" x = ID "=" e = expr { LET(x, true, e) }
  | e = expr { EXPR e }
  | i = item { i }
  | s1 = statement ";" s2 = statement { SEQ (s1, s2) }

%inline binop:
| "+" { ADD }
| "-" { SUB }
| "*" { MUL }
| "/" { DIV }
| "%" { MOD }
| "==" { EQ }
| "<=" { LEQ }