%{
  open Ast
%}

%token
  EOF
  LPAR          "("
  RPAR          ")"
  LBRC          "{"
  RBRC          "}"
  FN            "fn"
  LET           "let"
  MUT           "mut"
  IF            "if"
  ELSE          "else"
  LOOP          "loop"
  BREAK         "break"
  CONTINUE      "continue"
  TRUE          "true"
  FALSE         "false"
  TY_I32        "i32"
  TY_STRING     "String"
  TY_STR        "str"
  SEP           ";"
  COMMA         ","
  COLON         ":"
  ASSIGN        "="
  DOT           "."
  ARROW         "->"
  PLUS          "+"
  MINUS         "-"
  TIMES         "*"
  DIVIDE        "/"
  BANG          "!"
  EQ            "=="
  LEQ           "<="
  AMPERSAND     "&"
  PERC          "%"

%token <string> CONST
%token <string> STRING
%token <string> ID

%type <expr> block_expr
%start <crate> crate

%right "="
%left "==" "<="
%left "+" "-"
%left "*" "/" "%"
%nonassoc "&"

// Note: if WOSEP has priority over SEP, statement -> expr_with_block SEP will
// never be reduced and SEP will always be part of the next statement
%nonassoc TRUE FALSE CONST STRING LPAR LOOP LET LBRC IF ID FN CONTINUE BREAK
%nonassoc WOSEP
%nonassoc SEP
%left SS

%%

crate:
  | c = list(item) EOF { c }

item:
  | f = fun_decl { f }

fun_decl:
  | "fn" name = ID "(" pars = separated_list(COMMA, fun_parameter) ")" option(fun_return_type) body = block_expr { 
    let body, ret = remove_block body in
    FUNDECL {name; pars; body; ret} }

typ:
  | "i32" {  }
  | "String" {  }
  | "str" {  }

fun_type:
  | t = typ { t }
  | "&" boption("mut") t = typ { t }

fun_return_type:
  | "->" fun_type {  }

fun_parameter:
  | x = ID ":" fun_type { x }

block_expr:
  | "{" s = statement e = option(expr_without_block) "}" { BLOCK (s, e) }
  | "{" e = expr_without_block "}" { BLOCK (EMPTY, Some e) }

expr_with_block:
  | "if" e0 = expr e1 = block_expr "else" e2 = block_expr { IFE(e0, e1, e2) }
  | "loop" e = block_expr { LOOP e }
  | e = block_expr { e }

expr_without_block:
  | "true" { TRUE }
  | "false" { FALSE }
  | "break" { BREAK }
  | "continue" { CONTINUE }
  | n = CONST { CONST (int_of_string n) }
  | s = STRING { STR (s |> String.split_on_char '\"' |> fun s -> List.nth s 1 ) }
  | x = ID { VAR x }
  | e1 = expr op = binop e2 = expr { ARITH2 (op, e1, e2) }
  | x = ID "=" e = expr { ASSIGN (x,e) }
  | x = ID "." f = ID "(" args = separated_list(COMMA, expr) ")" { CALL(f, (VAR x) :: args) }
  | x = ID option("!") "(" args = separated_list(COMMA, expr) ")" { CALL (x,args) }
  | "(" e = expr ")" { e }
  | "&" mut = boption("mut") e = expr { REF { mut; e } }

expr:
  | e = expr_with_block { e }
  | e = expr_without_block { e }

statement:
  | ";" { EMPTY }
  | "let" mut = boption("mut") name = ID "=" body = expr ";" { LET { name; mut; body } }
  | e = expr_without_block ";" { EXPR e }
  // | e = expr_with_block option(";") { EXPR e }
  | e = expr_with_block ";" { EXPR e }
  | e = expr_with_block { EXPR e } %prec WOSEP
  | i = item { i }
  | s1 = statement s2 = statement { SEQ (s1, s2) } %prec SS

%inline binop:
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { MOD }
  | "==" { EQ }
  | "<=" { LEQ }