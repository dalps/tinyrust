open Ast
open State
open Utils

let join sep strings =
  let rec go = function
    | [] -> ""
    | [ s ] -> s
    | s :: ss -> s ^ sep ^ go ss
  in
  go strings

let string_of_var x = x
let rec string_of_expr (expr : Ast.expr) : string =
  match expr with
  | UNIT -> "()"
  | TRUE -> "true"
  | FALSE -> "false"
  | VAR x -> string_of_var x
  | CONST n -> string_of_int n
  | STRING s -> s
  | ARITH2 (op, e1, e2) ->
      string_of_expr e1 ^ string_of_binop op ^ string_of_expr e2
  | ASSIGN (x, e) -> spr "%s = %s" (string_of_var x) (string_of_expr e)
  | BLOCK (s, None) -> spr "{\n%s\n}\n" (string_of_statement s)
  | BLOCK (s, Some e) ->
      spr "{\n%s;\n%s\n}\n" (string_of_statement s) (string_of_expr e)
  | BLOCK_EXEC (s, None) -> spr "%%exec{\n%s\n}\n" (string_of_statement s)
  | BLOCK_EXEC (s, Some e) ->
      spr "%exec{\n%s\n}\n" (string_of_statement s) (string_of_expr e)
  | BLOCK_RET e -> string_of_expr e
  | CALL (x, es) ->
      let arg_strings = List.map string_of_expr es |> join "," in
      spr "%s(%s)" (string_of_var x) arg_strings
  | IFE (e0, e1, e2) ->
      spr "if %s {\n%s\n} else {\n%s\n}" (string_of_expr e0) (string_of_expr e1)
        (string_of_expr e2)
  | LOOP e -> spr "loop {\n%s\n}" (string_of_expr e)
  | LOOP_EXEC e -> spr "loop %%exec{\n%s\n}" (string_of_statement e.curr)
  | REF e -> spr "&%s %s" (if e.mut then "mut" else "") (string_of_expr e.e)
  | BREAK -> "break"
  | _ -> "?"

and string_of_statement (stmt : Ast.statement) =
  match stmt with
  | FUNDECL (_, _, _) -> _
  | LET (_, _, _) -> _
  | SEQ (s1, s2) ->
      spr "%s;\n%s" (string_of_statement s1) (string_of_statement s2)
  | EXPR e -> spr "%s;" (string_of_expr e)
  | EMPTY -> ";"
