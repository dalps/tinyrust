open Ast
open State
open Utils
open Errors
open Trace

let tabs i =
  let tab = ANSITerminal.(sprintf [ black ] "..") in
  Seq.repeat tab |> Seq.take i |> List.of_seq |> String.concat ""

let keyword = ANSITerminal.(sprintf [ cyan; Bold ])
let value = ANSITerminal.(sprintf [ green ] "%s")

let string_of_var x = x

let string_of_binop (op : Ast.binop) =
  match op with
  | ADD -> "+"
  | MUL -> "*"
  | SUB -> "-"
  | MOD -> "%"
  | DIV -> "/"
  | EQ -> "=="
  | LEQ -> "<="

let rec string_of_expr indent (expr : Ast.expr) : string =
  match expr with
  | UNIT -> "()" |> value
  | TRUE -> "true" |> value
  | FALSE -> "false" |> value
  | VAR x -> string_of_var x
  | CONST n -> string_of_int n |> value
  | STRING s -> spr "\"%s\"" s |> value
  | ARITH2 (op, e1, e2) ->
      spr "%s %s %s" (string_of_expr indent e1) (string_of_binop op)
        (string_of_expr indent e2)
  | ASSIGN (x, e) -> spr "%s = %s" (string_of_var x) (string_of_expr indent e)
  | BLOCK (s, None) ->
      spr "{\n%s\n%s}" (string_of_statement (indent + 1) s) (tabs indent)
  | BLOCK (s, Some e) ->
      spr "{\n%s\n%s%s\n%s}"
        (string_of_statement (indent + 1) s)
        (tabs (indent + 1))
        (string_of_expr (indent + 1) e)
        (tabs indent)
  | BLOCK_EXEC (s, None) ->
      spr "{%s\n%s\n%s}"
        ANSITerminal.(sprintf [ red ] "exec")
        (string_of_statement (indent + 1) s)
        (tabs indent)
  | BLOCK_EXEC (s, Some e) ->
      spr "{%s\n%s\n%s%s\n%s}"
        ANSITerminal.(sprintf [ red ] "exec")
        (string_of_statement (indent + 1) s)
        (tabs (indent + 1))
        (string_of_expr (indent + 1) e)
        (tabs indent)
  | BLOCK_RET e -> string_of_expr indent e
  | CALL (x, es) ->
      let arg_strings =
        List.map (string_of_expr indent) es |> String.concat ","
      in
      spr "%s(%s)" (string_of_var x) arg_strings
  | IFE (e0, e1, e2) ->
      spr "%s %s %s %s %s" (keyword "if") (string_of_expr indent e0)
        (string_of_expr indent e1) (keyword "else") (string_of_expr indent e2)
  | LOOP e -> spr "%s %s" (keyword "loop") (string_of_expr indent e)
  | LOOP_EXEC data ->
      spr "%s {%s\n%s\n%s}" (keyword "loop")
        ANSITerminal.(sprintf [ red ] "exec")
        (string_of_statement (indent + 1) data.curr)
        (tabs indent)
  | REF e ->
      spr "&%s%s"
        (if e.mut then keyword "mut " else "")
        (string_of_expr indent e.e)
  | BREAK -> keyword "break"
  | _ -> "?"

and string_of_statement indent (stmt : Ast.statement) =
  match stmt with
  | FUNDECL data ->
      spr "%s%s %s(%s) {\n%s\n%s}" (tabs indent) (keyword "fn") data.name
        (String.concat ", " data.pars)
        (string_of_statement (indent + 1) data.body)
        (Option.fold ~none:""
           ~some:(fun e ->
             spr "%s%s\n" (tabs (indent + 1)) (string_of_expr (indent + 1) e))
           data.ret)
  | LET data ->
      spr "%s%s %s%s = %s;" (tabs indent) (keyword "let")
        (if data.mut then "mut " else "")
        data.name
        (string_of_expr indent data.body)
  | SEQ (s1, s2) ->
      let s1 = string_of_statement indent s1 in
      let s2 = string_of_statement indent s2 in
      spr "%s\n%s" s1 s2
  | EXPR e -> spr "%s%s;" (tabs indent) (string_of_expr indent e)
  | EMPTY -> spr "%s" (tabs indent)

let string_of_memval = function
  | I32 i -> string_of_int i
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Str s -> spr "\"%s\"" s
  | Ref data -> spr "&(%d)" data.loc
  | String data -> spr "\"%s\"" data.value

let string_of_prim = function
  | PRINTLN -> "println"
  | PUSH_STR -> "push_str"

let string_of_envval = function
  | Loc data -> spr "%s%d" (if data.mut then "mut " else "") data.loc
  | Fn _ -> "<fn>"
  | Prim _ -> "<prim>"

let string_of_env env =
  Env.fold (fun k v acc -> spr "%s/%s" k (string_of_envval v) :: acc) env []
  |> String.concat ", "

let string_of_envstack envstack =
  Stack.fold
    (fun acc env -> spr "[ %s ]" (string_of_env env) :: acc)
    [] envstack
  |> String.concat "\n"

let string_of_memory h =
  spr "{ %s }"
    (Mem.fold (fun k v acc -> spr "%d/%s" k (string_of_memval v) :: acc) h []
    |> String.concat ", ")

let string_of_state st =
  spr "%s%s\n%s\n\n%s\n%s\n"
    (if st.loop_level <> 0 then
       spr "%s%d\n"
         ANSITerminal.(sprintf [ blue; Bold ] "Loop level: ")
         (st.loop_level)
     else "")
    ANSITerminal.(sprintf [ blue; Bold ] "Envstack:")
    (string_of_envstack st.envstack)
    ANSITerminal.(sprintf [ blue; Bold ] "Memory:")
    (string_of_memory st.memory)

let string_of_traceoutcome (d : trace_outcome) =
  List.mapi
    (fun i (s : snapshot) ->
      spr "%s\n%s\n%s\n%s"
        ANSITerminal.(sprintf [ red ] "--- Step %d ---" i)
        (string_of_state s.state)
        ANSITerminal.(sprintf [ blue; Bold ] "Program:")
        (string_of_expr 0 s.expr))
    d.trace
  |> String.concat "\n"

let string_of_trace_error = function
  | TypeError s -> spr "[TypeError] %s" s
  | CannotMutate x -> spr "[CannotMutate] cannot mutate immutable variable %s" x
  | UnboundVar x -> spr "[UnboundVar] %s not defined in this scope" x
  | MovedValue x -> spr "[MovedValue] borrow of moved value %s" x
  | OutOfGas i -> spr "[OutOfGas] trace run out of gas (%d)" i
  | NotInLoop -> "[NotInLoop] tried to break outside of a loop"
  | MutBorrowOfNonMut x ->
      spr
        "[MutBorrowOfNonMut] cannot borrow %s as mutable, as it is not \
         declared as mutable"
        x
  | DataRace data ->
      let format_mut = function
        | `mut -> "mutable"
        | `imm -> "immutable"
      in
      spr "[DataRace] cannot borrow %s as %s because it is also borrowed as %s"
        data.borrowed (format_mut data.is) (format_mut data.want)
  | SegFault loc -> spr "[SegFault] Illegal access at %d" loc
  | MismatchedArgs ide -> spr "[MismatchedArgs] %s" ide
  | TODO -> "[TODO]"
