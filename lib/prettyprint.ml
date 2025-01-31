open Ast
open State
open Utils
open Errors
open Trace

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
  | UNIT -> "()"
  | TRUE -> "true"
  | FALSE -> "false"
  | VAR x -> string_of_var x
  | CONST n -> string_of_int n
  | STRING s -> s
  | ARITH2 (op, e1, e2) ->
      spr "%s %s %s" (string_of_expr indent e1) (string_of_binop op)
        (string_of_expr indent e2)
  | ASSIGN (x, e) -> spr "%s = %s" (string_of_var x) (string_of_expr indent e)
  | BLOCK (s, None) -> spr "{\n%s\n}\n" (string_of_statement (indent + 1) s)
  | BLOCK (s, Some e) ->
      spr "{\n%s;\n%s\n}\n"
        (string_of_statement (indent + 1) s)
        (string_of_expr indent e)
  | BLOCK_EXEC (s, None) ->
      spr "[exec]{\n%s\n}\n" (string_of_statement (indent + 1) s)
  | BLOCK_EXEC (s, Some e) ->
      spr "[exec]{\n%s;\n%s\n}\n"
        (string_of_statement (indent + 1) s)
        (string_of_expr indent e)
  | BLOCK_RET e -> string_of_expr indent e
  | CALL (x, es) ->
      let arg_strings =
        List.map (string_of_expr indent) es |> String.concat ","
      in
      spr "%s(%s)" (string_of_var x) arg_strings
  | IFE (e0, e1, e2) ->
      spr "if %s {\n%s\n} else {\n%s\n}" (string_of_expr indent e0)
        (string_of_expr (indent + 1) e1)
        (string_of_expr (indent + 1) e2)
  | LOOP e -> spr "loop {\n%s\n}" (string_of_expr indent e)
  | LOOP_EXEC e ->
      spr "loop [exec]{\n%s\n}" (string_of_statement (indent + 1) e.curr)
  | REF e ->
      spr "&%s%s" (if e.mut then "mut " else "") (string_of_expr indent e.e)
  | BREAK -> "break"
  | _ -> "?"

and string_of_statement indent (stmt : Ast.statement) =
  let tabs i =
    Seq.repeat "  " |> Seq.take i |> List.of_seq |> String.concat ""
  in
  match stmt with
  | FUNDECL data ->
      spr "%sfn %s(%s) {\n%s\n}" (tabs indent) data.name
        (String.concat ", " data.pars)
        (string_of_expr (indent + 1) data.body)
  | LET data ->
      spr "%slet %s%s = %s;" (tabs indent)
        (if data.mut then "mut " else "")
        data.name
        (string_of_expr indent data.body)
  | SEQ (s1, s2) ->
      spr "%s%s\n%s%s" (tabs indent)
        (string_of_statement indent s1)
        (tabs indent)
        (string_of_statement indent s2)
  | EXPR e -> spr "%s%s;" (tabs indent) (string_of_expr indent e)
  | EMPTY -> ""

let string_of_stackval = function
  | I32 i -> string_of_int i
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | StringSlice s -> spr "\"%s\"" s
  | Ref (_, i) -> spr "ref: %d" i

let string_of_envval = function
  | HeapVal loc -> spr "%s" (string_of_int loc)
  | StackVal { mut; value } ->
      spr "%s%s" (if mut then "mut " else "") (string_of_stackval value)
  | Fn _ -> "<fn>"
  | Prim _ -> "<prim>"

let string_of_env env =
  Env.fold (fun k v acc -> spr "%s->%s" k (string_of_envval v) :: acc) env []
  |> String.concat ", "

let string_of_envstack envstack =
  Stack.fold (fun acc env -> spr "[%s]" (string_of_env env) :: acc) [] envstack
  |> String.concat "\n"

let string_of_ownedlocation (o : OwnedLocation.t) = spr "%d@%s" o.loc o.owner

let string_of_memory h =
  Heap.fold
    (fun k v acc -> spr "%s->%s" (string_of_ownedlocation k) v :: acc)
    h []
  |> String.concat ", "

let string_of_state st =
  spr "%s\n%s\n%s\n%s\n"
    ANSITerminal.(sprintf [ blue; Bold ] "Memory:")
    (string_of_memory st.memory)
    ANSITerminal.(sprintf [ blue; Bold ] "Envstack:")
    (string_of_envstack st.envstack)

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
  | DataRace (x, mut1, mut2) ->
      let format_mut m = if m then "mutable" else "immutable" in
      spr "[DataRace] cannot borrow %s as %s because it is also borrowed as %s"
        x (format_mut mut1) (format_mut mut2)
  | TODO -> "[TODO]"
