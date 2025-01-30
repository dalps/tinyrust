open Ast
open State
open Utils
open Errors
open Trace

let join sep strings =
  let rec go = function
    | [] -> ""
    | [ s ] -> s
    | s :: ss -> s ^ sep ^ go ss
  in
  go strings

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

let rec string_of_expr (expr : Ast.expr) : string =
  match expr with
  | UNIT -> "()"
  | TRUE -> "true"
  | FALSE -> "false"
  | VAR x -> string_of_var x
  | CONST n -> string_of_int n
  | STRING s -> s
  | ARITH2 (op, e1, e2) ->
      spr "%s %s %s" (string_of_expr e1) (string_of_binop op)
        (string_of_expr e2)
  | ASSIGN (x, e) -> spr "%s = %s" (string_of_var x) (string_of_expr e)
  | BLOCK (s, None) -> spr "{\n%s\n}\n" (string_of_statement s)
  | BLOCK (s, Some e) ->
      spr "{\n%s;\n%s\n}\n" (string_of_statement s) (string_of_expr e)
  | BLOCK_EXEC (s, None) -> spr "[exec]{\n%s\n}\n" (string_of_statement s)
  | BLOCK_EXEC (s, Some e) ->
      spr "[exec]{\n%s;\n%s\n}\n" (string_of_statement s) (string_of_expr e)
  | BLOCK_RET e -> string_of_expr e
  | CALL (x, es) ->
      let arg_strings = List.map string_of_expr es |> join "," in
      spr "%s(%s)" (string_of_var x) arg_strings
  | IFE (e0, e1, e2) ->
      spr "if %s {\n%s\n} else {\n%s\n}" (string_of_expr e0) (string_of_expr e1)
        (string_of_expr e2)
  | LOOP e -> spr "loop {\n%s\n}" (string_of_expr e)
  | LOOP_EXEC e -> spr "loop [exec]{\n%s\n}" (string_of_statement e.curr)
  | REF e -> spr "&%s%s" (if e.mut then "mut " else "") (string_of_expr e.e)
  | BREAK -> "break"
  | _ -> "?"

and string_of_statement (stmt : Ast.statement) =
  match stmt with
  | FUNDECL data ->
      spr "fn %s(%s) {\n%s\n}" data.name (join ", " data.pars)
        (string_of_expr data.body)
  | LET data ->
      spr "let %s%s = %s;"
        (if data.mut then "mut " else "")
        data.name (string_of_expr data.body)
  | SEQ (s1, s2) ->
      spr "%s\n%s" (string_of_statement s1) (string_of_statement s2)
  | EXPR e -> spr "%s;" (string_of_expr e)
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
  Env.fold (fun k v acc -> spr "{%s:%s}" k (string_of_envval v) :: acc) env []
  |> join ", "

let string_of_envstack envstack =
  Stack.fold (fun acc env -> spr "[%s]" (string_of_env env) :: acc) [] envstack
  |> join ", "

let string_of_ownedlocation (o : OwnedLocation.t) = spr "%d@%s" o.loc o.owner

let string_of_memory h =
  Heap.fold
    (fun k v acc -> spr "%s->%s" (string_of_ownedlocation k) v :: acc)
    h []
  |> join ", "

let string_of_state st =
  spr "<%s/%s>" (string_of_memory st.memory) (string_of_envstack st.envstack)

let string_of_traceoutcome (d : trace_outcome) =
  List.map
    (fun (s : snapshot) ->
      spr "%s\n%s" (string_of_state s.state) (string_of_expr s.expr))
    d.trace
  |> join "###\n"

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
