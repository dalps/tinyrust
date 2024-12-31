open Ast
open Types
open State
open Utils

type conf = Stop | Continue of statement [@@deriving variants]

let rec trace1_expr (st : State.state) (e : expr) : expr trace_result =
  let open R in
  match e with
  | UNIT -> return UNIT
  | CONST n -> return (CONST n)
  | STRING s -> return (STRING s)
  | VAR x -> get_var st x |> expr_of_memval |> return
  | ARITH2 (op, e1, e2) -> arith2 op <$> trace1_expr st e1 <*> trace1_expr st e2
  | ASSIGN (x, e) ->
      (* assign x <$> (trace1_expr st e) *)
      let* e' = trace1_expr st e in
      if is_value e' then (
        let v = get_value e' in
        State.bind_var st x v |> ignore;
        return UNIT)
      else return (ASSIGN (x, e'))
  | BLOCK (s, e) ->
      pushenv st (topenv st);
      return (BLOCK_EXEC (s, e))
  | BLOCK_EXEC (s, e) -> (
      let* s' = trace1_statement st s in
      match s' with
      | Stop -> (
          match e with
          | None ->
              popenv st;
              return UNIT
          | Some e -> return (BLOCK_RET e))
      | Continue s' -> return (BLOCK_EXEC (s', e)))
  | BLOCK_RET e ->
      let* e' = trace1_expr st e in
      if is_value e' then (
        popenv st;
        return e')
      else return (BLOCK_RET e')
  | CALL (f, args) -> trace1_args st f [] args
  | _ -> failwith "todo"

and trace1_args (st : state) (f : ide) (vals : expr list) (args : expr list) :
    expr trace_result =
  let open R in
  match args with
  | [] -> call_fun st f vals
  | v :: args when is_value v -> trace1_args st f (v :: vals) args
  | arg :: args ->
      let* arg' = trace1_expr st arg in
      return (CALL (f, arg' :: args))

and call_fun (st : state) (f : ide) (args : expr list) : expr trace_result =
  let open R in
  let env = topenv st in
  match env f with
  | Fun (pars, BLOCK (body, ret)) ->
      pushenv st (module_env st);
      List.iter2
        (fun par arg -> State.bind_var st par (get_value arg) |> ignore)
        pars args;
      return (BLOCK_EXEC (body, ret))
  | Prim prim ->
      (match args with [ STRING s ] -> println st s | _ -> ());
      return UNIT
  | _ -> type_fail @@ spr "Cannot call a non-function %s" f

and trace1_statement (st : state) (t : statement) : conf trace_result =
  let open R in
  match t with
  | EMPTY -> return stop
  | LET (x, mut, e) ->
      let* e' = trace1_expr st e in
      if is_value e' then (
        State.let_var st x (get_value e') ~mut |> ignore;
        return stop)
      else return (continue (LET (x, mut, e')))
  | FUNDECL (x, pars, body) ->
      let env = topenv st in
      let env' = bindf env x (Fun (pars, body)) in
      set_topenv st env';
      return stop
  | EXPR e ->
      let* e' = trace1_expr st e in
      if is_value e' then return stop else return (continue (EXPR e'))
  | SEQ (s1, s2) -> (
      let* s1' = trace1_statement st s1 in
      match s1' with
      | Stop -> return (continue s2)
      | Continue s1' -> return (continue @@ SEQ (s1', s2)))

let trace_item (st : state) (s : statement) : state trace_result =
  let open R in
  let rec go (st : state) (s : statement) : unit trace_result =
    let* s' = trace1_statement st s in
    match s' with Stop -> return () | Continue s' -> go st s'
  in
  let* _ = go st s in
  return st

let trace_prog (n_steps : int) (prog : statement list) : expr list trace_result
    =
  let open R in
  let st = State.init_state () in
  List.iter (ignore % trace_item st) prog;
  let st = State.({ st with module_env = topenv st }) in
  let rec go i (e : expr) : expr list trace_result =
    if i < n_steps then
      let* e' = trace1_expr st e in
      if is_value e' then return [ e' ]
      else
        let* rest = go (i + 1) e' in
        return (e :: rest)
    else return [ e ]
  in
  go 0 (CALL ("main", []))
