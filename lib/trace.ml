open Ast
open Types
open State
open Utils
open Errors

type 'a trace_result = ('a, trace_error) result

type conf = LoopContinue | Break | Stop | Continue of statement
[@@deriving variants]

let expr_of_memval : memval -> expr = function
  | I32 i -> CONST i
  | Bool true -> TRUE
  | Bool false -> FALSE
  | Unit -> UNIT
  | Ref data -> REF { mut = data.mut; e = CONST data.loc }
  | Str s -> STRING s
  | String data -> STRING data.value

let memval_of_expr : expr -> memval trace_result =
  let open Types.Result in
  function
  | CONST n -> ok (I32 n)
  | STRING s -> ok (Str s)
  | UNIT -> ok Unit
  | _ -> error (TypeError "Not a value")

let ( let& )
    ((step, wrapper, e) : (expr -> expr trace_result) * (expr -> 'term) * expr)
    (action : expr -> 'term trace_result) : 'term trace_result =
  let open Types.Result in
  if is_value e then action e
  else
    let* e' = step e in
    return (wrapper e')

let ( let$ ) ((wrapper, s) : (statement -> 'term) * conf trace_result)
    (action : [ `Nothing | `Break | `LoopContinue ] -> 'term trace_result) :
    'term trace_result =
  let open Types.Result in
  let* s = s in
  match s with
  | Continue s -> return (wrapper s)
  | Stop -> action `Nothing
  | Break -> action `Break
  | LoopContinue -> action `LoopContinue

let rec trace1_expr st (e : expr) : expr trace_result =
  let open Types.Result in
  match e with
  | TRUE | FALSE | UNIT | CONST _ | STRING _ | BREAK | CONTINUE -> return e
  | VAR x ->
      let* res = State.get_var st x in
      expr_of_memval res |> return
  | ARITH2 (op, e1, e2) -> (
      match (e1, e2) with
      | CONST n1, CONST n2 -> return (apply_binop op n1 n2)
      | CONST _, e2 ->
          let* e2' = trace1_expr st e2 in
          return (ARITH2 (op, e1, e2'))
      | e1, _ ->
          let* e1' = trace1_expr st e1 in
          return (ARITH2 (op, e1', e2)))
  | ASSIGN (x, e) ->
      let& e' = (trace1_expr st, assign x, e) in
      let* v = memval_of_expr e' in
      let* _ = State.set_var st x v in
      return UNIT
  | BLOCK (s, e) ->
      State.new_block_env st;
      trace1_expr st (BLOCK_EXEC (s, e))
  | BLOCK_EXEC (s, e) -> (
      let* c = trace1_statement st s in
      match c with
      | Break -> return BREAK
      | LoopContinue -> return CONTINUE
      | Stop -> (
          match e with
          | None ->
              State.dropenv st;
              return UNIT
          | Some e -> return (BLOCK_RET e))
      | Continue s' -> return (BLOCK_EXEC (s', e)))
  | BLOCK_RET e ->
      let& v = (trace1_expr st, block_ret, e) in
      State.dropenv st;
      return v
  | CALL (f, args) ->
      pr "calling %s with %d args\n" f (List.length args);
      trace1_args st f [] args
  | IFE (e0, e1, e2) -> (
      let& v = (trace1_expr st, (fun e0' -> ife e0' e1 e2), e0) in
      match v with
      | TRUE -> return e1
      | FALSE -> return e2
      | _ -> error (TypeError "if guard not bool"))
  | LOOP (BLOCK (s, e)) ->
      let s =
        if s = EMPTY then
          match e with
          | Some e -> expr e
          | None -> expr UNIT
        else
          match e with
          | Some e -> seq s (expr e)
          | None -> s
      in
      State.new_block_env st;
      State.enter_loop st;
      trace1_expr st (loop_exec s s)
  | LOOP_EXEC e -> (
      let* c = trace1_statement st e.curr in
      match c with
      | Break ->
          let* _ = State.exit_loop st in
          State.dropenv st;
          return UNIT
      | Stop | LoopContinue -> return (loop_exec e.orig e.orig)
      | Continue s' -> return (LOOP_EXEC { e with curr = s' }))
  | REF { mut; e } ->
      let* e = trace1_expr st e in
      let _ =
        match e with
        | VAR x ->
            let* _ = if mut then borrow_mut st x else borrow st x in
            return ()
        | _ -> return ()
      in
      return (REF { mut; e })
  | _ -> error TODO

and trace1_args st (f : ide) (vals : expr list) (args : expr list) :
    expr trace_result =
  let open Types.Result in
  match args with
  | [] -> call_fun st f vals
  | v :: args when is_value v -> trace1_args st f (v :: vals) args
  | arg :: args ->
      let* arg' = trace1_expr st arg in
      return (CALL (f, vals @ (arg' :: args)))

and call_fun st (f : ide) (args : expr list) : expr trace_result =
  let open Types.Result in
  match (State.get_fn st f, args) with
  | Ok (`Fn { pars; body; ret }), _ ->
      State.new_fn_env st;
      let* _ =
        pr "Call parameters: want: %d, got: %d\n" (List.length pars)
          (List.length args);
        try
          List.fold_left2
            (fun _ par arg ->
              let* v = memval_of_expr arg in
              State.new_var ~mut:false st par v)
            (ok ()) pars args
        with _ -> error (MismatchedArgs f)
      in
      return (BLOCK_EXEC (body, ret))
  | Ok (`Prim PRINTLN), [ STRING s ] ->
      let* _ = Prim.println st s in
      ok UNIT
  | Ok (`Prim PUSH_STR), [ VAR x; STRING s2 ] ->
      let* s = Prim.push_str st x s2 in
      ok UNIT
  | Ok (`Prim _), _ -> error (MismatchedArgs f)
  | Error err, _ -> error err

and trace1_statement st (t : statement) : conf trace_result =
  let open Types.Result in
  match t with
  | EMPTY -> return stop
  | LET data ->
      let& v =
        (trace1_expr st, continue % let_stmt data.name data.mut, data.body)
      in
      let* v = memval_of_expr data.body in
      let* _ = State.new_var st data.name v ~mut:data.mut in
      return stop
  | FUNDECL data ->
      let* _ = State.new_fn st data.name data.pars data.body data.ret in
      return stop
  | EXPR e -> (
      let& b = (trace1_expr st, continue % expr, e) in
      match b with
      | BREAK -> return Break
      | CONTINUE -> return LoopContinue
      | _ -> return stop)
  | SEQ (s1, s2) ->
      let$ _ = ((continue % fun s1' -> seq s1' s2), trace1_statement st s1) in
      return (continue s2)

type snapshot = { expr : expr; state : state }

type trace_outcome = {
  result : expr trace_result;
  trace : snapshot list;
  state : state;
}

let entry_point = CALL ("main", [])

let trace_prog (n_steps : int) (prog : statement list) : trace_outcome =
  let open Types.Result in
  let st = State.state_of_prog prog in
  let rec go i (acc : snapshot list) expr : snapshot list * expr trace_result =
    if i < n_steps then
      match trace1_expr st expr with
      | Ok e' ->
          if is_value e' then (acc, ok e')
          else go (i + 1) ({ expr; state = State.copy st } :: acc) e'
      | err -> (acc, err)
    else (acc, error (OutOfGas n_steps))
  in
  let lst, result = go 0 [] entry_point in
  { state = st; trace = List.rev lst; result }
