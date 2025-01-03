open Ast
open Types
open State
open Utils

type conf = Stop | Continue of statement [@@deriving variants]

let expr_of_stackval : stackval -> expr = function
  | I32 i -> CONST i
  | Bool true -> TRUE
  | Bool false -> FALSE
  | Unit -> UNIT
  | Ref (m, i) -> REF (m, CONST i)
  | String s -> STRING s

let stackval_of_expr : expr -> stackval trace_result =
  let open R in
  function
  | CONST n -> ok (I32 n)
  | STRING s -> ok (String s)
  | UNIT -> ok Unit
  | _ -> error (TypeError "Not a value")

let ( let& ) ((constr, e) : (expr -> 'term) * expr trace_result)
    (action : expr -> 'term trace_result) : 'term trace_result =
  let open R in
  let* e = e in
  if is_value e then action e else return (constr e)

let ( let$ ) ((constr, s) : (statement -> 'term) * conf trace_result)
    (action : unit -> 'term trace_result) : 'term trace_result =
  let open R in
  let* s = s in
  match s with
  | Continue s -> return (constr s)
  | Stop -> action ()

let rec trace1_expr (st : state) (e : expr) : expr trace_result =
  let open R in
  match e with
  | TRUE | FALSE | UNIT | CONST _ | STRING _ -> return e
  | VAR x ->
      let* res = St.get_var st x in
      expr_of_stackval res |> return
  | ARITH2 (op, e1, e2) -> arith2 op <$> trace1_expr st e1 <*> trace1_expr st e2
  | ASSIGN (x, e) ->
      let& e' = (assign x, trace1_expr st e) in
      let* v = stackval_of_expr e' in
      let* _ = St.bind_var st x v in
      return UNIT
  | BLOCK (s, e) ->
      St.pushenv st (St.topenv st);
      return (BLOCK_EXEC (s, e))
  | BLOCK_EXEC (s, e) -> (
      let$ _ = ((fun s' -> block_exec s' e), trace1_statement st s) in
      match e with
      | None ->
          St.popenv st;
          return UNIT
      | Some e -> return (BLOCK_RET e))
  | BLOCK_RET e ->
      let& v = (block_ret, trace1_expr st e) in
      St.popenv st;
      return v
  | CALL (f, args) -> trace1_args st f [] args
  | IFE (e0, e1, e2) -> (
      let& v = ((fun e0' -> ife e0' e1 e2), trace1_expr st e0) in
      match v with
      | TRUE -> return e1
      | FALSE -> return e2
      | _ -> error (TypeError "if guard not bool"))
  | LOOP (BLOCK (s, e), _) ->
      St.newenv st;
      St.enter_loop st;
      return (loop (LOOP_EXEC (s, e)) (LOOP_EXEC (s, e)))
  | LOOP (original, e) ->
      let& _ = (loop original, trace1_expr st e) in
      return (loop original original)
  | BREAK ->
      let* _ = St.exit_loop st in
      St.popenv st;
      return UNIT
  | _ -> error TODO

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
  let env = St.topenv st in
  let* res = Env.get env f in
  match res with
  | Fun (pars, BLOCK (body, ret)) ->
      St.pushenv st St.prelude;
      let* _ =
        List.fold_left2
          (fun _ par arg ->
            let* v = stackval_of_expr arg in
            St.bind_var st par v)
          (ok ()) pars args
      in
      return (BLOCK_EXEC (body, ret))
  | Prim prim -> (
      match args with
      | [ STRING s ] ->
          let* _ = Prim.println st s in
          ok UNIT
      | _ -> ok UNIT)
  | _ -> error (TypeError (spr "Cannot call a non-function %s" f))

and trace1_statement (st : state) (t : statement) : conf trace_result =
  let open R in
  match t with
  | EMPTY -> return stop
  | LET (x, mut, e) ->
      let& v = (continue % let_stmt x mut, trace1_expr st e) in
      let* v = stackval_of_expr v in
      let* _ = St.let_var st x v ~mut in
      return stop
  | FUNDECL (x, pars, body) ->
      let env' = Env.bind (St.topenv st) x (Fun (pars, body)) in
      St.set_topenv st env';
      return stop
  | EXPR e ->
      let& v = (continue % expr, trace1_expr st e) in
      return stop
  | SEQ (s1, s2) ->
      let$ _ = ((continue % fun s1' -> seq s1' s2), trace1_statement st s1) in
      return (continue s2)

let read_toplevel (st : state) (s : statement) : unit =
  match s with
  | FUNDECL (x, pars, body) ->
      let env' = Env.bind (St.topenv st) x (Fun (pars, body)) in
      St.set_topenv st env'
  | _ -> failwith "Not a toplevel item"

let trace_prog (n_steps : int) (prog : statement list) :
    state * expr list * expr trace_result =
  let open R in
  let st = St.init () in
  List.iter (read_toplevel st) prog;
  let st = { st with toplevel = St.topenv st } in
  let rec go i (acc : expr list) (e : expr) : expr list * expr trace_result =
    if i < n_steps then
      match trace1_expr st e with
      | Ok e' -> if is_value e' then (acc, ok e') else go (i + 1) (e :: acc) e'
      | err -> (acc, err)
    else (acc, error (OutOfGas n_steps))
  in
  let entry = CALL ("main", []) in
  let lst, res = go 0 [ entry ] entry in
  (st, List.rev lst, res)
