open Ast
open Types
open Utils

let spr = Printf.sprintf

module TraceResult = struct
  type ('a, 'state) t = Done of 'state | Continue of 'a * 'state

  let bind result next =
    match result with Done st -> Done st | Continue (s, st) -> next (s, st)

  let ( let* ) = bind
end

module Termination = struct
  type 'a t = Stop | Continue of 'a [@@deriving variants]

  let bind t next = match t with Stop -> Stop | Continue a -> next a

  let ( let<!> ) = bind
end

let rec trace1_expr : expr -> expr WithState.t =
  let open WithState in
  function
  | UNIT -> return UNIT
  | CONST n -> return (CONST n)
  | STRING s -> return (STRING s)
  | VAR x ->
      let$ st = get in
      get_var st x |> expr_of_memval |> return
  | ARITH2 (op, e1, e2) -> arith2 op <$> trace1_expr e1 <*> trace1_expr e2
  | ASSIGN (x, e) ->
      let$ e' = trace1_expr e in
      if is_value e' then
        let$ st = get in
        let v = get_value e' in
        let st' = bind_var st x v in
        let$ _ = set st' in
        return UNIT
      else return (ASSIGN (x, e'))
  | BLOCK (s, e) ->
      let$ st = get in
      pushenv st (topenv st);
      return (BLOCK_EXEC (s, e))
  | BLOCK_EXEC (s, e) -> (
      let$ s' = trace1_statement (Continue s) in
      match s' with
      | Stop -> (
          match e with
          | None ->
              let$ st = get in
              popenv st;
              return UNIT
          | Some e -> return (BLOCK_RET e))
      | Continue s' -> return (BLOCK_EXEC (s', e)))
  | BLOCK_RET e ->
      let$ e' = trace1_expr e in
      if is_value e' then (
        let$ st = get in
        popenv st;
        return e')
      else return (BLOCK_RET e')
  | CALL (f, args) -> trace1_args f [] args
  | _ -> failwith "todo"

and trace1_args f vals args : expr WithState.t =
  let open WithState in
  match args with
  | [] -> call_fun f vals
  | v :: args when is_value v -> trace1_args f (v :: vals) args
  | arg :: args ->
      let$ arg' = trace1_expr arg in
      return (CALL (f, arg' :: args))

and call_fun f args : expr WithState.t =
  let open WithState in
  let$ st = get in
  let env = topenv st in
  match env f with
  | Fun (pars, BLOCK (body, ret)) ->
      pushenv st (module_env st);
      List.iter2
        (fun par arg -> bind_var st par (get_value arg) |> ignore)
        pars args;
      return (BLOCK_EXEC (body, ret))
  | Prim prim ->
      (match args with [ STRING s ] -> println st s | _ -> ());
      return UNIT
  | _ -> type_fail @@ spr "Cannot call a non-function %s" f

and trace1_statement (t : statement Termination.t) :
    statement Termination.t WithState.t =
  let open Termination in
  let open WithState in
  let$ st = get in
  match t with
  | Stop -> return stop
  | Continue t -> (
      match t with
      | EMPTY -> return stop
      | LET (x, mut, e) ->
          let$ e' = trace1_expr e in
          if is_value e' then
            let _ = set @@ let_var st x (get_value e') ~mut in
            return stop
          else return (continue (LET (x, mut, e')))
      | FUNDECL (x, pars, body) ->
          let env = topenv st in
          let env' = bindf env x (Fun (pars, body)) in
          set_topenv st env';
          return stop
      | EXPR e ->
          let$ e' = trace1_expr e in
          if is_value e' then return stop else return (continue (EXPR e'))
      | SEQ (s1, s2) -> (
          let$ s1' = trace1_statement (continue s1) in
          match s1' with
          | Stop -> return (continue s2)
          | Continue s1' -> return (continue @@ SEQ (s1', s2))))

let trace_item state0 (p : statement) : state =
  let open WithState in
  let open Termination in
  let rec go (s : statement Termination.t) : statement Termination.t WithState.t
      =
    let$ s' = trace1_statement s in
    match s' with Stop -> return stop | Continue s' -> go (continue s')
  in
  let st, _ = go (continue p) state0 in
  st

let trace_prog (n_steps : int) (p : statement list) : state * expr list =
  let open WithState in
  let open Termination in
  let state1 = List.fold_left trace_item state0 p in
  let state2 = { state1 with module_env = topenv state1 } in
  let rec go i (e : expr) : expr list WithState.t =
    if i < n_steps then
      let$ e' = trace1_expr e in
      if is_value e' then return [ e' ]
      else
        let$ rest = go (i + 1) e' in
        return (e :: rest)
    else return [ e ]
  in
  go 0 (CALL ("main", [])) state2
