open Ast
open Types

let pr = Printf.printf
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
  let eplus e1 e2 =
    match (e1, e2) with
    | CONST n1, CONST n2 -> CONST (n1 + n2)
    | _, _ -> PLUS (e1, e2)
  in
  function
  | UNIT -> return UNIT
  | CONST n -> return (CONST n)
  | STRING s -> return (STRING s)
  | VAR x -> (
      let$ st = get in
      match get_var st x with
      | V_String s -> return (STRING s)
      | V_I32 n -> return (CONST n)
      | V_Unit -> return UNIT)
  | PLUS (e1, e2) -> eplus <$> trace1_expr e1 <*> trace1_expr e2
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
      Stack.push (topenv st) (envstack st);
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
  | CALL (f, arg) ->
      let$ arg' = trace1_expr arg in
      if is_value arg' then
        let$ st = get in
        let env = topenv st in
        match env f with
        | Fun (pars, BLOCK (body, ret)) ->
            newenv st;
            let st' = bind_var st pars (get_value arg') in
            let$ _ = set st' in
            return (BLOCK_EXEC (body, ret))
        | Prim prim ->
            (match arg with STRING s -> println st s | _ -> ());
            return UNIT
        | _ -> type_fail @@ spr "Cannot call a non-function %s" f
      else return (CALL (f, arg'))
  | _ -> failwith "todo"

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
          let$ v = trace1_expr e in
          if is_value v then (
            let_var st x (get_value v) ~mut;
            return stop)
          else return (continue (LET (x, mut, e)))
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

let trace_item (p : statement) : state =
  let open WithState in
  let open Termination in
  let rec go (s : statement Termination.t) : statement Termination.t WithState.t
      =
    let$ s' = trace1_statement s in
    match s' with Stop -> return stop | Continue s' -> go (continue s')
  in
  let st, _ = go (continue p) state0 in
  st

let trace_prog (n_steps : int) (p : statement) : state * expr list =
  let open WithState in
  let open Termination in
  let state1 = trace_item p in
  let rec go (e : expr) : expr list WithState.t =
    let$ e' = trace1_expr e in
    if is_value e' then return [ e' ]
    else
      let$ rest = go e' in
      return (e :: rest)
  in
  go (CALL ("main", UNIT)) state1

open Parser

let p =
  parse_string
    {|
  fn main(x) {
    let mut x = 3;   // variabile immutabile di tipo intero
    let y = x+1;
    x = x+y;     // errore: x immutabile
    {println!("{x} bla bla {y}");}
  }
|}

let%expect_test "" = trace_prog 30 p |> ignore
