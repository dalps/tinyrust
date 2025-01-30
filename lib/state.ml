open Ast
open Types
open Utils
open Errors

type ide = string [@@deriving show]
type loc = int [@@deriving show]
type fn_data = { pars : ide list; body : expr } [@@deriving show]

type ty = T_String | T_Ref of bool * ty | T_I32 | T_Bool | T_Unit
[@@deriving show]

type prim = PRINTLN [@@deriving show]

type stackval =
  | I32 of int
  | Bool of bool
  | Ref of bool * int
  | Unit
  | StringSlice of string
[@@deriving show]

type envval =
  | HeapVal of loc
  | StackVal of { mut : bool; mutable value : stackval }
  | Fn of fn_data
  | Prim of prim
[@@deriving show]

module Env = struct
  module H = Hashtbl.Make (String)
  include H
  type t = envval H.t

  let find env ide : envval trace_result =
    match H.find_opt env ide with
    | Some v -> Result.ok v
    | None -> Result.error (UnboundVar ide)
end

module OwnedLocation = struct
  type t = { loc : int; owner : ide }

  let equal o1 o2 = o1.loc = o2.loc && o1.owner = o2.owner
  let hash = Hashtbl.hash
end

module Heap = struct
  module H = Hashtbl.Make (OwnedLocation)
  include H
  type heap_value = string
  type t = heap_value H.t

  let find h loc owner : heap_value trace_result =
    let key : OwnedLocation.t = { loc; owner } in
    match find_opt h key with
    | Some v -> Result.ok v
    | None -> Result.error (MovedValue owner)

  let drop = H.remove
end

type env = Env.t
type mem = Heap.t

type state = {
          memory     : mem;
          envstack   : env Stack.t;
          toplevel   : env;
  mutable loop_level : int;
  mutable output     : string;
}
[@@deriving fields]
[@@ocamlformat "disable"]

let init () =
  let prelude = Env.create 99 in
  let envstack = Stack.create () in
  let memory = Heap.create 99 in
  Env.add prelude "println" (Prim PRINTLN);
  Stack.push prelude envstack;
  { memory; envstack; toplevel = prelude; loop_level = 0; output = "" }

let copy st =
  { st with envstack = Stack.copy st.envstack; memory = Heap.copy st.memory }

let topenv st = Stack.top st.envstack
let popenv st = Stack.drop st.envstack
let pushenv st env = Stack.push env st.envstack
let new_block_env st = pushenv st (topenv st)
let new_fn_env st = pushenv st (Env.copy st.toplevel)
let set_topenv st env =
  ignore (Stack.pop_opt st.envstack);
  Stack.push env st.envstack

let append_output st s = st.output <- st.output ^ s

let enter_loop st = st.loop_level <- st.loop_level + 1
let exit_loop st =
  let open Result in
  if st.loop_level > 0 then (
    st.loop_level <- st.loop_level - 1;
    ok ())
  else error NotInLoop

let get_var st x : stackval trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find env x in
  match v with
  | HeapVal loc ->
      let* value = Heap.find st.memory loc x in
      ok (StringSlice value)
  | StackVal { mut; value } -> ok value
  | _ -> error (TypeError (spr "%s is a function, but I expected a value" x))

let get_fn st x : [ `Fn of fn_data | `Prim of prim ] trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find env x in
  match v with
  | Fn data -> ok (`Fn data)
  | Prim prim -> ok (`Prim prim)
  | _ -> error (TypeError (spr "%s is a value, but I expected a function" x))

let new_var ~(mut : bool) st x v : unit trace_result =
  let env = topenv st in
  Env.add env x (StackVal { mut; value = v });
  Result.ok ()

let update_var st x v : unit trace_result =
  let open Result in
  let env = topenv st in
  let* res = Env.find env x in
  match res with
  | StackVal data ->
      if data.mut then ok (data.value <- v) else error (CannotMutate x)
  | _ -> error (TypeError (spr "cannot assign to the function %s" x))

let new_fn st name pars body = Env.add (topenv st) name (Fn { pars; body })

let state_of_prog (prog : statement list) : state =
  let st = init () in
  List.iter
    (function
      | FUNDECL data -> new_fn st data.name data.pars data.body
      | _ -> failwith "error initializing state: not a toplevel item")
    prog;
  { st with toplevel = Env.copy (topenv st) }
