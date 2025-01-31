open Ast
open Types
open Utils
open Errors

(* Should the borrow type be carried by the environment or by the reference? *)
type ide = string [@@deriving show]
type loc = {
  mut : bool;
  loc : [ `stack of int | `heap of int ];
  borrows : [ `imm of ide list | `mut of ide ];
}
[@@deriving show]
type fn_data = { pars : ide list; body : statement; ret : expr option }
[@@deriving show]

type ty = T_String | T_Ref of bool * ty | T_I32 | T_Bool | T_Unit
[@@deriving show]

type prim = PRINTLN | PUSH_STR [@@deriving show]

type stackval =
  | I32 of int
  | Bool of bool
  | Ref of bool * loc
  | Unit
  | Str of string
[@@deriving show]

type 'v owned = { value : 'v; owner : ide }

type envval = Loc of loc | Fn of fn_data | Prim of prim [@@deriving show]

module Env = struct
  module H = Map.Make (String)
  include H
  type t = envval H.t

  let find ide env : envval trace_result =
    match H.find_opt ide env with
    | Some v -> Result.ok v
    | None -> Result.error (UnboundVar ide)
end

module OwnedLocation = struct
  type t = { loc : int; owner : ide }

  let compare o1 o2 =
    let loc_cpm = compare o1.loc o2.loc in
    if loc_cpm = 0 then compare o1.owner o2.owner else loc_cpm
  let equal o1 o2 = o1.loc = o2.loc && o1.owner = o2.owner
  let hash = Hashtbl.hash
end

module Heap = struct
  module H = Map.Make (OwnedLocation)
  include H
  type heap_value = string
  type t = heap_value H.t

  let find h loc owner : heap_value trace_result =
    let key : OwnedLocation.t = { loc; owner } in
    match find_opt key h with
    | Some v -> Result.ok v
    | None -> Result.error (MovedValue owner)

  let drop = H.remove
end

module Mem = struct
  module M = Map.Make (Int)
  include M
  type t = stackval M.t

  let find m loc : stackval trace_result =
    find_opt loc m |> Option.to_result ~none:(SegFault loc)
end

type env = Env.t
type mem = Mem.t
type heap = Heap.t

type state = {
  mutable memory     : mem;
  mutable heap       : heap;
          envstack   : env Stack.t;
          toplevel   : env;
  mutable loop_level : int;
  mutable output     : string;
  mutable locs       : unit -> int
}
[@@deriving fields]
[@@ocamlformat "disable"]

let init () =
  let envstack = Stack.create () in
  let prelude =
    Env.of_list [ ("println", Prim PRINTLN); ("push_str", Prim PUSH_STR) ]
  in
  let memory = Mem.empty in
  let heap = Heap.empty in
  let counter = ref 0 in
  let locs () =
    let c = !counter in
    incr counter;
    c
  in
  let _f = Seq.(ints 0 |> to_dispenser) in
  Stack.push prelude envstack;
  {
    memory;
    heap;
    envstack;
    toplevel = prelude;
    loop_level = 0;
    output = "";
    locs;
  }

let copy st = { st with envstack = Stack.copy st.envstack }
let topenv st = Stack.top st.envstack
let popenv st = Stack.pop st.envstack
let dropenv st = Stack.drop st.envstack
let pushenv st env = Stack.push env st.envstack
let update_topenv st (update : env -> env trace_result) : unit trace_result =
  let open Types.Result in
  let env = popenv st in
  let* env' = update env in
  ok (pushenv st env')
let new_block_env st = pushenv st (topenv st)
let new_fn_env st = pushenv st st.toplevel
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

let borrow st x bob : loc trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find x env in
  match v with
  | Loc ({ borrows = `imm xs } as data) ->
      let data = { data with borrows = `imm (bob :: xs) } in
      let* _ = update_topenv st (fun env -> ok (Env.add x (Loc data) env)) in
      ok data
  | Loc { borrows = `mut _ } ->
      error (DataRace { borrowed = x; is = `imm; want = `mut })
  | _ -> error (TypeError "Non-value cannot be referenced")

let borrow_mut st x bob : loc trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find x env in
  match v with
  | Loc ({ borrows = `imm [] } as data) ->
      let data = { data with borrows = `mut bob } in
      let* _ = update_topenv st (fun env -> ok (Env.add x (Loc data) env)) in
      ok data
  | Loc _ -> error (DataRace { borrowed = x; is = `mut; want = `mut })
  | _ -> error (TypeError "Non-value cannot be referenced")

let get_var st x : stackval trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find x env in
  match v with
  | Loc { loc = `heap addr } ->
      let* value = Heap.find st.heap addr x in
      ok (Str value)
  | Loc { loc = `stack addr } -> Mem.find st.memory addr
  | _ -> error (TypeError (spr "%s is a function, but I expected a value" x))

let get_fn st x : [ `Fn of fn_data | `Prim of prim ] trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find x env in
  match v with
  | Fn data -> ok (`Fn data)
  | Prim prim -> ok (`Prim prim)
  | _ -> error (TypeError (spr "%s is a value, but I expected a function" x))

let new_var ~(mut : bool) st x init : unit trace_result =
  let open Result in
  let loc = st.locs () in
  let* _ =
    update_topenv st (fun env ->
        let loc =
          {
            mut;
            loc =
              (match init with
              | `copy v ->
                  st.memory <- Mem.add loc v st.memory;
                  `stack loc
              | `owned v ->
                  st.heap <- Heap.add { loc; owner = x } v st.heap;
                  `heap loc);
            borrows = `imm [];
          }
        in
        Result.ok (Env.add x (Loc loc) env))
  in
  ok ()

(* Assignment moves r. Problem: change r's owner, but the owner is associated to locs, not value
*)
let set_var st l r : unit trace_result =
  let open Result in
  let env = topenv st in
  let* res = Env.find l env in
  match res with
  | Loc { mut; loc } ->
      if mut then (
        st.memory <- Mem.add loc r st.memory;
        ok ())
      else error (CannotMutate l)
  | _ -> error (TypeError (spr "cannot assign to the function %s" l))

let new_fn st name pars body ret =
  update_topenv st (fun env ->
      Result.ok (Env.add name (Fn { pars; body; ret }) env))

let state_of_prog (prog : statement list) : state =
  let st = init () in
  List.iter
    (function
      | FUNDECL data ->
          new_fn st data.name data.pars data.body data.ret |> ignore
      | _ -> failwith "error initializing state: not a toplevel item")
    prog;
  { st with toplevel = topenv st }
