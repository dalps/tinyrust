open Ast
open Types
open Utils
open Errors

(* Should the borrow type be carried by the environment or by the reference? *)

type fn_data = { pars : ide list; body : statement; ret : expr option }
[@@deriving show]

type ty = T_String | T_Borrow of bool * ty | T_I32 | T_Bool | T_Unit
[@@deriving show]

type prim = PRINTLN | PUSH_STR [@@deriving show]

type memval =
  | I32 of int
  | Bool of bool
  | Borrow of loc owned
  | Unit
  | Str of string
  | String of string owned
  | Moved of ide
[@@deriving show]

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

module Mem = struct
  module M = Map.Make (Int)
  include M
  type t = memval M.t

  let find m loc : memval trace_result =
    let open Result in
    match M.find_opt loc m with
    | None -> error (SegFault loc)
    | Some (Moved x) -> error (MovedValue x)
    | Some v -> ok v

  let move m loc from =
    update loc
      (fun v ->
        match v with
        | Some (String _) -> Some (Moved from)
        | v -> v)
      m
end

type env = Env.t
type mem = Mem.t

type state = {
  mutable memory     : mem;
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
  let counter = ref 0 in
  let locs () =
    let c = !counter in
    incr counter;
    c
  in
  let _f = Seq.(ints 0 |> to_dispenser) in
  Stack.push prelude envstack;
  { memory; envstack; toplevel = prelude; loop_level = 0; output = ""; locs }

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

let borrow st ?(recv = "anon") src : memval trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find src env in
  match v with
  | Loc ({ borrows = `imm xs } as data) ->
      let data = { data with borrows = `imm (recv :: xs) } in
      let* _ = update_topenv st (fun env -> ok (Env.add src (Loc data) env)) in
      ok (Borrow { value = data; owner = src })
  | Loc { borrows = `mut _ } ->
      error (DataRace { borrowed = src; is = `imm; want = `mut })
  | _ -> error (TypeError "Non-value cannot be referenced")

(** [borrow_mut st src recv] lets [recv] borrow the value of [src] mutably. *)
let borrow_mut st ?(recv = "anon") src : memval trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find src env in
  match v with
  | Loc ({ borrows = `imm [] } as data) ->
      let data = { data with borrows = `mut recv; mut = true } in
      let* _ = update_topenv st (fun env -> ok (Env.add src (Loc data) env)) in
      ok (Borrow { value = data; owner = src })
  | Loc _ -> error (DataRace { borrowed = src; is = `mut; want = `mut })
  | _ -> error (TypeError "Non-value cannot be referenced")

let unborrow st borrow : unit trace_result =
  let open Result in
  match borrow with
  | Borrow data ->
      update_topenv st (fun env ->
          let* owner = Env.find data.owner env in
          match owner with
          | Loc owner_data -> ok env
          | _ -> error (TypeError "Can't unborrow a non-reference"))
  | _ -> error (TypeError "Can't unborrow a non-reference")

let deref st (borrow : memval) : memval trace_result =
  let open Result in
  match borrow with
  | Borrow data -> Mem.find st.memory data.value.loc
  | _ -> error (TypeError "Can't deref a non-reference")

let get_var st x : memval trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find x env in
  match v with
  | Loc { loc } -> (
      let* value = Mem.find st.memory loc in
      match value with
      | Moved _ -> error (MovedValue x)
      | _ -> ok value)
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
  let fresh_loc = { mut; loc = st.locs (); borrows = `imm [] } in
  update_topenv st (fun env ->
      let* mem =
        match init with
        | String data -> (
            let new_init = String { data with owner = x } in
            if data.owner = "" then
              ok (Mem.add fresh_loc.loc new_init st.memory)
            else
              (* find the previous owner *)
              let* v = Env.find data.owner env in
              match v with
              | Loc loc ->
                  let mem = Mem.move st.memory loc.loc data.owner in
                  let mem = Mem.add fresh_loc.loc new_init mem in
                  ok mem
              | _ -> ok st.memory)
        | _ -> ok (Mem.add fresh_loc.loc init st.memory)
      in
      st.memory <- mem;
      ok (Env.add x (Loc fresh_loc) env))

(*
   let mut x = String::from("hello")
   let y = x;  // move
   x = y       // move back
*)
let set_var st x (v : memval) : unit trace_result =
  let open Result in
  let env = topenv st in
  let* res = Env.find x env in
  match res with
  | Loc { mut = false } -> error (CannotMutate x)
  | Loc { borrows = `imm (_ :: _) | `mut _ } -> error (CannotAssignBorrowed x)
  | Loc { loc; borrows = `imm [] } ->
      let mem =
        match v with
        | String data -> Mem.move st.memory loc x
        | v -> Mem.add loc v st.memory
      in
      st.memory <- mem;
      ok ()
  | _ -> error (TypeError (spr "cannot assign to the function %s" x))

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
