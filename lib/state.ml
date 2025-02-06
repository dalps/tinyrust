open Ast
open Types
open Utils
open Errors

type fn_data = {
  id : ide;
  pars : ide list;
  body : statement;
  ret : expr option;
}
[@@deriving show]

type ty = T_String | T_Borrow of bool * ty | T_I32 | T_Bool | T_Unit
[@@deriving show]

type prim = PRINTLN | PUSH_STR [@@deriving show]

type memval =
  | I32 of int
  | Bool of bool
  | Borrow of borrow
  | Unit
  | Str of string
  | String of string owned
[@@deriving show]

let expr_of_memval : memval -> expr trace_result =
  let open Result in
  function
  | I32 i -> CONST i |> ok
  | Bool true -> TRUE |> ok
  | Bool false -> FALSE |> ok
  | Unit -> UNIT |> ok
  | Borrow data -> REF { mut = data.mut; e = CONST data.owner.loc } |> ok
  | Str s -> STR s |> ok
  | String data -> STRING data |> ok

let memval_of_expr st : expr -> memval trace_result =
  let open Types.Result in
  function
  | CONST n -> ok (I32 n)
  | STRING data -> ok (String data)
  | UNIT -> ok Unit
  | BORROW loc -> ok (Borrow loc)
  | STR value -> error (TypeError "String slice is not valid memval")
  | _ -> error (TypeError "Not a value")

type envval = Loc of variable | Fn of fn_data | Prim of prim [@@deriving show]

module Variable = struct
  type t = variable
  let compare (t1 : variable) (t2 : variable) = compare t1.loc t2.loc
end

module Env = struct
  module M = Map.Make (String)
  include M
  type t = envval M.t

  let find ide env : envval trace_result =
    let open Result in
    match M.find_opt ide env with
    | Some v -> ok v
    | None -> error (UnboundVar ide)
end

module Mem = struct
  module M = Map.Make (Variable)
  include M
  type t = memval M.t

  let find m var : memval trace_result =
    let open Result in
    match M.find_opt var m with
    | Some v -> ok v
    | None -> error (MovedValue var.id)

  let move m (prev_owner : variable) (new_owner : variable) : t trace_result =
    let open Result in
    let* v = find m prev_owner in
    let m1 =
      M.update prev_owner
        (fun v ->
          match v with
          | Some (String data) -> None
          | v -> v) (* copy *)
        m
    in
    ok
      (M.add new_owner
         (match v with
         | String data -> String { data with owner = new_owner }
         | v -> v)
         m1)
end

type env = Env.t
type mem = Mem.t

type state = {
  envstack           : env Stack.t;
  toplevel           : env;
  mutable memory     : mem;
  mutable loop_level : int;
  mutable output     : string;
  loc_generator      : unit -> int;
  id_generator       : unit -> int;
}
[@@deriving fields]
[@@ocamlformat "disable"]

let init () =
  let envstack = Stack.create () in
  let prelude =
    Env.of_list [ ("println", Prim PRINTLN); ("push_str", Prim PUSH_STR) ]
  in
  let memory = Mem.empty in
  let loc_counter = ref 0 in
  let id_counter = ref 0 in
  let generator r () =
    let c = !r in
    incr r;
    c
  in
  Stack.push prelude envstack;
  {
    memory;
    envstack;
    toplevel = prelude;
    loop_level = 0;
    output = "";
    loc_generator = generator loc_counter;
    id_generator = generator id_counter;
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

let borrow st ?(by = "anon") src : borrow trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find src env in
  match v with
  | Loc ({ borrows = `imm xs } as owner) ->
      let owner = { owner with borrows = `imm (BorrowSet.add by xs) } in
      let* _ = update_topenv st (fun env -> ok (Env.add src (Loc owner) env)) in
      ok { by; owner; mut = false }
  | Loc { borrows = `mut _ } ->
      error (DataRace { borrowed = src; is = `mut; want = `imm })
  | _ -> error (TypeError "Non-value cannot be referenced")

(** [borrow_mut st src recv] lets [recv] borrow the value of [src] mutably. *)
let borrow_mut st ?(by = "anon") src : borrow trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find src env in
  match v with
  | Loc data when not data.mut -> error (MutBorrowOfNonMut src)
  | Loc ({ borrows = `imm xs } as owner) when BorrowSet.is_empty xs ->
      let owner = { owner with borrows = `mut by; mut = true } in
      let* _ = update_topenv st (fun env -> ok (Env.add src (Loc owner) env)) in
      ok { by; owner; mut = true }
  | Loc { borrows = `imm _ } ->
      error (DataRace { borrowed = src; is = `imm; want = `mut })
  | Loc _ -> error (DataRace { borrowed = src; is = `mut; want = `mut })
  | _ -> error (TypeError "Non-value cannot be referenced")

let unborrow st (borrow : borrow) : unit trace_result =
  let open Result in
  update_topenv st (fun env ->
      Env.update borrow.owner.id
        (function
          | Some (Loc owner) ->
              let owner =
                if borrow.mut then { owner with borrows = `imm BorrowSet.empty }
                else
                  {
                    owner with
                    borrows =
                      (match owner.borrows with
                      | `imm s when BorrowSet.is_empty s ->
                          failwith "no immutable borrows to remove"
                      | `mut _ ->
                          failwith
                            "cannot remove mutable borrow from immutably \
                             borrowed variable"
                      | `imm s -> `imm (BorrowSet.remove borrow.by s));
                  }
              in
              Some (Loc owner)
          | Some v -> Some v
          | None -> None)
        env
      |> ok)

let rec deref st (borrow : borrow) : memval trace_result =
  let open Result in
  let* v = Mem.find st.memory borrow.owner in
  match v with
  | Borrow b -> deref st b
  | v -> ok v

let get_var st x : memval trace_result =
  let open Result in
  let env = topenv st in
  let* v = Env.find x env in
  match v with
  | Loc var ->
      let* value = Mem.find st.memory var in
      ok value
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
  let var : variable =
    { id = x; mut; loc = st.loc_generator (); borrows = `imm BorrowSet.empty }
  in
  update_topenv st (fun env ->
      let* mem =
        match init with
        | STR s ->
            ok (Mem.add var (String { value = s; owner = var }) st.memory)
        | STRING data -> Mem.move st.memory data.owner var
        | v ->
            let* v = memval_of_expr st v in
            ok (Mem.add var v st.memory)
      in
      st.memory <- mem;
      ok (Env.add x (Loc var) env))

(*
   let mut x = String::from("hello")
   let y = x;  // move
   x = y       // move back
*)
let set_var st x (v : expr) : unit trace_result =
  let open Result in
  let env = topenv st in
  let* res = Env.find x env in
  match res with
  | Loc { mut = false } -> error (CannotMutate x)
  | Loc ({ borrows = `imm xs } as var) when BorrowSet.is_empty xs ->
      let* v =
        match v with
        | STR s -> ok (String { value = s; owner = var })
        | v -> memval_of_expr st v
      in
      let* mem =
        match v with
        | String data -> Mem.move st.memory data.owner var
        | v -> ok (Mem.add var v st.memory)
      in
      st.memory <- mem;
      ok ()
  | Loc { loc; borrows = _ } -> error (CannotAssignBorrowed x)
  | _ -> error (TypeError (spr "cannot assign to the function %s" x))

let set_mutborrow st (b : borrow) (v : expr) : unit trace_result =
  let open Result in
  let* mem =
    if b.owner.mut then
      let* v =
        match v with
        | STR s -> ok (String { value = s; owner = b.owner })
        | v -> memval_of_expr st v
      in
      ok (Mem.add b.owner v st.memory)
    else error (CannotMutate b.owner.id)
  in
  st.memory <- mem;
  ok ()

let new_fn st id pars body ret =
  update_topenv st (fun env ->
      Result.ok (Env.add id (Fn { id; pars; body; ret }) env))

let state_of_prog (prog : statement list) : state =
  let st = init () in
  List.iter
    (function
      | FUNDECL data ->
          new_fn st data.name data.pars data.body data.ret |> ignore
      | _ -> failwith "error initializing state: not a toplevel item")
    prog;
  { st with toplevel = topenv st }
