open Ast
open Types
open Utils

type ide = string [@@deriving show]

type ty = T_String | T_Ref of bool * ty | T_I32 | T_Bool | T_Unit
[@@deriving show]

type parameter = ide [@@deriving show]

type loc = Stack of int | Heap of int [@@deriving show]

type trace_error =
  | TypeError of string
  | CannotMutate of ide
  | MutBorrowOfNonMut of ide
  | DataRace of ide * bool * bool
  | UnboundVar of ide
  | OutOfGas of int
  | NotInLoop
  | MovedValue of ide
  | TODO
[@@deriving show]

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

type 'a trace_result = ('a, trace_error) result

type prim = PRINTLN [@@deriving show]

(** The type of values that the environment keeps track of *)
type stackval =
  | I32 of int
  | Bool of bool
  | Ref of bool * int
  | Unit
  | String of string
[@@deriving show]

type variable = { mut : bool; mutable value : stackval } [@@deriving show]
(** The information associated to a variable in the environment *)

(** The type of items that can be associated to a name in the environment *)
type envval = Var of variable | Fun of parameter list * expr | Prim of prim
[@@deriving show]

let string_of_stackval = function
  | I32 i -> string_of_int i
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | String s -> s
  | Ref (_, i) -> spr "ref: %d" i

module Env = struct
  module StringMap = Map.Make (String)

  include StringMap

  type t = envval StringMap.t

  let bottom = StringMap.empty

  let get (env : t) (ide : ide) : envval trace_result =
    let open R in
    match StringMap.find_opt ide env with
    | Some v -> ok v
    | None -> error @@ UnboundVar ide

  let bind (env : t) (ide : ide) (v : envval) = StringMap.add ide v env

  let borrow _ = R.error TODO

  let move _ = R.error TODO
end

module Heap = struct
  include Map.Make (Int)

  let drop _ = R.error TODO
end

type env = Env.t
type mem = string Heap.t

module St = struct
  type t = {
    envstack : env Stack.t;
    mutable loop_level : int;
    toplevel : env;
    mutable output : string;
  }
  [@@deriving fields]

  let prelude = Env.bind Env.bottom "println" (Prim PRINTLN)

  let init () =
    let envstack = Stack.create () in
    Stack.push prelude envstack;
    { envstack; loop_level = 0; toplevel = prelude; output = "" }

  let topenv st = Stack.top st.envstack
  let popenv st = Stack.pop st.envstack |> ignore
  let pushenv st env = Stack.push env st.envstack
  let newenv st = Stack.push (topenv st) st.envstack
  let set_topenv st env =
    ignore (Stack.pop_opt st.envstack);
    Stack.push env st.envstack

  let append_output st s = st.output <- st.output ^ s

  let enter_loop st = st.loop_level <- st.loop_level + 1
  let exit_loop st =
    let open R in
    if st.loop_level > 0 then (
      st.loop_level <- st.loop_level - 1;
      ok ())
    else error NotInLoop

  let get_var st x : stackval trace_result =
    let open R in
    let env = topenv st in
    let* res = Env.get env x in
    match res with
    | Var { mut; value } -> ok value
    | _ -> error (TypeError "cannot use a function here")

  let let_var ~(mut : bool) st x v : unit trace_result =
    let open R in
    let env = topenv st in
    let env' = Env.bind env x (Var { mut; value = v }) in
    ok (set_topenv st env')

  let bind_var st x v : unit trace_result =
    let open R in
    let env = topenv st in
    let* res = Env.get env x in
    match res with
    | Var { mut = false; _ } -> error (CannotMutate x)
    | Var ({ mut; value = old_value } as data) ->
        let env' = Env.bind env x (Var { data with value = v }) in
        ok (set_topenv st env')
    | _ -> error (TypeError (spr "cannot assign to the function %s" x))
end

type state = St.t

module Prim = struct
  let println (st : state) s : unit trace_result =
    let open R in
    let open Re in
    let re =
      compile
      @@ seq
           [ char '{'; group (rep1 @@ compl [ char '{'; char '}' ]); char '}' ]
    in
    let* line =
      split_full re s
      |> List.fold_left
           (fun acc tok ->
             let* acc = acc in
             let* next =
               match tok with
               | `Text s -> ok s
               | `Delim groups ->
                   let var = Group.get groups 1 in
                   let* v = St.get_var st var in
                   ok (string_of_stackval v)
             in
             ok (acc ^ next))
           (ok "")
    in
    St.append_output st (line ^ "\n");
    ok ()
end
