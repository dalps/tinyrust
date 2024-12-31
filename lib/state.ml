open Ast
open Types

type ide = string [@@deriving show]

type ty = T_String | T_Ref of bool * ty | T_I32 | T_Bool | T_Unit
[@@deriving show]

type parameter = { name : ide; ty : ty } [@@deriving show]

type variable = { name : ide; mut : bool; ty : ty } [@@deriving show]

type loc = int [@@deriving show]

type trace_error =
  | TypeError of string
  | MutabilityError of ide * string
  | UnboundLoc of loc
  | UnboundVar of ide
[@@deriving variants]

type 'a trace_result = ('a, trace_error) result

type prim = PRINTLN [@@deriving show]

type envval =
  | ConstVar of loc
  | MutVar of loc
  | Fun of parameter list * expr
  | Prim of prim
  | Ref of bool * ide * loc
[@@deriving show]

type memval = V_String of string | V_I32 of int | V_Unit
[@@deriving show, variants]

let string_of_memval (m : memval) =
  match m with
  | V_String s -> s
  | V_I32 i -> string_of_int i
  | V_Unit -> "()"

let expr_of_memval (m : memval) : expr =
  match m with
  | V_String s -> STRING s
  | V_I32 i -> CONST i
  | V_Unit -> UNIT

let get_value : expr -> memval trace_result =
  let open R in
  function
  | CONST n -> ok @@ v_i32 n
  | STRING s -> ok @@ v_string s
  | UNIT -> ok @@ v_unit
  | _ -> error @@ typeerror "Not a value"

module Env = struct
  type t = (ide, envval) Hashtbl.t

  let init () = Hashtbl.create 99

  let get self ide : envval trace_result =
    let open R in
    match Hashtbl.find_opt self ide with
    | None -> error @@ unboundvar ide
    | Some v -> ok v
end
type mem = (loc, memval) Hashtbl.t

module St = struct
  type t = {
    envstack : env Stack.t;
    mutable memory : mem;
    mutable firstloc : loc;
    module_env : env;
  }
  [@@deriving fields]

  let consume_loc st =
    let l = st.firstloc in
    st.firstloc <- st.firstloc + 1;
    l

  let init () =
    let envstack = Stack.create () in
    let env0 = bindf bottom_env "println" (Prim PRINTLN) in
    Stack.push env0 envstack;
    { envstack; memory = bottom_mem; firstloc = 0; module_env = bottom_env }

  let topenv st = Stack.top st.envstack
  let popenv st = Stack.pop st.envstack |> ignore
  let pushenv st env = Stack.push env st.envstack
  let newenv st = Stack.push (topenv st) st.envstack
  let set_topenv st env =
    ignore (Stack.pop_opt st.envstack);
    Stack.push env st.envstack

  let get_var st x : memval =
    let env = topenv st in
    match env x with
    | ConstVar l | MutVar l -> memory st l
    | Fun _ | Prim _ -> type_fail "Cannot use a function here"

  let let_var ~(mut : bool) st x v =
    let env = topenv st in
    let mem = memory st in
    let loc = consume_loc st in
    let env' = bindf env x (if mut then MutVar loc else ConstVar loc) in
    let mem' = bindf mem loc v in
    set_memory st mem';
    set_topenv st env';
    st

  let bind_var st x v =
    pr "Binding %s to %s\n" x (show_memval v);
    let env = topenv st in
    let mem = memory st in
    try
      match env x with
      | ConstVar _ -> failwith @@ "Cannot assign to immutable variable " ^ x
      | MutVar l ->
          set_memory st (bindf mem l v);
          st
      | Fun _ | Prim _ -> failwith "Cannot assign to a function"
    with UnboundVar x ->
      pr "Allocating %s\n" x;
      let st = let_var ~mut:false st x v in
      st
end

type env = Env.t
type mem = Mem.t
type state = St.t

module Prim = struct
  let println (st : state) s =
    let open Re in
    let re =
      compile
      @@ seq
           [ char '{'; group (rep1 @@ compl [ char '{'; char '}' ]); char '}' ]
    in
    replace ~all:true
      ~f:(fun groups ->
        let var = Group.get groups 1 in
        let v = St.get_var st var in
        string_of_memval v)
      re s
    |> print_endline
end
