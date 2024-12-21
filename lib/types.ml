open Ast

type ide = string [@@deriving show]

type parameter = ide [@@deriving show]

type loc = int [@@deriving show]

type trace_error =
  | TypeError of string
  | MutabilityError of ide * string
  | UnboundLoc of loc
  | UnboundVar of ide
[@@deriving variants]

exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc
let type_fail s = raise (TypeError s)

type prim = PRINTLN [@@deriving show]

type envval =
  | ConstVar of loc
  | MutVar of loc
  | Fun of parameter * expr
  | Prim of prim
[@@deriving show]

type memval = V_String of string | V_I32 of int | V_Unit
[@@deriving show, variants]

let get_value = function
  | CONST n -> v_i32 n
  | STRING s -> v_string s
  | UNIT -> v_unit
  | _ -> type_fail "Not a value"

type env = ide -> envval
type mem = loc -> memval

type state = {
  envstack : env Stack.t;
  mutable memory : mem;
  mutable firstloc : loc;
}
[@@deriving fields]

let consume_loc st =
  let l = st.firstloc in
  st.firstloc <- st.firstloc + 1;
  l

let bottom_env : env = fun x -> raise (UnboundVar x)
let bottom_mem : mem = fun x -> raise (UnboundLoc x)

let bindf f x v y = if x = y then v else f y

let state0 =
  let s = Stack.create () in
  let env0 = bindf bottom_env "println" (Prim PRINTLN) in
  Stack.push env0 s;
  { envstack = s; memory = bottom_mem; firstloc = 0 }

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
  set_topenv st env'

let bind_var st x v =
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
    let_var st x v;
    st

let println (st : state) s =
  let open Re in
  let re =
    compile
    @@ seq [ char '{'; group (rep1 @@ compl [ char '{'; char '}' ]); char '}' ]
  in
  let matches = all re s in
  List.iter
    (fun groups ->
      let var = Group.get groups 1 in
      let v = get_var st var in
      Printf.printf "%s: %s\n" var (show_memval v))
    matches

module WithState = struct
  type 'a t = state -> state * 'a

  let return (e : 'a) : 'a t = fun st -> (st, e)

  let set (st : state) : unit t = fun _ -> (st, ())

  let get : state t = fun st -> (st, st)

  let bind (e : 'a t) (next : 'a -> 'b t) : 'b t =
   fun st ->
    let st', a = e st in
    next a st'

  let ( let$ ) = bind

  let seq (f : ('a -> 'b) t) (e : 'a t) : 'b t =
    let$ f = f in
    let$ e = e in
    return (f e)

  let map (f : 'a -> 'b) (e : 'a t) : 'b t =
    let$ e = e in
    return (f e)

  let ( <*> ) = seq
  let ( <$> ) = map
end

let ( % ) = Fun.compose

module R = struct
  include Result

  let ( <$> ) = map

  let ( <|> ) : ('a, 'e) result -> ('a, 'e) result -> ('a, 'e) result =
   fun a b -> match a with Error _ -> b | Ok a -> Ok a

  let ( <*> ) : ('a -> 'b, 'e) result -> ('a, 'e) result -> ('b, 'e) result =
   fun a b -> match a with Error s -> Error s | Ok f -> f <$> b

  let ( >>= ) = bind
  let ( let* ) = bind
end

module WithStateResult = struct
  type ('a, 'e) t = ('a, 'e) Result.t WithState.t

  let ok e = WithState.return (Result.ok e)
  let fail s = Result.error s
end
