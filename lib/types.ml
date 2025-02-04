open Ast
open Utils

module Result = struct
  include Result

  let ( <$> ) = map

  let ( <|> ) : ('a, 'e) result -> ('a, 'e) result -> ('a, 'e) result =
   fun a b ->
    match a with
    | Error _ -> b
    | Ok a -> Ok a

  let ( <*> ) : ('a -> 'b, 'e) result -> ('a, 'e) result -> ('b, 'e) result =
   fun a b ->
    match a with
    | Error s -> Error s
    | Ok f -> f <$> b

  let ( >>= ) = bind
  let ( let* ) = bind

  let return = ok
end

module WithState = struct
  type ('s, 'a) t = 's -> 's * 'a

  let return (e : 'a) : ('s, 'a) t = fun st -> (st, e)

  let set (st : 's) : ('s, unit) t = fun _ -> (st, ())

  let get : ('s, 's) t = fun st -> (st, st)

  let bind (e : ('s, 'a) t) (next : 'a -> ('s, 'b) t) : ('s, 'b) t =
   fun st ->
    let st', a = e st in
    next a st'

  let ( let$ ) = bind

  let seq (f : ('s, 'a -> 'b) t) (e : ('s, 'a) t) : ('s, 'b) t =
    let$ f = f in
    let$ e = e in
    return (f e)

  let map (f : 'a -> 'b) (e : ('s, 'a) t) : ('s, 'b) t =
    let$ e = e in
    return (f e)

  let ( <*> ) = seq
  let ( <$> ) = map
end

(* There is no way to push the same value twice *)
module Locs : sig
  type secret
  val pop : unit -> secret
  val push : secret -> unit
  val to_int : secret -> int
end = struct
  let s = ref (Seq.ints 0)
  type secret = int

  let pop () =
    let x, xs = Seq.uncons !s |> Option.get in
    s := xs;
    x

  let push n = s := Seq.cons n !s

  let to_int n = n
end
