open State
open Utils
open Errors

let string_of_stackval = function
  | I32 i -> string_of_int i
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | StringSlice s -> s
  | Ref (_, i) -> spr "ref: %d" i

let println (st : state) s : unit trace_result =
  let open Types.Result in
  let open Re in
  let re =
    compile
    @@ seq [ char '{'; group (rep1 @@ compl [ char '{'; char '}' ]); char '}' ]
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
                 let* v = State.get_var st var in
                 ok (string_of_stackval v)
           in
           ok (acc ^ next))
         (ok "")
  in
  State.append_output st (line ^ "\n");
  ok ()

let push_str s1 s2 : string trace_result = String.cat s1 s2 |> Result.ok
