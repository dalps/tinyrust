open State
open Utils
open Errors

let string_of_memval = function
  | I32 i -> string_of_int i
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Str s -> s
  | String data -> data.value
  | Borrow data ->
      spr "&%s(%d)" (if data.mut then "mut " else "") data.owner.loc

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
                 let* r = borrow ~by:"println" st var in
                 let* v = deref st r in
                 let* _ = unborrow st r in
                 ok (string_of_memval v)
           in
           ok (acc ^ next))
         (ok "")
  in
  State.append_output st (line ^ "\n");
  ok ()

let push_str st x str : unit trace_result =
  let open Types.Result in
  let* ref = borrow_mut st x ~by:"push_str" in
  let* v = deref st ref in
  match v with
  | String s1 -> State.set_var st x (String { s1 with value = s1.value ^ str })
  | _ -> error (TypeError "push_str")
