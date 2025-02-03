open TinyrustLib
open Utils
open Trace
open Prettyprint
open Minttea

type model = {
  _example_name : string;
  program : string;
  trace_outcome : trace_outcome;
  current_step : int;
  max_steps : int;
}

(* type event = Next | Prev | Quit | Menu *)

let init _model = Command.Noop

let update event (model : model) =
  match event with
  | Event.KeyDown (Key "q" | Escape) -> (model, Command.Quit)
  | Event.KeyDown (Key "g") ->
      ({ model with current_step = 0 }, Command.Noop)
  | Event.KeyDown (Key "h") ->
      ({ model with current_step = model.max_steps }, Command.Noop)
  | Event.KeyDown (Key "j" | Down) ->
      let current_step =
        if model.current_step < model.max_steps then model.current_step + 1
        else model.current_step
      in
      ({ model with current_step }, Command.Noop)
  | Event.KeyDown (Key "k" | Up) ->
      let current_step =
        if model.current_step = 0 then model.current_step
        else model.current_step - 1
      in
      ({ model with current_step }, Command.Noop)
  | _ -> (model, Command.Noop)

let view model =
  let usage =
    ANSITerminal.(
      sprintf [ black; Bold ] "q: quit%s%s, g/h: jump to start/end"
        (if model.current_step < model.max_steps then ", j/↓: next step" else "")
        (if model.current_step > 0 then ", k/↑: previous step" else ""))
  in
  if model.current_step = 0 then
    spr "%s%s\n%s"
      ANSITerminal.(sprintf [ yellow ] "\n--- Source code ---\n\n")
      model.program usage
  else
    let snapshot =
      List.nth model.trace_outcome.trace (model.current_step - 1)
    in
    spr {|
%s

%s

%s

%s%s%s

%s
|}
      ANSITerminal.(sprintf [ red ] "--- Step %-2d ---" model.current_step)
      (string_of_state snapshot.state)
      ANSITerminal.(sprintf [ blue; Bold ] "Program:")
      (string_of_expr 0 snapshot.expr)
      (if model.current_step = model.max_steps then
         spr "\n\n%s" (string_of_trace_result model.trace_outcome.result)
       else "")
      (if snapshot.state.output <> "" then
         spr "\n\n%s\n%s"
           ANSITerminal.(sprintf [ yellow; Bold ] "Output:")
           snapshot.state.output
       else "")
      usage

let () =
  let max_steps = Sys.argv.(1) |> int_of_string in
  let _example_name = Sys.argv.(2) in
  let program = read_file _example_name in
  let trace_outcome =
    program |> Parser.parse_string |> Trace.trace_prog max_steps
  in
  let initial_model : model =
    {
      _example_name;
      program;
      trace_outcome;
      current_step = 0;
      max_steps = List.length trace_outcome.trace;
    }
  in
  let app = Minttea.app ~init ~update ~view () in
  Minttea.start app ~initial_model
