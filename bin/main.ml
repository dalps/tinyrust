open TinyrustLib
open Utils

let () =
  let steps = Sys.argv.(1) |> int_of_string in
  let filename = Sys.argv.(2) in
  let prog = read_file filename in
  ANSITerminal.(printf [ yellow; Bold ] "Code:\n");
  print_endline prog;
  let out = prog |> Parser.parse_string |> Trace.trace_prog steps in
  print_endline (Prettyprint.string_of_traceoutcome out)
