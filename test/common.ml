let ( % ) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) = fun g f x -> g (f x)

let read_file filename =
  let ch = open_in filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  str

let examples_dir = "/home/dalpi/tinyrust/test/examples/"

let abs_path name = examples_dir ^ name

let examples =
  let dirs = Sys.readdir examples_dir in
  Array.sort String.compare dirs;
  dirs

let programs =
  Array.map (read_file % abs_path) examples

let examples_dict =
  Array.map (
    fun e -> (e, read_file (abs_path e))
  ) examples

let pr = Printf.printf
