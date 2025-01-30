open TinyrustLib.Utils

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
