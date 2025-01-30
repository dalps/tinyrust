let ( % ) g f x = g (f x)

let pr = Printf.printf
let spr = Printf.sprintf

let read_file filename =
  let ch = open_in filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  str
