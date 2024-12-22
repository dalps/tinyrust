open TinyrustLib
open Trace
open Parser
open Common
open Ast

let helper n s = s |> parse_string |> trace_prog n |> ignore

let%expect_test "test_trace_2" =
  {|
  fn main() {
    let x = 2; // prima dichiarazione di x
    let x = x + 1; // seconda dichiarazione di x
    {
        let x = x * 2; // terza dichiarazione di x
        println!("{x}"); // output: 6
    };
    println!("{x}") // output: 3
  }
  |}
  |> helper 30

let%expect_test "test_trace_1" =
  {|
    fn main() {
        let x = 3; // variabile immutabile di tipo intero
        let y = x + 1; // idem
        println!("{x}"); // output: 3
        println!("{y}") // output: 4
    }
  |}
  |> helper 25
  [@expect {|
    3
    4
    |}]

let _a = show_binop
let _b = programs.(0)

(*
let%expect_test "" =
  {|
    fn foo(x: i32, y: i32) -> i32 {
      let z = x + y;
      z + z
    }
    fn double(x: i32) -> i32 {
      if x + 0 == 0 {
        println!("hi");
      }
      else {
        println!("hello");
      };
      x + x
    }
    fn main() {
      let mut x = 3;
      let y = foo(x,42);
      println!("{x} bla bla {y}")
    }
  |}
  |> helper 30 *)
