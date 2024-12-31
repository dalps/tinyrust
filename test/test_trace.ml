open TinyrustLib
open Trace
open Parser
open Common

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

let tests =
  (* gas, outcome *)
  [
    (25, Ok "3\n4\n");
    (25, Error "cannot mutate immutable variable x");
    (25, Ok "7\n");
    (25, Error "cannot mutate immutable variable x");
    (25, Ok "Ciao, mondo\n");
    (25, Ok "6\n3\n");
    (25, Error "y not found in this scope");
    (25, Ok "7\n");
    (25, Error "borrow of x");
    (25, Error "borrow of x");
    (25, Ok "Ciao\nCiao\n");
    (25, Ok {|
      il parametro prestato: Ciao
      il parametro x: Ciao
    |});
    (25, Error "cannot mutate immutable variable x");
    (25, Ok {|
      Ciao, mondo
      Ciao, mondo
    |});
    (25, Error "cannot borrow x as immutable");
    (25, Ok "1\n2\n3\n4\n5\n...");
    (40, Ok "3\n2\n1\n0");
    (50, Ok {|
      0,0
      0,1
      1,0
      1,1
    |});
    (25, Ok "7");
    (25, Error "interna not found in this scope");
    (25, Error "y not found in this scope");
  ]

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
  |> helper 30
