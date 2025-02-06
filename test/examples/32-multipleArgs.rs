// Bonus program to test support for multiple arguments

fn foo(x: i32, y: i32, bar: i32) -> i32 {
  println!("{bar}");
  bar + x * y
}

fn main() {
  let x = 2;
  {
    let y = 3;
    {
      let x = y;
      let z = x + y + foo(y, x, 42);
      println!("{z}");
    };
    println!("{y}");
  };
  println!("{x}");
}
