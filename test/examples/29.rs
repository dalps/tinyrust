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
