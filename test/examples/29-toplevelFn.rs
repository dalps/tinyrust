// Bonus program to test out-of-order function declarations

fn quadruple(x: i32) -> i32 {
  let z = double(x);
  z + z
}
fn main() {
  let mut x = 3;
  let y = quadruple(double(x));
  println!("{x} x 8 = {y}")
}
fn double(x: i32) -> i32 {
  x + x
}