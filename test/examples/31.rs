fn main () {
  let mut x = String::from("hello");
  let y = x;
  x = String::from("ciao");
  println!("{x} {y}"); // ok
}