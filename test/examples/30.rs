fn main () {
  let mut x = String::from("hello");
  let y = x;
  x = y;
  println!("{x}"); // ok
  
}