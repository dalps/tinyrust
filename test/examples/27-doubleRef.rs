fn main() {
  let x = &mut String::from("ciao");
  let y = &x; // reference of reference

  println!("{x}, {y}"); // ok
}
