fn main() {
  let mut x = String::from("ciao");
  let y = &mut x; // reference of reference

  x.push_str("hola"); // cannot borrow `x` as mutable more than once at a time
  println!("{x}, {y}");
}
