fn main() {
  let mut x = String::from("ciao");
  let z = x;
  let y = &z;
  let x = z; // cannot move out of z because it is borrowed

  println!("{x}, {y}");
}
