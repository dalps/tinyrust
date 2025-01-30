fn main() {
  let x = &mut String::from("ciao");
  let y = x;

  println!("{x}, {y}"); // borrow of moved value
}
