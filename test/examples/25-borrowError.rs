fn main() {
  let x = String::from("ciao");
  let y = x;

  println!("{x}, {y}"); // borrow of moved value x
}
