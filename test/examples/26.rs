fn main() {
  let mut x = &String::from("ciao");
  let y = &mut x;

  println!("{x}, {y}"); // cannot borrow x as immutable
}
