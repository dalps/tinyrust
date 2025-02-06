fn main() {
  let mut x = String::from("ciao");
  let y = &mut x; // reference of reference

  println!("{x}, {y}"); // cannot borrow x as immutable because it is also borrowed as mutable
}
