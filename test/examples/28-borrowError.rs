fn main() {
  let x = &String::from("ciao");
  let y = &mut x; // cannot borrow as mutable because x is not declared as mutable

  println!("{x}, {y}");
}
