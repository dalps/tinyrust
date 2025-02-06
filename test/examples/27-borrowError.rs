fn main() {
  let mut x = String::from("Ciao");
  let y = &mut x; // borrow di x a y (mutabile)
  y.push_str(", mondo");
  println!("{x}"); // errore: cannot borrow `x` as immutable because it is also borrowed as mutable
  println!("{y}");
}


// Note: x is borrowed while the mutable borrow in y is still active

