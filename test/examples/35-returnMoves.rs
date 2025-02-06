fn fie(s: String) -> String {
  println!("dentro fie: {s}");
  s
}

fn main() {
  let mut x = String::from("ciao");
  println!("prima di fie: {x}");
  x = fie(x);
  println!("dopo fie: {x}");
}
