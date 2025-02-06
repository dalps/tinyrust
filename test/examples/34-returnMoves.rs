fn fie(s: String) -> String {
  println!("dentro fie: {s}");
  s
}

fn main() {
  let x = String::from("ciao");
  println!("prima di fie: {x}");
  let x = fie(x);
  println!("dopo fie: {x}");
}
