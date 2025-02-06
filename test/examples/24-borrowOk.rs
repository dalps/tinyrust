fn pushCiao(s: &mut String) {
  s.push_str("ciao");
}

fn main() {
  let mut x = String::from("hello");
  pushCiao(&mut x);

  println!("{x}"); // ok
}
