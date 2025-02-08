fn main() {
  let x = String::from("greetings");
  if x == String::from("saluti") {
    println!("ciao")
  } else {
    let z = x;
    println!("hello")
  }
  println!("outer block: {x}"); // error: borrow of moved value
}