fn main() {
  let x = String::from("greetings");
  {
    let z = x;
    println!("inner block: {z}");
  }
  println!("outer block: {x}"); // error: borrow of moved value
}
