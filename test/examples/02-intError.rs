fn main() {
    let x = 42; // variabile immutabile di tipo intero
    let y = x + 1;
    x = x + y; // errore: x immutabile
    println!("{x}");
}
