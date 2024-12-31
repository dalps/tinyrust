let pr = Printf.printf
let spr = Printf.sprintf

let ( % ) : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c) = fun g f x -> g (f x)
