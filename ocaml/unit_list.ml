type 'a list = Empty | Cat of ('a * 'a list)

let rec map f lst =
  match lst with Empty -> Empty | Cat (x, xs) -> Cat (f x, map f xs)

let a = Cat (3, Empty)
let b = Empty
let c = map print_int a
let d = map print_int b
let print_bool b = if b then print_int 1 else print_int 0
let () = print_bool (c > d)
let () = print_bool (c >= d)
let () = print_bool (c < d)
let () = print_bool (c <= d)
let () = print_bool (c = d)
