let square x = x * x
let a = square 3
let () = print_int a
let rec factorial x = if x <= 1 then 1 else x * factorial (x - 1)
let b = factorial 5
let () = print_int b
let c = (1, (2, 3))
let d = match c with (9, (a, _)) -> a | (_, (_, a)) -> a
let () = print_int d
