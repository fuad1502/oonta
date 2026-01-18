type int_list =
  | Empty
  | Cat of (int * int_list)

let print_bool b =
  if b then
    print_int 1
  else
    print_int 0

let a = (1, (2, 4))
let b = (1, (1, 4))
let () = print_bool (a > b)
let () = print_bool (a >= b)
let () = print_bool (a < b)
let () = print_bool (a <= b)
let () = print_bool (a > a)
let () = print_bool (a >= a)
let () = print_bool (a < a)
let () = print_bool (a <= a)
let () = print_bool (a = a)
let a = Cat (1, Cat (3, Empty))
let b = Cat (1, Cat (2, Cat (3, Empty)))
let c = Cat (1, Cat (2, Empty))
let () = print_bool (a > b)
let () = print_bool (a >= b)
let () = print_bool (a < b)
let () = print_bool (a <= b)
let () = print_bool (b > c)
let () = print_bool (b >= c)
let () = print_bool (b < c)
let () = print_bool (b <= c)
let () = print_bool (a > a)
let () = print_bool (a >= a)
let () = print_bool (a < a)
let () = print_bool (a <= a)
let () = print_bool (a = a)
