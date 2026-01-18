type 'a list =
  | Empty
  | Cat of ('a * 'a list)

let rec len lst =
  match lst with
  | Cat (_, lst) -> 1 + len lst
  | Empty -> 0

let a = Cat (4, Cat (2, Empty))
let b = Cat (1 > 2, Empty)
let c = Empty
let d = a > c

let () =
  if d then
    print_int 1
  else
    print_int 0

let () = print_int (len a)
let () = print_int (len b)
let () = print_int (len c)
