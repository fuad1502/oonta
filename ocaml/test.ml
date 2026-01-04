let x = 5
let () = print_int x
let add a b = a + (b + x)
let addthree = add 3
let y = add x x
let () = print_int y

let z =
  let f =
   fun x ->
    let z = 1 in
    fun y -> (x * y) + z
  in
  f
    ((let x = 4 in
      fun y -> y + x)
       3)
    x

let () = print_int z
let rec f x = f (x - 1) + 1
let a = 4 >= 4

let () =
  if a then
    print_int 1
  else
    print_int 0

let rec factorial x =
  if x <= 1 then
    1
  else
    x * factorial (x - 1)

let b = factorial 5
let () = print_int b

let c x =
  if x - 1 > 0 then
    ()
  else
    ()

let () = c 2
let d = print_int
let () = d 2
