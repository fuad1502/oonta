let x = 5
let add a b = a + (b + x)
let addthree = add 3
let y = add x x

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

let rec f x = f (x - 1)
