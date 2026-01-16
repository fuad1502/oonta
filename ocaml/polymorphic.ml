let g x y f = if x > y then f x else f y
let h = g 1 2
let i = h print_int
