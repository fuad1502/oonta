let g x y f = if x > y then f x else f y
let h f = g 1 2 f
let i = h print_int
