type 'a list =
  | Empty
  | Cat of ('a * 'a list)

let rec create_lst_aux n acc =
  match n with
  | 0 -> acc
  | _ -> create_lst_aux (n - 1) (Cat (read_int (), acc))

let compare lst_a lst_b =
  if lst_a > lst_b then
    print_int 1
  else if lst_a = lst_b then
    print_int 0
  else
    print_int 2

let create_lst n = create_lst_aux n Empty
let n = read_int ()
let lst = create_lst n
let () = compare lst lst
