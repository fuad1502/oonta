type list =
  | Empty
  | Cat of (int * list)

let rec map f lst =
  match lst with
  | Empty -> ()
  | Cat (x, l) ->
      let () = f x in
      map f l

let rec create_lst_aux n acc =
  match n with
  | 0 -> acc
  | _ -> create_lst_aux (n - 1) (Cat (read_int (), acc))

let rec insert elem lst =
  match lst with
  | Empty -> Cat (elem, Empty)
  | Cat (head, tail) ->
      if elem <= head then
        Cat (elem, lst)
      else
        Cat (head, insert elem tail)

let rec insertion_sort lst =
  match lst with
  | Empty -> Empty
  | Cat (head, tail) -> insert head (insertion_sort tail)

let create_lst n = create_lst_aux n Empty
let n = read_int ()
let lst = create_lst n
let sorted_lst = insertion_sort lst
let () = map print_int sorted_lst
