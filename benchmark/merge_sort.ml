type list =
  | Empty
  | Cat of (int * list)

let rec map f lst =
  match lst with
  | Empty -> ()
  | Cat (x, l) ->
      let () = f x in
      map f l

let rec split_aux lst acc_left acc_right =
  match lst with
  | Empty -> (acc_left, acc_right)
  | Cat (head, rest) -> split_aux rest acc_right (Cat (head, acc_left))

let split lst = split_aux lst Empty Empty

let rec merge left right =
  match (left, right) with
  | Empty, right -> right
  | lest, Empty -> lest
  | Cat (left_head, left_rest), Cat (right_head, right_rest) ->
      if left_head <= right_head then
        Cat (left_head, merge left_rest right)
      else
        Cat (right_head, merge left right_rest)

let rec merge_sort lst =
  match lst with
  | Empty -> lst
  | Cat (_, Empty) -> lst
  | _ ->
      let splitted_lst = split lst in
      let left =
        match splitted_lst with
        | lst, _ -> lst
      in
      let right =
        match splitted_lst with
        | _, lst -> lst
      in
      merge (merge_sort left) (merge_sort right)

let rec create_lst_aux n acc =
  match n with
  | 0 -> acc
  | _ -> create_lst_aux (n - 1) (Cat (read_int (), acc))

let create_lst n = create_lst_aux n Empty
let n = read_int ()
let lst = create_lst n
let sorted_lst = merge_sort lst
let () = map print_int sorted_lst
