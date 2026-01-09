type list = Empty | Cat of (int * list)

let rec map f l =
  match l with
  | Empty -> ()
  | Cat (x, l) ->
      let () = f x in
      map f l

let rec reverse_aux acc l =
  match l with Empty -> acc | Cat (x, xs) -> reverse_aux (Cat (x, acc)) xs

let reverse l = reverse_aux Empty l
let l = Cat (5, Cat (4, Cat (3, Cat (2, Empty))))
let rev_l = reverse l
let () = map print_int rev_l

(* let rec merge cmp left right = *)
(*   match left, right with *)
(*   | [], ys -> ys *)
(*   | xs, [] -> xs *)
(*   | x :: xs', y :: ys' -> *)
(*       if cmp x y <= 0 then *)
(*         x :: merge cmp xs' right *)
(*       else *)
(*         y :: merge cmp left ys' *)
(**)
(* let split lst = *)
(*   let rec aux slow fast acc = *)
(*     match fast with *)
(*     | [] -> (List.rev acc, slow) *)
(*     | [_] -> (List.rev acc, slow) *)
(*     | _ :: _ :: fast' -> *)
(*         match slow with *)
(*         | [] -> (List.rev acc, []) *)
(*         | s :: slow' -> aux slow' fast' (s :: acc) *)
(*   in *)
(*   aux lst lst [] *)
(**)
(* let rec merge_sort cmp lst = *)
(*   match lst with *)
(*   | [] | [_] -> lst *)
(*   | _ -> *)
(*       let left, right = split lst in *)
(*       merge cmp (merge_sort cmp left) (merge_sort cmp right) *)
(**)
(* let () = *)
(*   let unsorted = [5; 3; 8; 1; 2; 7; 4; 6] in *)
(*   let sorted = merge_sort compare unsorted in *)
(*   Printf.printf "Original: [%s]\n" *)
(*     (String.concat "; " (List.map string_of_int unsorted)); *)
(*   Printf.printf "Sorted:   [%s]\n" *)
(*     (String.concat "; " (List.map string_of_int sorted)) *)
(**)
