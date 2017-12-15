type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;


let rec fold_left f acc t = match t with
  | Empty -> acc
  | Node(v, l, r) -> fold_left f (f (fold_left f acc l) v) r;;


let elements_in t = fold_left (fun acc x -> acc + x) 0 t;;

let at_level t level = 
  let rec internal_aux t acc counter = match t with
    | Empty -> acc
    | Node(x, l, r) -> 
      if counter=level then
        x::acc
      else
        internal_aux l (internal_aux r acc (counter+1)) (counter+1)
  in
  internal_aux t [] 1;;
