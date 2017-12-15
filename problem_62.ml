type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


let internal t = 
  let rec internal_aux t acc = match t with
    | Empty -> acc
    | Node(x, Empty, Empty ) -> acc
    | Node(x, l, r) -> internal_aux l (x::(internal_aux r acc))
  in
  internal_aux t [];;


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
