type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec layout_binary_tree_1 t=
  let rec aux depth xleft t = match t with
    | Empty -> (Empty, xleft)
    | Node(v, l ,r ) ->
      let l, l_x = aux (depth+1) xleft l
      in
      let r, r_x = aux (depth+1) (l_x+1) r
      in
      (Node((v,l_x, depth), l, r),r_x)
  in
  fst(aux 1 1 t)
