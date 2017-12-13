type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;


let layout_binary_tree_2 (t:char binary_tree) = 
  let rec height_f t = match t with
    | Empty -> 0
    | Node(_,l,r) -> 1+ (max (height_f l) (height_f r))
  in
  let height = height_f t
  in
  let max_d = 2.0**(float_of_int height)
  in
  let x_diff depth = int_of_float (max_d/. (2.0**(float_of_int (depth+1))))
  in
  let rec min_x t depth xpos= match t with
    | Empty -> xpos
    | Node(_,l,_ ) -> min_x l (depth +1) (xpos + (x_diff depth))
  in
  let offset = min_x t 1 0
  in
  let rec aux (t:char binary_tree) depth xpos = match t with 
    | Empty -> Empty 
    | Node(x, l, r) -> 
      let l = aux l (depth+1) (xpos- (x_diff depth))
      in
      let r = aux r (depth+1) (xpos+ (x_diff depth))
      in
      Node((x,xpos+offset, depth), l,r)
  in
  aux t 1 0
