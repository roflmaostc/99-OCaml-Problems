type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;


(*Returns the left border of a tree as a list of x_pos value
 * Height is not needed since every element is one discrete height-step*)
let left_border t x_off = 
  let rec left_border_to_list t x_off acc = match t with
    | Empty -> acc
    | Node ((v, xpos, ypos, x_off_new), Empty, right) -> left_border_to_list right (x_off_new + x_off) ((xpos+x_off)::acc)
    | Node ((v, xpos, ypos, x_off_new), left, _) -> left_border_to_list left (x_off_new+x_off) ((xpos-x_off)::acc)
  in
  List.rev (left_border_to_list t x_off [])


(*Returns the right border of a tree as a list of x_pos value
 * Height is not needed since every element is one discrete height-step*)
let right_border t x_off =
  let rec right_border_to_list t x_off acc = match t with
    | Empty -> acc
    | Node ((v, xpos, ypos, x_off_new), left, Empty) -> right_border_to_list left (x_off_new+x_off) ((xpos-x_off)::acc)
    | Node ((v, xpos, ypos, x_off_new), _, right) -> right_border_to_list right (x_off_new+x_off) ((xpos+x_off)::acc)
  in
  List.rev (right_border_to_list t x_off [])

(*Checks wether two border lists have a collision*)
let rec check_collision left_l right_l = match (left_l,right_l) with
  | (hl::tl, hr::tr) -> if hl>=hr then true else check_collision tl tr
  | _ -> false


let update_xpos t x_dist = match t with
  | Empty -> Empty
  | Node((v, xpos, ypos, x_offset_childs), l,r) -> Node((v, xpos+x_dist, ypos, x_offset_childs), l, r)


(*Shifts subtree left and right until there is no intersections of the subtrees*)
let rec find_valid_t left_tree right_tree x_dist = 
  if check_collision (left_border left_tree (-x_dist)) (right_border right_tree x_dist) then
    find_valid_t left_tree right_tree (x_dist+1)
  else 
    let l = update_xpos left_tree (-x_dist) 
    in
    let r = update_xpos right_tree (x_dist)
    in
    (x_dist, l, r)

(* let rec layout tree x_off y = match tree with *)
(*   | Empty -> Empty *)
(*   | Node(v,left,right) -> *) 
(*     let xpos = x_off *)
(*     in *)
(*     let l_new = layout left x_off (y+1) *)
(*     in *)
(*     let r_new = layout right x_off (y+1) *)
(*     in *)
    

