type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;


(*Returns the left border of a tree as a list of x_pos value
 * Height is not needed since every element is one discrete height-step*)
let left_border t x_off = 
  let rec left_border_to_list t x_off acc = match t with
    | Empty -> acc
    | Node ((v, xpos, ypos, x_off_new), Empty, right) -> left_border_to_list right (x_off+x_off_new) ((xpos+x_off)::acc)
    | Node ((v, xpos, ypos, x_off_new), left, _) -> left_border_to_list left (x_off -x_off_new) ((xpos+x_off)::acc)
  in
  List.rev (left_border_to_list t x_off [])


(*Returns the right border of a tree as a list of x_pos value
 * Height is not needed since every element is one discrete height-step*)
let right_border t x_off =
  let rec right_border_to_list t x_off acc = match t with
    | Empty -> acc
    | Node ((v, xpos, ypos, x_off_new), left, Empty) -> right_border_to_list left (x_off-x_off_new) ((xpos+x_off)::acc)
    | Node ((v, xpos, ypos, x_off_new), _, right) -> right_border_to_list right (x_off+x_off_new) ((xpos+x_off)::acc)
  in
  List.rev (right_border_to_list t x_off [])

(*Checks wether two border lists have a collision*)
let rec check_collision left_l right_l = match (left_l,right_l) with
  | (hl::tl, hr::tr) -> if hl>=hr then true else check_collision tl tr
  | _ -> false


let update_xpos t x_dist = match t with
  | Empty -> Empty
  | Node((v, xpos, ypos, x_offset_childs), l,r) -> Node((v, xpos, ypos, x_offset_childs+x_dist), l, r)


let rec finalize tree x_offset = match tree with
  | Empty -> Empty
  | Node((v,xpos,ypos, x_offset_childs), l, r) -> Node ((v, xpos+x_offset, ypos), finalize l (x_offset-x_offset_childs), finalize r (x_offset+x_offset_childs))

(*Shifts subtree left and right until there is no intersections of the subtrees*)
let rec find_valid_t left_tree right_tree x_dist_childs = 
  if check_collision (right_border left_tree (-x_dist_childs)) (left_border right_tree x_dist_childs) then
    (Printf.printf "Colission\n"; find_valid_t left_tree right_tree (x_dist_childs+1))
  else 
    (* let l = update_xpos left_tree (-x_dist_childs) *) 
    (* in *)
    (* let r = update_xpos right_tree (x_dist_childs) *)
    (* in *)
    (x_dist_childs, left_tree, right_tree)

let rec layout tree depth = match tree with
  | Empty -> Empty
  | Node(v,left,right) -> 
    let l_new = layout left (depth+1)
    in
    let r_new = layout right (depth+1)
    in
    let (x_offset_childs, l_new, r_new) = find_valid_t l_new r_new 1
    in
    Node((v, 0, depth, x_offset_childs), l_new, r_new)

let rec left_pos t = match t with
  | Empty -> 1
  | Node((_,x,_),l,r) -> if l=Empty then x else left_pos l

let rec shift t off = match t with
  | Empty -> Empty
  | Node((v,x,y), l,r) -> Node((v,x-off+1,y), shift l off, shift r off)

let layout_binary_tree_3 t =
  let t = finalize (layout t 1) 0
  in
  shift t (left_pos t);;
