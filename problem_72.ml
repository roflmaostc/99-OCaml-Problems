type 'a mult_tree = T of 'a * 'a mult_tree list

let bottom_up t = 
  let rec aux (T(v,eles)) l = 
    List.fold_right (fun ele acc -> aux ele acc) eles (v::l) 
  in
  aux t
