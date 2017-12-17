type 'a mult_tree = T of 'a * 'a mult_tree list

let rec length tree = 
  let rec aux tree s= match tree with
    | T(_, l) -> (List.fold_left (fun acc x -> acc + (aux x (s+1))) s l ) 
  in
  aux tree 0
