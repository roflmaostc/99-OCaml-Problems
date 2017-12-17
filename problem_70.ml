type 'a mult_tree = T of 'a * 'a mult_tree list

let rec string_of_tree t = 
  let rec aux t str = match t with
    | T(v,l) -> List.fold_left (fun acc ele -> acc^(aux ele "")^"^") (String.make 1 v) l 
  in
  (aux t "")^"^"



