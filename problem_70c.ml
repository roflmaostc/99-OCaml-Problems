type 'a mult_tree = T of 'a * 'a mult_tree list;;

let rec count_nodes (T(_,l)) = List.fold_left (fun acc n -> acc+(count_nodes n) ) 1 l
