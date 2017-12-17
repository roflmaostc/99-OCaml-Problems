type 'a mult_tree = T of 'a * 'a mult_tree list

let rec string_of_tree t = 
  let rec aux t str = match t with
    | T(v,l) -> List.fold_left (fun acc ele -> acc^(aux ele "")^"^") (String.make 1 v) l 
  in
  (aux t "")^"^"



let rec tree_of_string str = 
  let rec aux tree_l str pos = 
    if pos>=(String.length str) || str.[pos]='^' then
      List.rev tree_l, (pos+1)
    else 
      let sub_tree, pos_n = aux [] str (pos+1)
      in
      aux ((T(str.[pos], sub_tree))::tree_l) str pos_n
  in
  match aux [] str 0 with
    | [t], _ -> t
    | _ -> failwith "Some error"

