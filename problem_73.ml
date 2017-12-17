type 'a mult_tree = T of 'a * 'a mult_tree list

let lispy t = 
  let rec aux t = match t with
    | T(v, []) -> String.make 1 v
    | T(v,l) -> 
      "("^(String.make 1 v)^(List.fold_right (fun x acc -> (aux x)^acc) l "") ^ ")"
  in
  aux t
