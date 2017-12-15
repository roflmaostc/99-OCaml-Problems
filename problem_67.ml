type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec string_of_tree t = match t with
  | Empty -> ""
  | Node(x,l,r) -> 
    if l = Empty && r = Empty then (String.make 1 x)
    else (String.make 1 x)^"("^(string_of_tree l)^","^(string_of_tree r)^")"
