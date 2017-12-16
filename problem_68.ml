type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec preorder t = match t with
  | Empty -> []
  | Node(v,l,r) -> v::(preorder l )@(preorder r);;

let rec inorder t = match t with
  | Empty -> []
  | Node(v,l,r) -> (inorder l)@[v]@(inorder r);;

