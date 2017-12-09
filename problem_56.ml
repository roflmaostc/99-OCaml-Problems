type 'a binary_tree = Empty | Node of 'a*'a binary_tree*'a binary_tree

let rec is_mirror t1 t2 = match (t1,t2) with
  | (Empty, Empty) -> true
  | (Node(_,l1,r1),Node(_,l2,r2)) -> is_mirror r1 l2 && is_mirror l1 r2
  | (Node(_), Empty) -> false
  | (Empty, Node(_)) -> false

let is_symmetric t = match t with
  | Empty -> true
  | Node(_,l,r) -> is_mirror l r
