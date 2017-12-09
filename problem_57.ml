type 'a binary_tree = Empty | Node of 'a*'a binary_tree*'a binary_tree


let rec insert t ele = match t with
  | Empty -> Node(ele, Empty, Empty)
  | Node(v, l,r) -> if ele<=v then Node(v, insert l ele, r) else Node(v, l,insert r ele)

let construct l= List.fold_left (fun tree x -> insert tree x ) Empty l;; 

