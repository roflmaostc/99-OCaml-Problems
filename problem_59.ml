type 'a binary_tree = Empty | Node of 'a*'a binary_tree*'a binary_tree

let add_trees left right acc = 
  let add_right_tree acc ln = List.fold_left (fun acc rn -> (Node('x', ln, rn))::acc) acc right
  in
  List.fold_left add_right_tree acc left;;


let rec hbal_tree n = 
  if n=0 then [Empty]
  else if n=1 then [Node('x', Empty, Empty)]
  else
    let t1 = hbal_tree (n-1)
    in
    let t2 = hbal_tree (n-2)
    in
    add_trees t1 t1 (add_trees t1 t2 (add_trees t2 t1 []))
