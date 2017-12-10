type 'a binary_tree = Empty | Node of 'a*'a binary_tree*'a binary_tree
                                    
let rec single n =
  if n=0 then Empty
  else if n=1 then Node('x', Empty, Empty)
  else if n mod 2 = 1 then
    Node('x', single (n/2), single (n/2) )
  else
    Node('x', single (n/2), single (n/2-1) )


let add_trees left right acc = 
  let add_right_tree acc ln = List.fold_left (fun acc rn -> (Node('x', ln, rn))::acc) acc right
  in
  List.fold_left add_right_tree acc left;;


let rec cbal_tree n = 
  if n=0 then [Empty]
  else if n=1 then [Node('x', Empty, Empty)]
  else if n mod 2 = 1 then
    let lr = cbal_tree (n/2)
    in
    add_trees lr lr []
  else 
    let l = cbal_tree (n/2)
    in
    let r = cbal_tree (n/2-1)
    in
    add_trees l r (add_trees r l []);;
