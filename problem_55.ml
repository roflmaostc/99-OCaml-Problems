type 'a binary_tree = Empty | Node of 'a*'a binary_tree*'a binary_tree
                                    
let rec cbal_tree n =
  if n=0 then Empty
  else if n=1 then Node('x', Empty, Empty)
  else if n mod 2 = 1 then
    Node('x', cbal_tree (n/2), cbal_tree (n/2) )
  else
    Node('x', cbal_tree (n/2), cbal_tree (n/2-1) )
