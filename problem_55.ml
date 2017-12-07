
(*FEHLER PASSIERT BEIM RIGHT EINFUEGEN*)

(* AVL-Tree implementation *) 
type ('a,'b) binary_tree =
    | Leaf 
    | Node of {k:'a; v:'b; bal: int; l:('a,'b) binary_tree; r: ('a,'b) binary_tree}

type direction = Left | Right;;
type height_change = Remained | Increased | Decreased;;


let update_balance (t:('a,'b) binary_tree) hchange dir = match t with
  | Leaf -> (Remained,Leaf)
  | Node{v;k;bal;l=left; r=right} as n-> 
    if hchange=Remained then (Remained, n)
    else if hchange=Decreased then 
      if dir=Right then (
        if bal<=0 then (Remained,Node{v;k;bal=bal-1; l=left; r=right})
        else (Decreased,  Node{v;k;bal=bal-1; l=left; r=right}) 
      )
      else 
        if bal>=0 then (Remained,Node{v;k;bal=(bal+1); l=left; r=right})
        else (Decreased,  Node{v;k;bal=bal+1; l=left; r=right}) 
    else (*if hchanged=Increased*)
      if dir=Right then (
        if bal>=0 then (Increased,Node{v;k;bal=(bal+1); l=left; r=right})
        else (Remained,  Node{v;k;bal=bal+1; l=left; r=right}) 
      )
      else 
        if bal<=0 then (Increased,Node{v;k;bal=(bal-1); l=left; r=right})
        else (Remained,  Node{v;k;bal=bal-1; l=left; r=right}) 


let rotate_right t = match t with
  | Leaf -> Leaf
  | Node{v;k;bal;l=Leaf; r=_} -> failwith "Don't call rotate_right with l=Leaf"
  | Node{v;k;bal;l=Node left;r=right} -> Node{v=left.v; k=left.k; bal; l=left.l; 
                                            r=Node{v;k;bal;l=left.r;r=right}}

let rotate_left t = match t with
  | Leaf -> Leaf
  | Node{v;k;bal;l=_; r=Leaf} -> failwith "Don't call rotate_left with r=Leaf"
  | Node{v;k;bal;l=left;r=Node right} -> Node{v=right.v; k=right.k; bal; 
                                              l=Node{v;k;bal;l=right.l;r=left};r=right.r}



let rebalance (h,t) = match t with
  | Leaf -> (Remained,Leaf)
  | Node{v;k;bal;l=Leaf;r=Leaf} as n -> (Remained,n)
  | Node{v;k;bal;l=left; r=right} -> 
    if bal=(-2) then (
      let Node ln = left (*This is guaranteed since -2 can only occur when left!=Leaf*)
      in
      if ln.bal<=0 then (Decreased,rotate_right (Node {v;k;bal=0; l=(Node{ln with bal=0});r=right})) 
      else (Decreased,rotate_right (Node{v;k;bal=1;l=rotate_left (Node{ln with bal=0});r=right})))
    else if bal=2 then (
      let Node rn = right (*This is guaranteed since -2 can only occur when left!=Leaf*)
      in
      if rn.bal>=0 then (Decreased,rotate_left (Node {v;k;bal=0; l=left; r=(Node{rn with bal=0})})) 
      else  
        let Node rn_ln = rn.l
        in
        let rn_ln = Node{k=rn_ln.k;v=rn_ln.v;bal=0;l=rn_ln.l;r=rn_ln.r}
        in
        (Decreased,rotate_left(Node{v;k;bal=(-1);l=left; r=(rotate_right (Node{v=rn.v;k=rn.k;bal=0;l=rn.l;r=rn_ln})) }))
    ) 
    else (h,t)

(* inserts element into tree and does rebalancing *)
(* internal the tree and balance is returned *)
let insert t key value =
  let rec aux_insert t key value from = match t with
    | Leaf -> (Increased, Node{k=key; v=value; bal=0; l=Leaf; r=Leaf})
    | Node{k; v; bal; l; r } -> 
      if key<=k then
        let hchange,l = aux_insert l key value Left
        in
        rebalance(update_balance (Node{k;v;bal;l=l;r}) hchange Left)
      else
        let hchange,r = aux_insert r key value Right
        in
        rebalance(update_balance (Node{k;v;bal;l;r=r}) hchange Right)
  in
  let hchange,t = aux_insert t key value Left
  in
  t



(*finds value to key*)
let rec find t key = match t with
  | Leaf -> None
  | Node{k;v;bal=_;l;r} -> 
    if key=k then Some v
    else if key<k then find l key
    else find r key;;


let rec rand_list k lim = if k<lim then (k, Random.float 100.0)::(rand_list (k+1) lim) else [];;
