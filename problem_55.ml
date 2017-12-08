(* AVL-Tree implementation *) 
type ('a,'b) binary_tree =
    | Leaf 
    | Node of {k:'a; v:'b; bal: int; l:('a,'b) binary_tree; r: ('a,'b) binary_tree}

type direction = Left | Right;;
type height_change = Remained | Increased | Decreased;;

let rec max_tree t = match t with
  | Leaf -> None
  | Node{k;v;r;_} -> if r=Leaf then Some (k,v) else max_tree r;;

let rec min_tree t = match t with
  | Leaf -> None
  | Node{k;v;l;_} -> if l=Leaf then Some(k,v) else min_tree l;;


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
  | Leaf -> (Remained,Leaf)
  | Node{v;k;bal;l=Leaf; r=_} -> failwith "Don't call rotate_right with l=Leaf"
  | Node{v;k;bal;l=Node left;r=right} -> 
    (* Node{v=left.v; k=left.k; bal=0; l=left.l; *) 
    (*                                         r=Node{v;k;bal=0;l=left.r;r=right}} *)
   if left.bal=0 then (Remained,Node{v=left.v; k=left.k; bal=1; 
                           l=left.l;r=Node{v;k;bal=(-1);l=left.r;r=right}})
    else  (Remained,Node{v=left.v; k=left.k; bal=0; 
                           l=left.l;r=Node{v;k;bal=0;l=left.r;r=right}})

let rotate_left t = match t with
  | Leaf -> (Remained,Leaf)
  | Node{v;k;bal;l=_; r=Leaf} -> failwith "Don't call rotate_left with r=Leaf. Error in rebalance"
  | Node{v;k;bal;l=left;r=Node right} ->
    if right.bal=0 then (Remained,Node{v=right.v; k=right.k; bal=(-1); 
                                l=Node{v;k;bal=1;l=left;r=right.l};r=right.r})
    else  (Remained, Node{v=right.v; k=right.k; bal=0; 
                                l=Node{v;k;bal=0;l=left;r=right.l};r=right.r})



(*Look up wikipedia for the rules*)
let rotate_right_left t = match t with
  | Node{v;k;bal;l=left; r=Node{k=r_k; v=r_v;bal=r_bal; l=Node{k=rl_k; v=rl_v; bal=rl_bal; l=rl_l; r=rl_r}; r=r_r}} ->
    if rl_bal>0 then 
      (Remained, Node{v=rl_v;k=rl_k;bal=0; l=Node{v;k;bal=(-1);l=left;r=rl_l};r=Node{k=r_k;v=r_v;bal=0; l=rl_r;r=r_r}})
    else if rl_bal=0 then
      (Remained, Node{v=rl_v;k=rl_k;bal=0; l=Node{v;k;bal=0;   l=left;r=rl_l};r=Node{k=r_k;v=r_v;bal=0; l=rl_r;r=r_r}})
    else
      (Remained, Node{v=rl_v;k=rl_k;bal=0; l=Node{v;k;bal=0;   l=left;r=rl_l};r=Node{k=r_k;v=r_v;bal=1; l=rl_r;r=r_r}})
  | _ -> failwith "Something inside rotate_right_left went wrong"


let rotate_left_right t = match t with
  | Node{v;k;bal;l=Node{k=l_k; v=l_v;bal=l_bal; l=l_l;r=Node{k=lr_k; v=lr_v; bal=lr_bal; l=lr_l; r=lr_r}}; r=right} ->
    if lr_bal<0 then 
      (Remained, Node{v=lr_v;k=lr_k;bal=0; l=Node{v=l_v;k=l_k;bal=0;   l=l_l;r=lr_l};r=Node{v;k;bal=1;l=lr_r; r=right}}) 
    else if lr_bal=0 then
      (Remained, Node{v=lr_v;k=lr_k;bal=0; l=Node{v=l_v;k=l_k;bal=0;   l=l_l;r=lr_l};r=Node{v;k;bal=0;l=lr_r; r=right}}) 
    else
      (Remained, Node{v=lr_v;k=lr_k;bal=0; l=Node{v=l_v;k=l_k;bal=(-1);l=l_l;r=lr_l};r=Node{v;k;bal=0;l=lr_r; r=right}}) 
  | _ -> failwith "Something inside rotate_left_right went wrong"



(*rebalance checks what to do. But rotating and chaning of balance weights is done in rotate_right etc.*)
let rebalance (h,t) = match t with
  | Leaf -> (Remained,Leaf)
  | Node{v;k;bal;l=Leaf;r=Leaf} as n -> (Remained,n)
  | Node{v;k;bal;l=left; r=right} -> 
    if bal=(-2) then 
      let Node left = left
      in
      if left.bal<=0 then rotate_right (Node {v;k;bal; l=Node left;r=right}) 
      else rotate_left_right t 
    else if bal=2 then
      let Node right = right
      in
      if right.bal>=0 then rotate_left (Node {v;k;bal; l=left; r=Node right}) 
      else rotate_right_left t 
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

let rec remove t key =
  let rec aux_remove t key from =
    match t with
      | Leaf -> failwith "key not in tree"
      | Node{k; v; bal; l; r} -> 
        if k=key then match (max_tree l) with 
            | None -> (Decreased, r) 
            | Some (k,v) -> 
              let hchange,l = aux_remove l k Left
              in
              rebalance(update_balance (Node{k;v;bal;l;r}) hchange Left)
        else if key<k then
          let hchange,l = aux_remove l key Left
          in
          rebalance(update_balance (Node{k;v;bal;l;r}) hchange Left)
        else 
          let hchange,r = aux_remove r key Right
          in
          rebalance(update_balance (Node{k;v;bal;l;r}) hchange Right)
  in
  let hchange, t = aux_remove t key Left
  in
  t




let rec fold_left f acc tree = match tree with
  | Leaf -> acc
  | Node{k=x;v=v;bal;l;r} -> fold_left f (f x (fold_left f acc l)) r ;;


let to_list tree = List.rev (fold_left (fun x acc-> x::acc) [] tree);;


(*finds value to key*)
let rec find t key = match t with
  | Leaf -> None
  | Node{k;v;bal=_;l;r} -> 
    if key=k then Some v
    else if key<k then find l key
    else find r key;;


let rec rand_list k lim = if k<lim then (k, Random.float 100.0)::(rand_list (k+1) lim) else [];;

(*from 99problems solution*)
let rec permutation list =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux acc list len =
      if len = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (picked :: acc) rest (len-1)
    in
    aux [] list (List.length list);;


let rand_tree size = List.fold_left (fun t (key, value) -> insert t key value) Leaf (permutation(rand_list 0 size));;


