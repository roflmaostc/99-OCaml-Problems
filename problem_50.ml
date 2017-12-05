(*Compiling via ocamlfind ocamlc -o a.ou -linkpkg -package psq problem_50.ml*)
(*opam install psq*)
(* open Psq *)


(*Straightforward queue implementation from http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html*)
module PrioQueue =
    struct
      type priority = float
      type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
      let empty = Empty
      let rec insert queue prio elt =
        match queue with
          Empty -> Node(prio, elt, Empty, Empty)
        | Node(p, e, left, right) ->
            if prio <= p
            then Node(prio, elt, insert right p e, left)
            else Node(p, e, insert right prio elt, left)
      exception Queue_is_empty
      let rec remove_top = function
          Empty -> raise Queue_is_empty
        | Node(prio, elt, left, Empty) -> left
        | Node(prio, elt, Empty, right) -> right
        | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                          (Node(rprio, relt, _, _) as right)) ->
            if lprio <= rprio
            then Node(lprio, lelt, remove_top left, right)
            else Node(rprio, relt, left, remove_top right)
      let extract = function
          Empty -> raise Queue_is_empty
        | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
    end;;


open PrioQueue

type 'a tree = Leaf | NodeTree of {v:'a; l:'a tree; r:'a tree}

let rec to_code acc tree str = match tree with
  | NodeTree{v=va; l=Leaf; r=Leaf} -> (va, str)::acc
  | NodeTree{v=va;l=l; r=r} -> to_code (to_code acc l (str^"0")) r (str^"1")
  | _ -> failwith "Does not happen";;

let huffman (l:(string*int) list) = 
  let l_sorted = List.sort (fun (a1, b1) (a2, b2) -> if b1>=b2 then 1 else -1) l  (*sorts according to occurrences*)
  in
  let base = List.fold_left (fun acc (var, freq) -> acc+freq) 0 l_sorted    (*sum of all occurrences*)
  in
  let l_probs = List.rev_map (fun (a,b) -> (a,(float b)/.(float base))) l_sorted    (*probality insteaf of occurence*)
  in
  let queue = List.fold_left (fun queue (var,prio) -> insert queue prio (NodeTree {v=var; l=Leaf; r=Leaf})) Empty l_probs     (*Initializes priority queue*)
  in
  let rec aux queue = 
    let (prio1, ltree , queue) = extract queue 
    in
    match queue with
        | Empty -> insert Empty prio1 ltree 
        | _ -> 
          let (prio2, rtree , queue) = extract queue
          in
          aux (insert queue (prio1+.prio2) (NodeTree {v=" "; l=ltree; r=rtree}))
  in 
  let (_,tree,_) = extract(aux queue)
  in
  to_code [] tree "";;











