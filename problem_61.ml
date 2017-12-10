type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


(*straightforward non tail recursive*)
let rec count_leaves t = match t with 
  | Empty -> 0
  | Node (_, l,r ) -> 1+(count_leaves l)+(count_leaves r)


type cont = Cont of (int -> cont list ->int)

(*tail recursive version*)
let rec count_leaves2 t = 
  let rec aux (t:'a binary_tree) (counter:int) (funs:cont list) = match t with
    | Empty -> (match funs with
      | [] -> counter
      | (Cont h)::ts -> h counter ts)
    | Node(v,left,right) -> aux left (counter+1) ( (Cont (fun c tail -> aux right c tail))::funs)
  in
  aux t 0 []

let leaves t =
  let rec leaves_aux t acc = match t with
      | Empty -> acc 
      | Node(x, Empty, Empty) -> x::acc
      | Node(x, l, r) -> leaves_aux l (leaves_aux r acc) 
  in  
  leaves_aux t [];;
