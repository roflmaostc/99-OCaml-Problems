(*problem 26*)
let rec extract k elems =
  let rec aux k elems acc = if k=0 then acc else(
    match elems with
        | [] -> [] 
        | x::xs -> (aux k xs acc)@(aux (k-1) xs (List.map (fun y-> x::y) acc))
  )
  in aux k elems [[]];;

(*remove sub of base*)
let set_diff base sub = 
  let rec not_contains elem ls = match ls with
    | x::xs -> if x=elem then false else (not_contains elem xs)
    | [] -> true
  in
  List.filter (fun x -> not_contains x sub)base;; 


let rec low_combine l permus = match permus with
  | [] -> []
  | p::ps -> (p::l)::(low_combine l ps);;
(* combine [[1;2;3]] [[4];[5]] -> [[[4]; [1; 2; 3]]; [[5]; [1; 2; 3]]] *)

let rec high_combine ori set k = match set with
  | x::xs -> (low_combine x (extract k (set_diff ori (List.flatten x))))@(high_combine ori xs k)
  | [] ->[];;

(*iterating over all group sizes*)
let rec group2 ori groups acc= match groups with
  | x::xs -> group2 ori xs (high_combine ori acc x)
  |[] -> acc

(*wrapper to init whole list*)
let rec group l g = match g with 
  | x::xs -> group2 l xs (List.map(fun x -> [x]) (extract x l))
  | [] -> [];;
