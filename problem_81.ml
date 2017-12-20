type 'a node = 'a
type 'a graph = ('a node * 'a * 'a node) list


let add_weight graph = List.rev_map (fun (a,b) -> (a,1,b)) graph 

let neighbours node graph = 
  let rec aux l acc = match l with
  | (n1, w, n2)::t -> aux t 
                        (if n1=node then n2::acc 
                         else if n2=node then n1::acc
                         else acc)
  | [] -> acc
  in aux graph []
    

let paths g a b = 
  let rec aux node way =
    if node = a then [way]
    else
      let nbs = neighbours node g 
      in
      let nbs = List.filter (fun x -> not (List.mem x way) ) nbs
      in
      if nbs = []
      then []
      else List.fold_left (fun acc x -> (List.flatten (aux x (x::way)))::acc ) [] nbs
  in
  List.filter (fun x -> x!=[]) (aux b [b]);;
