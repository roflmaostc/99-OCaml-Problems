type ('a, 'b) labeled_graph = { nodes : 'a list;
                                  labeled_edges : ('a * 'a * 'b) list };;
let neighbours v g = List.map 
    (fun (v1,v2, _) -> if v1=v then v2 else v1) 
    (List.filter (fun (v1, v2, w)  -> v1=v || v2=v ) g.labeled_edges)

(* type 'a queue = ('a list * 'a list) *)

(* let empty_queue = ([],[]) *)

(* let enqueue (inq, outq) ele = (ele::inq,outq) *)
                              
(* let dequeue (inq, outq) = match outq with *)
(*   | hd::tl -> (hd, (inq, tl)) *)
(*   | [] -> *)
(*     match List.rev inq with *)
(*       | [] -> failwith "Empty queue" *)
(*       | ele::outq -> (ele, ([], outq)) *)


let dfs g f init_acc start_node = 
  (*folds a function in order over graph nodes*)
  let rec aux visited n acc =
    let rec iterate nodes_neighbours acc visited = 
      match nodes_neighbours with
        | [] -> (visited, acc) 
        | hd::tl -> 
          (*more efficient with hash map*)
          if (List.mem hd visited) then
            iterate tl acc visited
          else
            let visited_n, acc_n = aux visited hd acc
            in
            iterate tl acc_n visited_n
    in
    iterate (neighbours n g) (f acc n) (n::visited)
  in
  snd (aux [start_node] start_node init_acc) 
