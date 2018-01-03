type ('a, 'b) labeled_graph = { nodes : 'a list;
                                  labeled_edges : ('a * 'a * 'b) list };;
let neighbours v g = List.map 
    (fun (v1,v2, _) -> if v1=v then v2 else v1) 
    (List.filter (fun (v1, v2, w)  -> v1=v || v2=v ) g.labeled_edges)

type 'a queue = ('a list * 'a list)

let empty_queue = ([],[])

let enqueue (inq, outq) ele = (ele::inq,outq)
                              
let dequeue (inq, outq) = match outq with
  | hd::tl -> (hd, (inq, tl))
  | [] ->
    match List.rev inq with
      | [] -> failwith "Empty queue"
      | ele::outq -> (ele, ([], outq))

let bfs g f init_acc start_node =
  let rec aux acc queue visited =
    if queue = ([],[]) then
      acc
    else
      let ele, queue = dequeue queue
      in
      let queue, visited = List.fold_left 
          (fun (acc,visited) x -> 
            if (List.mem x visited) then 
              (acc,visited) 
            else 
              (enqueue acc x, x::visited)) 
          (queue, visited) (neighbours ele g)
      in
      aux (f acc ele) queue (visited)
  in
  aux [] (enqueue empty_queue start_node) [start_node] 


let is_bipartite g =
  let rec aux acc queue visited =
    if queue = ([],[]) then
      acc
    else
      let (ele, ele_color), queue = dequeue queue
      in
      let color = if ele_color="black" then "white" else "black"
      in
      let queue, visited = List.fold_left 
          (fun (acc,visited) x -> 
            if (List.mem x visited) then 
              (acc,visited) 
            else 
              (enqueue acc (x, color), x::visited)) 
          (queue, visited) (neighbours ele g)
      in
      aux ((ele, ele_color)::acc) queue (visited)
  in
  let coloring = aux [] (enqueue empty_queue (List.hd g.nodes, "black")) [List.hd g.nodes]
  in
  List.for_all 
  (fun (ele,color) -> 
    List.for_all
    (fun (ele2) -> (List.assoc ele2 coloring)!=color) 
    (neighbours ele g) ) 
  coloring
  
