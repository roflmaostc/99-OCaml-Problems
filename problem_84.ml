type ('a, 'b) labeled_graph = { nodes : 'a list; labeled_edges : ('a * 'a * 'b) list }

let neighbours v g = List.map 
    (fun (v1,v2,w ) -> if v1=v then v2 else v1) 
    (List.filter (fun (v1, v2, w)  -> v1=v || v2=v ) g.labeled_edges)

let neighbours_edges v g = List.map 
    (fun (v1,v2,w ) -> if v1=v then (v1,v2,w) else (v2,v1,w))
    (List.filter (fun (v1, v2, w)  -> v1=v || v2=v ) g.labeled_edges)



exception Valid_edge
exception Invalid_edge

(*type which indicates whether an edge can be 
 * removed (because inner edge in already reached nodes)
 * Val_edge1 means n1 is a node in spanning tree to node n2 outside spanning tree
 * Val_edge2 n2 is in spanning tree and n1 outsinde
 * Lea_edge means this edge is useful but neither n1 nor n2 is in spanning tree*)
type n = Rem_edge | Val_edge1 | Val_edge2 | Lea_edge


let minimum_tree g =
  (*stores the nodes which are in spanning_tree*)
  let nodes = [List.hd g.nodes]
  in
  let edges = List.sort (fun (_,_, v1) (_,_, v2) -> compare v1 v2) g.labeled_edges
  in
  (*edge is valid if one of n1 or n2 is in spanning tree and the other outside
   * if both are inside then edge can be removed
   * if both are outside edge must be kept*)
  let valid_edge n1 n2 nodes =
    (*List.mem can be more efficiently implemtend via Hash-Map*)
    let b1 = List.mem n1 nodes
    in
    let b2 = List.mem n2 nodes
    in
    if b1 && b2 
        then Rem_edge
    else if ( b1 && not b2) 
        then Val_edge1
    else if (not b1 && b2)  
        then Val_edge1
    else 
        Lea_edge
  in
  (*returns next valid edge and already filters unnecessary edges out*)
  let rec give_min_edge edges nodes acc = match edges with
    | (n1,n2,v)::tl -> 
       let r = (valid_edge n1 n2 nodes) 
       in
       if r=Rem_edge 
         then give_min_edge tl nodes acc
       else if r=Lea_edge 
         then give_min_edge tl nodes ((n1,n2,v)::acc)
       else if r=Val_edge1 
         then Some ((n1,n2,v), List.rev_append acc edges)
       else
         Some ((n2,n1,v), List.rev_append acc edges)
    | [] -> None 
  in
  (*here edges are searched until no edges are left and we are finished*)
  let rec aux edges s_edges nodes = 
    match give_min_edge edges nodes [] with
      | Some ((start, stop, v), edges) -> aux edges ((start,stop, v)::s_edges) (stop::nodes)
      | None -> s_edges
  in
  List.rev (aux edges [] nodes)
