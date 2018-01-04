let assoc_opt ele l = 
  try Some (List.assoc ele l)
  with Not_found -> None

(* Returns a list of possible positions for the knight *)
let jump n (x,y) visited =
  let rec aux diffs acc = match diffs with 
    | (xd, yd)::tl -> 
      if xd+x<1 || xd+x>n || yd+y<1 || yd+y>n || List.mem (xd+x,yd+y) visited then 
        aux tl acc
      else
        aux tl ((xd+x,yd+y)::acc)
    | [] -> acc
  in
  aux [(2,1); (2,-1); (-2,1);(-2,-1); (1,2); (1,-2); (-1,2); (-1,-2)] []


(*Using Warnsdorfs rule only 
 * Doesn't differ between closed and unclosed tours*)
let knights_tour n start_x start_y =
  (*looping until we reached all squares or we stuck*)
  let rec aux tour pos counter =
    (*if we have visited n*n squares then we found a solution*)
    if counter = n*n then
      tour
    else
      let possible_ways = jump n pos tour 
      in
      (*if there is no way and counter!=n then we didn't find a solution*)
      if possible_ways = [] then 
        failwith "no solution"
      else
        (*here we search for the square with fewest possible ways*)
        let _,new_pos = 
          List.fold_left 
            (fun (min_l, min_pos) pos -> 
               let l = List.length (jump n pos tour)
               in
               if l<min_l && (l>0 || counter=n*n-1) then
                 (l, pos)
               else 
                 (min_l, min_pos)
            )
            (*8 because no field has 8 possibilities and (0,0) because
             * it isnt on the chessboard*)
            (8,(0,0)) possible_ways
      in
      aux (new_pos::tour) new_pos (counter+1) 
  in
  List.rev (aux [start_x, start_y] (start_x, start_y) 1)

