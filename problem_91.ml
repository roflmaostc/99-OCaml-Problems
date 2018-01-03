
type 'a queen = (int * int )

let assoc_opt ele l = 
  try Some (List.assoc ele l)
  with Not_found -> None


let queens_positions limit = 
  let valid_position queens = 
    if List.length (List.sort_uniq compare (List.fold_left (fun acc (x,y) -> y::acc) [] queens)) = limit &&
    List.length (List.sort_uniq compare (List.fold_left (fun acc (x,y) -> (x+y-1)::acc) [] queens )) = limit &&
    List.length (List.sort_uniq compare (List.fold_left (fun acc (x,y) -> ((8-x)+y)::acc) [] queens)) = limit
    then true 
    else false
  in
  let increment_position queens = 
    List.fold_left 
      (fun (inc_b, acc) (x,y) -> 
        if inc_b then 
        if y=limit then
            (true, (x,1)::acc)
          else
            (false, (x,y+1)::acc)
        else
          (false, (x,y)::acc)
      ) 
      (true, []) queens
  in
  let border = int_of_float ((float_of_int limit)**(float_of_int limit)) 
  in
  let rec aux counter valid_positions queens = 
    if counter = border 
    then valid_positions
    else 
      let _,queens = increment_position queens
      in
      let queens = List.rev queens
      in
      if (valid_position queens) 
      then aux (counter+1) (queens::valid_positions) queens
      else aux (counter+1) valid_positions queens
  in
  let rec create_queens queens counter = 
    if counter = limit
    then queens
    else create_queens ((counter+1,1)::queens) (counter+1)
  in
  aux 0 [] (create_queens [] 0)

let () = Printf.printf "%d" (List.length (queens_positions 8))
