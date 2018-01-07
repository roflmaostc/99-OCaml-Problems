exception Complete

(* Checks wether a element at (x,y) violates any of the rules*)
let violation s x y ele =
  let rec vio_horizontal c =
    if c >= 9 then false
    else if s.(y).(c)=ele then true
    else vio_horizontal (c+1)
  in
  let rec vio_vertical c =
    if c >= 9 then false
    else if s.(c).(x)=ele then true
    else vio_vertical (c+1)
  in
  let rec vio_box c =
    if c >= 9 then false
    else if s.(3*(y/3)+c/3).(3*(x/3)+(c mod 3))=ele then true
    else vio_box (c+1)
  in
  vio_horizontal 0 || vio_vertical 0 || vio_box 0

let rec next_field (x,y) board = 
  if x=8 then
    if y=8 then
      raise Complete
    else if board.(y+1).(0) = 0 then
      (0,y+1)
    else 
      next_field (0,y+1) board
  else if board.(y).(x+1) = 0 then
    (x+1,y)
  else 
    next_field (x+1,y) board

let solve board = 
  let rec aux changed (x,y) number = 
    if number=10 then
      let () = board.(y).(x) <- 0
      in
      let x,y, number = List.hd changed
      in
      let () = board.(y).(x) <- 0
      in
      aux (List.tl changed) (x,y) (number+1)
    else
      if violation board x y number then
        aux changed (x,y) (number+1)
      else
        let() = board.(y).(x) <- number
        in
        aux ((x,y,number)::changed) (next_field (x,y) board) 1
  in 
  try aux [] (next_field (-1,0) board) 1 
  with Complete -> 0

(* let hard_board = [|[|0;0;0;0;0;0;0;0;0|];[|0;0;0;0;0;3;0;8;5|];[|0;0;1;0;2;0;0;0;0|];[|0;0;0;5;0;7;0;0;0|];[|0;0;4;0;0;0;1;0;0|];[|0;9;0;0;0;0;0;0;0|];[|5;0;0;0;0;0;0;7;3|];[|0;0;2;0;1;0;0;0;0|];[|0;0;0;0;4;0;0;0;9|]|];; *)
(* solve hard_board;; *)
