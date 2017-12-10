let rec replicate l counter = 
  let rec aux l c = match l with
    | [] -> []
    | x::xs -> if c>0 then x::(aux l (c-1)) else aux xs counter
  in
  aux l counter;;
