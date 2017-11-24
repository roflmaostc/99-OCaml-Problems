let drop l n = 
  let rec aux l counter = match l with
    | [] -> []
    | x::xs -> if counter>1 then x::(aux xs (counter-1)) else (aux xs n)
  in
  aux l n;;
