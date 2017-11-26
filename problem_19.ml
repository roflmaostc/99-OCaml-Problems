let rotate l k= 
  let k = if k>0 then k else (List.length l)+k
  in
  let rec aux l acc counter = match l with
    | []    -> []
    | x::xs -> if counter<=k then aux xs (x::acc) (counter+1) else l@(List.rev acc)
  in 
  if k>=(List.length l) then l else aux l [] 1
