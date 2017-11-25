let split l cut = 
  let rec aux l out1 out2 counter = match l with 
    | [] -> (List.rev out1, List.rev out2)
    | x::xs -> if counter<=cut then aux xs (x::out1) out2 (counter+1) else aux xs out1 (x::out2) (counter+1)
  in aux l [] [] 1;;


