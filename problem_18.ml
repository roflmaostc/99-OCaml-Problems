let slice l i k = 
  let rec aux l out counter= match l with 
    | [] -> List.rev out
    | x::xs -> if (counter<=k && counter>=i) then aux xs (x::out) (counter+1) else (
                    if counter>k then List.rev out else aux xs out (counter+1))
  in aux l [] 1;;
