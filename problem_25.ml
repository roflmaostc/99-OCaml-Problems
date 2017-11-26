
let remove_at k l = let liste, _ =List.fold_left (fun (l, c) ele->  if k=c then (l,c+1) else (ele::l,c+1)) ([],0) l
    in List.rev liste;;

let rec rand_select l c = if c> 0 then 
    let i = Random.int (List.length l) in
    (List.nth l i)::(rand_select (remove_at i l) (c-1))
  else [];;


let permutation l = rand_select l (List.length l);;

