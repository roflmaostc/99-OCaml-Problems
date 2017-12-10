(*problem 21*)
let remove_at k l = let liste, _ =List.fold_left (fun (l, c) ele->  if k=c then (l,c+1) else (ele::l,c+1)) ([],0) l
    in List.rev liste;;

    (* problem 22 *)
let rec range a b = let rec aux acc c = if a<=c && c<= b then aux (c::acc) (c+1) else if c>b then acc else aux acc (c+1)
  in if a>b then List.rev (range b a) else List.rev (aux [] 1);;

(*problem 24*)
let rec rand_select l c = if c> 0 then 
    let i = Random.int (List.length l) in
    (List.nth l i)::(rand_select (remove_at i l) (c-1))
  else [];;

let lotto_select n k = rand_select (range 1 k) n;;
