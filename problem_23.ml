(*use previous used function*)

(*problem 21*)
let remove_at k l = let liste, _ =List.fold_left (fun (l, c) ele->  if k=c then (l,c+1) else (ele::l,c+1)) ([],0) l
    in List.rev liste;;

let rec rand_select l c = if c> 0 then 
    let i = Random.int (List.length l) in
    (List.nth l i)::(rand_select (remove_at i l) (c-1))
  else [];;
(* Random.self_init;; *)
(* let res = rand_select [1;2;3;4;5;6;8;9;10] 3;; *)
(* List.iter (Printf.printf "%d ") res;; *)
