(* Problem 22 *)
let rec range a b = let rec aux acc c = if a<=c && c<= b then aux (c::acc) (c+1) else if c>b then acc else aux acc (c+1)
  in if a>b then List.rev (range b a) else List.rev (aux [] 1);;


(*Calculates all primes up to a limit.*)
let rec sieve_era limit = 
  let rec aux l acc = match l with
  | x::xs -> (aux (List.filter (fun n -> if n mod x = 0 then false else true) l) (x::acc))
  | [] -> acc
  in List.rev(aux (range 2 limit) []);;


let rec all_primes a b = 
  let rec aux l = match l with
    | x::xs -> if x>=a then l else aux xs
    | _ -> failwith "a greater than b."
  in aux (sieve_era b);;

let () = List.iter (Printf.printf "%d ") (all_primes 2 7920);
