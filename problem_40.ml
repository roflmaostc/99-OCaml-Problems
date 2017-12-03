(* Problem 22 *)
let rec range a b = let rec aux acc c = if a<=c && c<= b then aux (c::acc) (c+1) else if c>b then acc else aux acc (c+1)
  in if a>b then List.rev (range b a) else List.rev (aux [] 1);;


(*Calculates all primes up to a limit. Sieve of Eratosthenes*)
let rec sieve_era limit = 
  let rec aux l acc = match l with
  | x::xs -> (aux (List.filter (fun n -> if n mod x = 0 then false else true) l) (x::acc))
  | [] -> acc
  in List.rev(aux (range 2 limit) []);;


let goldbach n = 
  let rec aux l1 l2 = match (l1,l2) with
    | (h1::t1, h2::t2) -> 
      if (h1+h2)<n then aux t1 l2 
      else if (h1+h2)>n then aux l1 t2
      else (h1, h2)
    | _ -> failwith "Goldbach disproved!\n Or maybe your input was not even"
  in
  aux (sieve_era n) (List.rev (sieve_era n));;

let a,b = goldbach 28;;
let () = Printf.printf "(%d, %d)" a b;;
