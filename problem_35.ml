(* Problem 22 *)
let rec range a b = let rec aux acc c = if a<=c && c<= b then aux (c::acc) (c+1) else if c>b then acc else aux acc (c+1)
  in if a>b then List.rev (range b a) else List.rev (aux [] 1);;


(*Calculates all primes up to a limit.*)
let rec sieve_era limit = 
  let rec aux l acc = match l with
  | x::xs -> (aux (List.filter (fun n -> if n mod x = 0 then false else true) l) (x::acc))
  | [] -> acc
  in List.rev(aux (range 2 limit) []);;


(*Also possible to do loop over all even integers, but I wanted to use Sieve of Erastothenes*)
let factors n = let sieve = sieve_era (int_of_float(sqrt(float n)))
  in
  let rec aux n acc = 
    if n>1 then 
      let factor = (List.find (fun x -> if (n mod x)=0 then true else false) sieve)
      in 
      aux (n/factor) (factor::acc)
    else acc
  in List.rev( aux n []);;

let () = List.iter (Printf.printf "%d ") (factors 315);;
