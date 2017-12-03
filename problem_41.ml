(* Problem 22 *)
let rec range a b = let rec aux acc c = if a<=c && c<= b then aux (c::acc) (c+1) else if c>b then acc else aux acc (c+1)
  in if a>b then List.rev (range b a) else List.rev (aux [] 1);;


(*Calculates all primes up to a limit. Sieve of Eratosthenes*)
let rec sieve_era limit = 
  let rec aux l acc = match l with
  | x::xs -> (aux (List.filter (fun n -> if n mod x = 0 then false else true) l) (x::acc))
  | [] -> acc
  in List.rev(aux (range 2 limit) []);;


let goldbach n siev= 
  let rec aux l1 l2 = match (l1,l2) with
    | (h1::t1, h2::t2) -> 
      if (h1+h2)<n then aux t1 l2 
      else if (h1+h2)>n then aux l1 t2
      else (h1, h2)
    | (_,_) -> failwith "Goldbach disproved!\n Or maybe your input was not even"
  in
  aux siev (List.rev siev);;

let goldbach_list a b =
  let siev = sieve_era b
  in
  let even = List.filter (fun x -> x mod 2 =0 && x<>2) (range a b)
  in
  List.map (fun x -> (x,goldbach x siev) ) even;; 


let rec goldbach_limit a b limit = List.filter (fun (x,(a,b)) -> a>=limit) (goldbach_list a b);;


(* List.iter (fun a -> Printf.printf "%d, " a) (goldbach_list 4 2000);; *)
List.iter (fun (x,(a,b)) -> Printf.printf "(%d,(%d,%d)), " x a b) (goldbach_limit 1 200000 50);;
