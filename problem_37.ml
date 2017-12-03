(* Problem 10 *)
let encode l = match l with
  | [] -> [] 
  | x::xs ->( let rec aux l ele counter= match l with
      | [] -> [(counter, ele)] 
      | x::xs -> if x = ele then aux xs ele (counter+1) else (counter, ele)::(aux xs x 1) 
    in
    (aux xs x 1)
  )


(* Problem 22 *)
let rec range a b = let rec aux acc c = if a<=c && c<= b then aux (c::acc) (c+1) else if c>b then acc else aux acc (c+1)
  in if a>b then List.rev (range b a) else List.rev (aux [] 1);;


(*Calculates all primes up to a limit.*)
let rec sieve_era limit = 
  let rec aux l acc = match l with
  | x::xs -> (aux (List.filter (fun n -> if n mod x = 0 then false else true) l) (x::acc))
  | [] -> acc
  in List.rev(aux (range 2 limit) []);;


(* Problem 31 *)
let is_prime input = 
  let rec is_prime2 input n = 
    if n>=input then true 
    else if (input mod n) =0 then false 
    else is_prime2 input (n+2)
  in
  if input=1 then false
  else if input = 2 then true
  else if (input mod 2 =0) then false
  else is_prime2 input 3;;


(*Also possible to do loop over all even integers, but I wanted to use Sieve of Erastothenes*)
let factors n = let sieve = sieve_era n
  in
  let rec aux n acc = 
    if n>1 then 
      let factor = (List.find (fun x -> if (n mod x)=0 then true else false) sieve)
      in 
      aux (n/factor) (factor::acc)
    else acc
  in
  if is_prime n then [(n,1)] else List.map (fun (a,b) -> (b,a) ) (encode (List.rev( aux n [])));;


let phi_improved n = int_of_float (List.fold_left (fun acc (a,b) -> acc*.(float a)**(float b) ) 1.0 (factors n));;

let () = Printf.printf "%d" (phi_improved 10);;
