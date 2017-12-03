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

let rec gcd a b =
    if b = 0 then a else gcd b (a mod b);;

let coprime a b = (gcd a b)=1;;

let phi m = let rec aux x acc =
              if x<m then (if coprime m x then aux (x+1) (acc+1) else aux (x+1) acc )
              else acc
  in 
  aux 2 1;;

let () = Printf.printf "%d" (phi 13);;
