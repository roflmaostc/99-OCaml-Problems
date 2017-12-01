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
