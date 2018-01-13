let rec duplicate = function
  | [] -> []
  | x::xs -> x::x::(duplicate xs)

