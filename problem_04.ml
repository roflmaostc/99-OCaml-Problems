(* 4. Find the number of elements of a list. (easy) *)

let length l = let rec helper l counter = match l with
    | [] -> counter 
    | x::xs -> helper xs (counter+1) in
  helper l 0


let () = Printf.printf "%d" (length [1])
