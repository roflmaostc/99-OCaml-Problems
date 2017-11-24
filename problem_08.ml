(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let compress l = match l with
  | [] -> [] 
  | x::xs ->( let rec aux l ele = match l with
      | [] -> []
      | x::xs -> if x = ele then aux xs ele else x::(aux xs x) 
    in
    x::(aux l x)
  )

let test_liste = [1;3;2;10;1;1;2;1;10;10;5;5;5;4];;

let () = List.iter (Printf.printf "%d ") (compress test_liste);;
