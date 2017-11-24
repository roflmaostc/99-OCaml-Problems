(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)


let rec last_two l = match l with
  | [] | [_] -> None
  | [a;b] ->  Some (a,b)
  | _::xs -> last_two xs;;

let extract t = match t with
  | None -> (0,0)
  | Some (a,b) -> (a,b)


let () = let a,b = extract (last_two [10])
  in Printf.printf "%d %d" a b
