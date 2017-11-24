(* 3. Find the k'th element of a list. (easy) *)


let rec at i l = match l with
  | [] -> None
  | x::xs -> if i=1 then Some x else at (i-1) xs 

let extract t = match t with
  | None -> 0
  | Some x -> x


let () = let res = extract (at 1 [1;2;3;4;5]) in
    Printf.printf "%d" res;
