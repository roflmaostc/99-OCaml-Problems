(* 5. Reverse a list. (easy) *)

let rev l = 
  let rec aux l_old l_new = match l_old with
    | [] -> l_new
    | x::xs -> aux xs (x::l_new) in
  aux l []

let () = List.iter (Printf.printf "%d " ) (rev [123;523;41;23;52;32])
