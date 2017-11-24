let rev l = 
  let rec aux l_old l_new = match l_old with
    | [] -> l_new
    | x::xs -> aux xs (x::l_new) in
  aux l []

let is_palindrome l = 
  let rec aux l1 l2 = match (l1,l2) with
    | (x1::x1s, x2::x2s) -> aux x1s x2s
    | ([], []) -> true
    | _ -> false in
  aux l (rev l)


let () = Printf.printf "%b" (is_palindrome [1;2;3;4;1;1])
