(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
(* val last: 'a list -> 'a option *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | x::xs -> last xs


let extract x = match x with
  | None -> 0
  | Some x -> x

let () = Printf.printf "%d" (extract (last [1;2;3;4]))
