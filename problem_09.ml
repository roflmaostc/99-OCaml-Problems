(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack l = match l with
  | [] -> []
  | x::xs -> (
        let rec aux l ele acc out = match l with
          | [] -> (acc)::out
          | x::xs -> if x=ele then aux xs ele (x::acc) out else aux xs x [x] (acc::out) 
        in
        List.rev (aux xs x [x] [])
      );;

let test_liste = [1;3;2;10;1;1;2;1;10;10;5;5;5;4];;
let print_list l = Printf.printf "["; List.iter (Printf.printf "%d; ") l; Printf.printf "]";;
print_list test_liste;;
let () = Printf.printf "\n";;
let () = List.iter (print_list) (pack test_liste)
(* let () *) 
