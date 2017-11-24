


let encode l = match l with
  | [] -> [] 
  | x::xs ->( let rec aux l ele counter= match l with
      | [] -> [(counter, ele)] 
      | x::xs -> if x = ele then aux xs ele (counter+1) else (counter, ele)::(aux xs x 1) 
    in
    (aux xs x 1)
  )

(* let test_liste = [1;3;2;10;1;1;2;1;10;10;5;5;5;4];; *)
let test_liste = ["a"; "a"; "b"; "b"; "b"; "c"; "a"; "a"; "d"];;
let () = List.iter (Printf.printf "%s " ) test_liste;;
Printf.printf "\n";;
let () = List.iter (fun (a,b) -> Printf.printf "(%d, %s)," a b;) (encode test_liste);;


