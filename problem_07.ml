(* # flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];; *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let test_liste2 = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

let test_liste= [One(3); One(12); Many([One(40);One(50); Many([One(31); One(49); One(100)])])];;

(*not tail recursive and uses @*)
let flatten l = 
  let rec aux l = match l with
    | [] -> []
    | One(x)::xs -> x::(aux xs)
    | Many(x)::xs -> (aux x)@(aux xs)
  in
  aux l;;


let flatten2 l =
  let rec aux l acc = match l with
    | [] -> acc
    | One(x)::xs -> aux xs (x::acc)
    | Many(x)::xs -> aux xs (aux x acc)
  in
  List.rev (aux l []);;

let () = List.iter (Printf.printf "%d ") (flatten2 test_liste);;
