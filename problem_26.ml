let rec extract k elems =
  let rec aux k elems acc = if k=0 then acc else(
    match elems with
        | [] -> [] 
        | x::xs -> (aux k xs acc)@(aux (k-1) xs (List.map (fun y-> x::y) acc))
  )
  in aux k elems [[]];;

let lol=[1;2;3;4;5;6]
let test = extract 3 [1;2;3;4;5;6];;
(* let () = List.iter (Printf.printf "%d ") lol;; *)
(* let test = [[1;2];[3;4]];; *)
List.iter (fun x-> match x with a::b::c::[] -> Printf.printf "[%d;%d;%d] " a b c;) test;;
