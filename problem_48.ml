type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let rec eval expr state = match expr with
  | Var x -> state x
  | Not expr -> not (eval expr state)
  | And (aexpr, bexpr) -> (eval aexpr state) && (eval bexpr state)
  | Or (aexpr, bexpr) -> (eval aexpr state) || (eval bexpr state);;


let rec make_state_function var info = fun x -> match (var,info) with
  | (h1::t1, h2::t2) -> if h1=x then h2 else make_state_function t1 t2 x
  | _ -> failwith "Variable not defined";;

(*Not tail recursive, but not many variables normally*)
let rec count_up x l = match l with
  | h::t -> 
    if x=false && h=false then false::(count_up x t)
    else if h=false && x=true then true::(count_up false t)
    else if h=true && x=false then true::(count_up x t)
    else if h=true && x=true then false::(count_up x t)
    else failwith "Doesn't happen"
  | [] -> []

let rec false_list length = 
  let rec aux k acc = if k<=length then aux (k+1) (false::acc) else acc
  in
  aux 1 [];;

let table vars expr = 
    let limit = int_of_float((float 2)**(float (List.length vars)))
    in
    let rec aux counter state_list acc =
      if counter>limit then acc
      else
        let state_list = count_up true state_list 
        in
        let status_fun = make_state_function vars state_list
        in
        let truth_line = List.map (fun x -> (x, status_fun x)) vars
        in
        aux (counter+1) state_list (((truth_line, eval expr status_fun))::acc)
    in
    aux 1 (false_list (List.length vars)) [];;


(* let table var expr = *) 
