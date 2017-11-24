type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode input = 
  let rec list_create acc ele counter = if counter=0 then acc else list_create (ele::acc) ele (counter-1)
  in
  let rec aux l out = match l with
    | [] -> out
    | x::xs -> ( match x with
        | One(a) -> aux xs (a::out)
        | Many(n,var) -> aux xs (list_create out var n) 
    )
  in
  List.rev (aux input []);;
