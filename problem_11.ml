type 'a rle =
    | One of 'a
    | Many of int * 'a;;
type 'a rle = One of 'a | Many of int * 'a

let encode l = match l with
  | [] -> [] 
  | x::xs ->( let rec aux l ele counter= match l with
      | [] -> if counter = 1 then [One(ele)] else [Many(counter, ele)]
      | x::xs -> if x = ele then aux xs ele (counter+1) else (
          if counter = 1 then One(ele)::(aux xs x 1) else Many(counter, ele)::(aux xs x 1) )
    in
    (aux xs x 1)
  )



