(* first part *)
let length_sort l= List.sort (fun x y -> let x = List.length x in let y = List.length y in compare x y) l;;


(*second part*)
let frequency_sort l =
    let rec update infos lnew flag = match infos with
      | (counter, l)::xs when l=lnew -> (counter+1,l)::xs
      | x::xs -> x::update xs lnew false
      | [] -> if flag=false then [(1,lnew)] else []
    in
    let infos = List.fold_left (fun acc x -> update acc (List.length x) false) [] l
    in
    let rec find infos l_ = match infos with 
      | (c, l)::is when l=l_ -> c
      | _::is -> find is l_
      | _ -> failwith "should not happen"
    in
    let compare a b =  if (find infos (List.length a))>(find infos (List.length b)) then 1 else -1
    in
    List.sort (compare) l;;
