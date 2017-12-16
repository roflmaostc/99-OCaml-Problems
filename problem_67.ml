type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec string_of_tree t = match t with
  | Empty -> ""
  | Node(x,l,r) -> 
    if l = Empty && r = Empty then (String.make 1 x)
    else (String.make 1 x)^"("^(string_of_tree l)^","^(string_of_tree r)^")"



let tree_of_string str = 
  let rec aux str pos = 
    if pos>=String.length str || str.[pos]= ',' || str.[pos] = ')' then
      (Empty, (pos))
    else if pos +1 < String.length str && str.[pos+1]='(' then 
      let l, pos_new = aux str (pos+2)
      in
      let r,pos_new = aux str (pos_new+1)
      in
      Node(str.[pos], l, r), (pos_new+1)
    else (Node(str.[pos], Empty, Empty), pos+1)
  in
  aux str 0
