let binomialCoeff n p =
  (*taken from https://rosettacode.org/wiki/Evaluate_binomial_coefficients#OCaml*)
  let n,p = float_of_int n, float_of_int p in
  let p = if p < n -. p then p else n -. p in
  let rec cm res num denum =
    (* this method partially prevents overflow.
     * float type is choosen to have increased domain on 32-bits computer,
     * however algorithm ensures an integral result as long as it is possible
     *)
    if denum <= p then cm ((res *. num) /. denum) (num -. 1.) (denum +. 1.)
    else res in
  int_of_float (cm 1. n 1.)

let next_combination board line = 
  (*this method constructs a new line combination in dependence of the old one*)
  try 
    (let rec aux counter length acc acti = 
    if counter = (Array.length (board.(line))) then if acti then  (counter-length, length)::acc
      else acc
    else if board.(line).(counter) = '_' then
      if acti then aux (counter+1) 0 ((counter-length,length)::acc) false
      else aux (counter+1) 0 acc false
    else aux (counter+1) (length+1) acc true
   in
   let content = aux 0 0 [] false 
   in
   let rec inc l prevx = match l with
     | [] -> []
     | (pos,len)::tl -> 
         if (pos+len)+1=prevx then 
           let posb, lenb = List.hd tl
           in
           if posb+lenb+3+len >=prevx then (pos,len)::inc tl pos
           else (posb+lenb+2,len)::inc tl (posb+lenb+3) 
         else (pos+1,len)::tl
   in
   let content = inc content (Array.length board.(line)+1)in
   let () = for i=0 to (Array.length board.(line)-1) do board.(line).(i) <- '_' done in
   let () = List.fold_left (fun _ (pos, len) -> for i=pos to (pos+len-1) do board.(line).(i) <- 'X' done ) () content in
   (board,true)) 
   with  Failure x -> (board,false)


let solve_nonogram nono =
  let n, m = List.length (List.hd nono), List.length (List.nth nono 1) in
  let consx, consy =  
    (List.fold_right (fun ele acc -> match acc with [] -> (n-1,ele)::acc | (a,_)::tl -> (a-1,ele)::acc ) (List.nth nono 0) [] ),
    (List.fold_right (fun ele acc -> match acc with [] -> (m-1,ele)::acc | (a,_)::tl -> (a-1,ele)::acc ) (List.nth nono 1) [] ) in
  (*we sort according to possible combinations. Rows with fewest combinations will be placed first*)
  let horiz_sortFunc (pos,ele) k = 
    let sum_blocks, blocks = List.fold_left (fun acc ele -> acc+ele) 0 ele, List.length ele in
    binomialCoeff (k-sum_blocks+1) (k-sum_blocks-blocks+1)
    (* binomialCoeff (blocks+1+(n-sum_blocks-blocks+1)-1) (n-sum_blocks-blocks+1) *) in
  let consx = List.sort (fun a b -> compare (horiz_sortFunc a m) (horiz_sortFunc b m)) consx
  in 
  let board = Array.make_matrix n m '_'
  in 
  let init_row cons board = 
    let i,eles = List.hd cons
    in
    let rec aux counter eles = match eles with
      | [] -> List.tl cons
      | h::tl -> (for j = 0 to (h-1) do board.(i).(j+counter) <- 'X' done); 
        aux (counter+h+1) tl
    in
    aux 0 eles
  in
  (* let consx = init_row consx board *)
  (* in *) 
  (* let consx = init_row consx board *)
  (* in *)
  let consx = init_row [(2,[1;2;1;1])] board
  in
  board
  (* consx, consy *)
  (* horiz_sortFunc (2, [6]) m *)
