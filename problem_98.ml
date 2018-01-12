let binomialCoeff n p =
  (*taken from https://rosettacode.org/wiki/Evaluate_binomial_coefficients#OCaml*)
  let n = float_of_int n
  in
  let p = float_of_int p
  in
  let p = if p < n -. p then p else n -. p in
  let rec cm res num denum =
    (* this method partially prevents overflow.
     * float type is choosen to have increased domain on 32-bits computer,
     * however algorithm ensures an integral result as long as it is possible
     *)
    if denum <= p then cm ((res *. num) /. denum) (num -. 1.) (denum +. 1.)
    else res in
  int_of_float (cm 1. n 1.)
 


let solve_nonogram nono =
  let n = List.length (List.hd nono)
  in
  let m = List.length (List.nth nono 1)
  in
  let consx, consy =  
    (List.fold_right (fun ele acc -> match acc with [] -> (n-1,ele)::acc | (a,_)::tl -> (a-1,ele)::acc ) (List.nth nono 0) [] ),
    (List.fold_right (fun ele acc -> match acc with [] -> (m-1,ele)::acc | (a,_)::tl -> (a-1,ele)::acc ) (List.nth nono 1) [] )
  in
  (*we sort according to possible combinations. Rows with fewest combinations will be placed first*)
  let horiz_sortFunc (pos,ele) k = 
    let sum_blocks = List.fold_left (fun acc ele -> acc+ele) 0 ele
    in
    let blocks = List.length ele
    in
    binomialCoeff (k-sum_blocks+1) (k-sum_blocks-blocks+1)
    (* binomialCoeff (blocks+1+(n-sum_blocks-blocks+1)-1) (n-sum_blocks-blocks+1) *)
  in
  let consx = List.sort (fun a b -> compare (horiz_sortFunc a m) (horiz_sortFunc b m)) consx
  in consx
  (* consx, consy *)
  (* horiz_sortFunc (2, [6]) m *)
