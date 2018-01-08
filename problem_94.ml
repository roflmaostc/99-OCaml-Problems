

exception Solution_Found
exception No_Solution
(*Constraint
 * combis is list of arithmetic operations
 * 0 is =
 * 1 is +
 * 2 is -
 * 3 is * 
 * 4 is /
 * *)
let arithmetic_puzzle l =
  let n = List.length l
  in
  let next_combination combis =
    List.fold_right (fun ele (b, acc) ->
        if ele = 0 then
          (b, ele::acc)
        else
          if b = true then
            if ele = 4 then
              (true, 1::acc)
            else
              (false, ele+1::acc)
          else
            (false, ele::acc)
      )
      combis (true, [])
  in
  let next_equal_pos combis = 
    List.fold_right (fun ele (b, acc) -> 
        if b=true then 
          (false, 0::acc) 
        else if ele=0 then
          (true, 1::acc)
        else 
          (false, ele::acc)
    ) combis (false , [])
  in
  let operator x = if x = 1 then (+)
    else if x = 2 then (-)
    else if x = 3 then ( * )
    else if x = 4 then (fun a b -> if a mod b != 0 then raise Division_by_zero else a/b )
    else failwith "no operator to this number "
  in
  let rec eval c l = match (c,l) with
    |([], [x]) -> x
    | (hdc::tlc, hdl::tll) -> (operator hdc) (eval tlc tll) hdl
    | _ -> failwith "invalid input"
  in
  let split c l = 
    let rec aux c l b = 
      match (c,l) with 
      | ([], [x]) -> ([],[],[], [x])
      | (hdc::tlc, hdl::tll) -> 
          let acc_f_c, acc_f_l, acc_s_c, acc_s_l = aux tlc tll (if hdc= 0 then true else b)
          in
          if hdc=0
          then acc_f_c, hdl::acc_f_l, acc_s_c, acc_s_l
          else
            if b then acc_f_c, acc_f_l, hdc::acc_s_c, hdl::acc_s_l 
            else hdc::acc_f_c, hdl::acc_f_l, acc_s_c, acc_s_l
      | _ -> failwith "Error in split"
    in
    aux c l false
  in
  let eval_equation c l = 
    let firstc, firstl, secondc, secondl = split c l
    in
    try 
    (if (eval (List.rev firstc) (List.rev firstl)) = (eval (List.rev secondc) (List.rev secondl)) then true
    else false) with Division_by_zero -> false
  in
  let rec solve c = 
    if eval_equation c l = false then
      let b,c = next_combination c 
      in
      if b then 
        let b2, c = next_equal_pos c
        in
        if b2=false then raise No_Solution
        else
            solve c
      else solve c
    else c 
  in
  let operator_string x = if x=0 then "="
    else if x=1 then "+"
    else if x=2 then "-"
    else if x=3 then "*"
    else if x=4 then "/"
    else failwith "failure in operator string"
  in
  let rec to_string c l acc b = match (c,l) with
    | ([], [x]) -> acc^(string_of_int x)
    | (hdc::tlc, hdl::tll) -> 
      if hdc = 0 then
        acc^(string_of_int hdl)^"="^(to_string tlc tll  "" false)
      else
      to_string tlc tll  ((if b then "(" else "")^acc^(string_of_int hdl)^(if b then ")" else "")^operator_string hdc) true
    | _ -> failwith "Failure in to_string"
  in
  let str = to_string (solve  (List.rev_append (List.rev_map (fun _ -> 1 ) (List.tl (List.tl l))) [0])) l "" false
  in
  str




