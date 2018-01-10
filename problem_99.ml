
type orientation = Vertical | Horizontal
exception No_Solution
exception Solution 

(*Considering only empty crosswords*)
let solve_crossword file =
  let explode s = 
    let rec aux i b =
      if i=b then [] else s.[i]::(aux (i+1) b)
    in
    aux 0 (String.length s)
  in
  (*reads file and returns words and field*)
  let read_file file = 
    let inc = open_in file
    in
    let rec aux words crossword b = 
      try 
        let l = input_line inc
        in
        if l = "" then aux words crossword false
        else if b then aux (l::words) crossword b
        else aux words (l::crossword) b
      with End_of_file -> (words, crossword)
    in
    let words, crossword = aux [] [] true
    in
    (words, List.rev crossword)
  in
  (*convert list filed to array*)
  let field_to_array field =
    let n = List.fold_left (fun acc x -> if String.length x>acc then String.length x else acc) 0 field
    in
    let arr = Array.make_matrix (List.length field) n '0'
    in 
    let _ = List.fold_left (
      fun y line -> let _ = List.fold_left 
          (fun c ch -> arr.(y).(c) <- if ch = '.' then ' ' else '0' ; c+1) 0 (explode line)  in (y+1) 
    ) 0 field
    in
    arr
  in
  let words, field = read_file file 
  in
  let words = List.sort (fun a b -> compare (String.length b) (String.length a)) words
  in
  let field = field_to_array field
  in
  let rec search_horizontal_slots field (x:int) (y:int) acc (counter:int) (n:int) = 
    if y = Array.length field  then acc 
    else if x = n then (if counter>1 then search_horizontal_slots field 0 (y+1) ((x-counter,y,counter, Horizontal)::acc) 0 n
                   else search_horizontal_slots field 0 (y+1) acc 0 n)
    else if field.(y).(x) = '0' then 
      ( if counter>1 then search_horizontal_slots field (x+1) y ((x-counter,y,counter, Horizontal)::acc) 0 n
        else search_horizontal_slots field (x+1) y acc 0 n
      ) 
    else search_horizontal_slots field (x+1) y acc (counter+1) n
  in
  let rec search_vertical_slots field (x:int) (y:int) acc (counter:int) (n:int) = 
    if x = Array.length field.(0)  then acc 
    else if y = n then (if counter>1 then search_vertical_slots field (x+1) y ((x,y-counter,counter, Vertical)::acc) 0 n
                   else search_vertical_slots field (x+1) 0 acc 0 n)
    else if field.(y).(x) = '0' then 
      ( if counter>1 then search_vertical_slots field x (y+1) ((x,y-counter,counter, Vertical)::acc) 0 n
        else search_vertical_slots field x (y+1) acc 0 n
      ) 
    else search_vertical_slots field x (y+1) acc (counter+1) n
  in
  let vslots = search_vertical_slots field 0 0 [] 0 (Array.length field) 
  in
  let hslots = search_horizontal_slots field 0 0 [] 0 (Array.length field.(0))
  in
  let slots = List.rev_append vslots hslots
  in
  let slots = List.sort (fun (x1,y1,_,_) (x2,y2,_,_) -> compare (x1*x1+y1*y1) (x2*x2+y2*y2)) slots
  in
  let check_insert_word field (x,y,l,dir) word = 
    if String.length word != l then ([],false)
    else 
      let rec aux c b acc =
           if c=b then (acc,true) 
           else if (if dir=Horizontal then field.(y).(x+c)=word.[c] else field.(y+c).(x)=word.[c]) then
             aux (c+1) b acc 
           else if (if dir=Horizontal then field.(y).(x+c) = ' ' else field.(y+c).(x)=' ' ) then
             aux (c+1) b ((if dir=Horizontal then ((field.(y).(x+c) <- word.[c]); (x+c,y)) else ((field.(y+c).(x) <- word.[c]);(x,y+c)) )::acc) 
           else (acc,false)
      in
      aux 0 (String.length word) []
  in
  (* check_insert_word field (2, 2, 5, Horizontal) "hallo" *) 
  let clear_field field hist = 
    List.fold_left (fun field (x,y) -> (field.(y).(x) <- ' '); field) field hist
  in
  let rec solve field checked_w unchecked_w slots first = match (unchecked_w, slots) with
    | ([],[]) -> raise Solution
    | ([],_) -> if first then raise No_Solution else field 
    | (hw::tlw, hs::tls) -> 
      let hist, b = check_insert_word field hs hw  
      in
      if b then 
        let _ = solve field [] (List.rev_append checked_w tlw) tls false
        in
        let _ = clear_field field hist
        in
        solve field (hw::checked_w) tlw (hs::tls) first
      else 
        let _ = clear_field field hist
        in
        solve field (hw::checked_w) tlw (hs::tls) first
    | (_,[]) -> raise Solution
  in
  try solve field [] words slots true with Solution -> field;;



let print_board board = 
  for x = 0 to Array.length board.(0)-1 do
    (
    (for y = 0 to Array.length board-1 do
      Printf.printf "%s " (String.make 1 board.(y).(x))
    done);
    Printf.printf "\n")
  done


let () = print_board (solve_crossword "p7_09b.dat")













