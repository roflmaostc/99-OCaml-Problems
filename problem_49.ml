let gray n = 
  let rec aux k l = 
    if k<n then aux (k+1) (List.rev_append (List.rev_map (fun x -> "0"^x) l)(List.rev (List.rev_map (fun x -> "1"^x) (List.rev l))))
    else l
  in
  aux 1 ["0"; "1"];;

let l = gray 5;;
let () = List.iter (Printf.printf " %s " ) l;;

let gray n = 
  let rec gray_next_level k l = 
    if k<n then
      (*first_half is normally the list with each element appended a 0. But since List.rev_append turns only first list around, everything is in right order.*)
      let first_half = List.rev_map (fun x -> "0"^x) l
      in
      (*This can be in reverse order, since Gray code appends mirrored list *)
      let second_half = List.rev_map (fun x -> "1"^x) l 
      in
      gray_next_level (k+1) (List.rev_append first_half second_half) 
    else l
  in
  gray_next_level 1 ["0"; "1"];;

let () = Printf.printf "\n";;
let l = gray 5;;
let () = List.iter (Printf.printf " %s " ) l;;


(*Tail recursive version of gray. List.fold_left and List.rev_append are both tail recursive.
 * Since we traverse the input list on every level only once, it is also very fast.*)
let gray n = 
  let rec gray_next_level k l = 
    if k<n then
      (* This is the core part of the Gray code construction. 
       * first_half is reversed and has a "0" attached to every element.
       * Second part is reversed (it must be reversed for correct gray code). 
       * Every element has "1" attached to the front.*)
      let (first_half,second_half) = 
        List.fold_left (fun (acc1,acc2) x -> 
            (("0"^x)::acc1, ("1"^x)::acc2 )) ([],[]) l
      in
      (* List.rev_append turns first_half around and attaches it to second_half. 
       * The result is the modified first_half in correct order attached to 
       * the second_half modified in reversed order.*)
      gray_next_level (k+1) (List.rev_append first_half second_half)
    else l
  in
  gray_next_level 1 ["0"; "1"];;


let () = Printf.printf "\n";;
let l = gray 5;;
let () = List.iter (Printf.printf " %s " ) l;;
