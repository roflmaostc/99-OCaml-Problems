let gray n = 
  let rec aux k l = 
    if k<n then aux (k+1) (List.rev_append (List.rev_map (fun x -> "0"^x) l)(List.rev (List.rev_map (fun x -> "1"^x) (List.rev l))))
    else l
  in
  aux 1 ["0"; "1"];;

let l = gray 15;;
let () = List.iter (Printf.printf " %s " ) l;;
