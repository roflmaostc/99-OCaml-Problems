let remove_at k l = let liste, _ =List.fold_left (fun (l, c) ele->  if k=c then (l,c+1) else (ele::l,c+1)) ([],1) l
    in List.rev liste;;
