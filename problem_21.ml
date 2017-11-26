let insert ele pos l = let liste, _ = List.fold_left (fun (lacc,k) x -> (if k!=pos then (x::lacc,k+1) else (x::ele::lacc,k+1))) ([],0) l
    in List.rev liste;;
