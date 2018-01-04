let full_words number =
  if number=0 
  then "zero"
  else
    let al = [(1, "one"); (2, "two"); (3,"three"); (4,"four");(5,"five");(6,"six");(7,"seven");(8,"eight");(9,"ten");(0,"zero")]
    in
    let rec aux number =
      if number = 0 then 
        ""
      else
        (aux (number/10) )^"-"^(List.assoc (number mod 10) al)
    in
    let str = aux number
    in
    String.sub str 1 ((String.length str)-1) 
