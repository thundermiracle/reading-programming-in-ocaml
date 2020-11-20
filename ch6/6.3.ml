let fact n = 
  let rec fact' n = if n = 0 then 1 else n * fact' (n-1) in
  if n < 0 then None else Some (fact' n);;

fact 3;;
fact (-10);;
