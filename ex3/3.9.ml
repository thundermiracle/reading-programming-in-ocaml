(* 3.9 *)
let cond (b, e1, e2) : int =
  if b then e1 else e2;;
  
let rec fact n =
  cond ((n = 1), 1, n * fact (n-1));;
  
fact 4;;

(* Stack Overflow because there's no break here *)
(*
  const ((4=1), 1, 4 * fact(4-1)) 
  const ((3=1), 1, 3 * fact(3-1)) 
  const ((2=1), 1, 2 * fact(2-1)) 
  const ((1=1), 1, 4 * fact(1-1)) 
  const ((0=1), 1, 0 * fact(0-1)) 
  const ((-1=1), 1, -1 * fact(-1-1)) 
  ...
*)
