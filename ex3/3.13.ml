(* 3.13.1 *)
(* curry pow1 in 3.7.ml *)
let rec pow n x =
  if n = 0 then 1.
  else x *. pow (n-1) x;;
let cube = pow 3;;

cube 5.;;


(* 3.13.2 *)
let rec reverse_pow x n = pow n x;;
let cube2 x = reverse_pow x 3;;

cube2 5.;;
  