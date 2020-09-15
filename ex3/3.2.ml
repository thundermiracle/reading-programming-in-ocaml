(* 3.2 *)
let new_and b1 b2 =
  if b1 then b2 else false
;;

new_and false true;;

let new_or b1 b2 =
  if b1 then true else b2
;;

new_or false true;;
