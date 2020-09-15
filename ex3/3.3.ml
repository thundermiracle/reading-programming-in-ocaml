(* 3.3 *)
let new_double_and b1 b2 =
  not (not b1 || not b2);;

new_double_and true false;;

let new_double_or b1 b2 =
  not (not b1 && not b2);;

new_double_or true false;;
