(* 4.6 *)
let s x y z = x z (y z);;
let k x y = x;;
let i = s k k;;

(* fun x y -> y *)
let funny = k i;;
(* 
  k i x y
  -> (k i x) y
  -> i y
  -> y
*)

funny 1 3;;
