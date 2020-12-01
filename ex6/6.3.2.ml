(* 6.3.1 - monus *)
type nat = Zero | OneMoreThan of nat;;

let rec monus m n = 
  match (m, n) with
  (Zero, _) -> Zero
  | (r, Zero) -> r
  | (OneMoreThan m', OneMoreThan n') -> monus m' n';;

(* Test *)
let one = OneMoreThan Zero;;
let two = OneMoreThan one;;

monus Zero two;;
(* Zero *)

monus two one;;
(* OneMoreThan Zero *)

monus two two;;
(* Zero *)
