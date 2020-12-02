(* 6.4 *)
type nat = Zero | OneMoreThan of nat;;
type 'a option = None | Some of 'a;;

let rec monus m n = 
  match (m, n) with
  (Zero, _) -> Zero
  | (r, Zero) -> r
  | (OneMoreThan m', OneMoreThan n') -> monus m' n';;

let minus m n =
  if m = Zero && n != Zero then None
  else Some (monus m n);;


(* Test *)
let one = OneMoreThan Zero;;
let two = OneMoreThan one;;

minus Zero two;;
(* None *)

minus two one;;
(* Some (OneMoreThan Zero) *)

minus two two;;
(* Some (Zero) *)
