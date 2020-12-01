(* 6.3.1 - multiple *)
type nat = Zero | OneMoreThan of nat;;
let rec add m n =
  match m with
  Zero -> n
  | OneMoreThan m' -> OneMoreThan (add m' n);;

let rec mul m n = 
  match m with
  Zero -> Zero
  | OneMoreThan m' -> add (mul m' n) n;;


(* Test *)
let one = OneMoreThan Zero;;
let two = OneMoreThan one;;

mul Zero two;;
(* Zero *)

mul one two;;
(* OneMoreThan (OneMoreThan Zero) *)

mul two two;;
(* OneMoreThan (OneMoreThan (OneMoreThan (OneMoreThan Zero))) *)
