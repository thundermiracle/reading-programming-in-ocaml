(* 3.4.1 *)
let x = 1 in let x = 3 in let x = x + 2 in x * x;;
(* last x win, so x is 5 -> 25 *)

(* 3.4.2 *)
let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y;;
(* and is different with in which will not apply the newest value before it.
   So it means (let y = 2 and x = 3 + 2 in 5 * 2) + 3 -> 13 *)

(* 3.4.3 *)
let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z;;
(* in will apply all changes before it,
   So it means let y=2 in let z=2+2 in 2*2*4 -> 16 *)
