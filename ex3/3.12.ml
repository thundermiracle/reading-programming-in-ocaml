(* 3.12 *)
(* arctan1の展開, 1/(4n+1)-1/(4n+3) -> n from 0 *)
let pos n =
  let rec iterpos (cur, res) =
    if (cur > n) then res
    else 
      let increment = 1. /. (4. *. float_of_int(cur) +. 1.) -. 1. /. (4. *. float_of_int(cur) +. 3.) in
      iterpos(cur + 1, res +. increment)
  in iterpos(0, 0.)
;;

let pi = 4. *. pos 80000;;
