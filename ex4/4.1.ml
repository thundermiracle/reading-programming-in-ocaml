(* 4.1 *)
let curry f x y = f (x, y);;

let average (x, y) = (x +. y) /. 2.;;
let curried_average = curry average;;
(* curried_average 3. 4.;; *)

let uncurry f (x, y) = f x y;;

let uncurried_average = uncurry curried_average;;

uncurried_average (3., 4.);;
