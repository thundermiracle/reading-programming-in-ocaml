(* 4.5 *)
let twice f x = f(f x);;

(* 
  twice twice f x
  -> twice (twice f) x
  -> (twice f) ((twice f) x)
  -> (twice f) (twice f x)
  -> (twice f) f(f x)
  -> twice f f(f x)
  -> f(f(f(f x)))
*)
