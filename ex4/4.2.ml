(* 4.2 *)
let rec repeat f n x =
  if n > 0 then repeat f (n-1) (f x)
  else x;;


let fib n =
  let (fibn, _) =
    (* anonymous function returns (Xn + Xn-1, Xn) *)
    (* count from n-2 becouse first 2 elements are already counted *)
    repeat (fun (xn, xn_1) -> (xn + xn_1, xn)) (n-2) (1, 1)
  in fibn;;

fib 5;;
