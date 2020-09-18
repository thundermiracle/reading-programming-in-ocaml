(* 3.14 *)
(* 定積分 a~b  f(x)dx *)
let integral f a b =
  let dx = 0.1e-3 in
  let rec loop x res =
    let xTo = x +. dx in
    if xTo > b then res
    else (f x +. f (xTo)) *. dx /. 2. +. (loop xTo res)
  in
  loop a 0.
;;

(* 0~π, f: sinx *)
integral sin 0. pi;;
(* ≈ 2 *)
