(* 3.10 *)
let fib m =
  let rec fib_pair n =
    if n = 1 then (1, 0)
    else
      let (fn_1, fn_2) = fib_pair(n - 1) in
      (fn_1 + fn_2, fn_1)
  in
  let (res, _) = fib_pair m in
  res
;;

(* 
  -> fib_pair 4
  -> if 4 = 1 then (1, 0) else 
    let (fn_1,fn_2) = fib_pair(4 - 1) in (fn_1+fn_2,fn_1)
  -> if 3 = 1 then (1, 0) else
    let (fn_1,fn_2) = fib_pair(3 - 1) in (fn_1+fn_2,fn_1)
  -> if 2 = 1 then (1, 0) else
    let (fn_1,fn_2) = fib_pair(2 - 1) in (fn_1+fn_2,fn_1)
  -> if 1 = 1 then (1, 0) else
    let (fn_1,fn_2) = fib_pair(1 - 1) in (fn_1+fn_2,fn_1)

  -> let (1, 0) = fib_pair(2 -1) in (1+0, 1)
  -> let (1, 1) = fib_pair(3 -1) in (1+1, 1)
  -> let (2, 1) = fib_pair(4 -1) in (2+1, 2)

  -> let (3, _) = fib_pair 4

  3
*)
