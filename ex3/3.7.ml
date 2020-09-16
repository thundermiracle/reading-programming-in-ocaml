(* 3.7.1 *)
let rec pow1 (x, n) =
  if (n = 0) then 1.
  else x *. pow1(x, n - 1);;

#trace pow1;;
pow1 (2., 8);;
#untrace pow1;;


(* 3.7.2 *)
let rec pow2 (x, n) =
  if (n = 0) then 1.
  else 
    let res = pow2(x, n / 2) in
    if n mod 2 = 0 then res ** 2. (* (x ** n/2) * (x ** n/2) *)
    else res ** 2. *. x (* (x ** n/2) * (x ** n/2) * x *)
;;

#trace pow2;;
pow2(2., 8);;
#untrace pow2;;
