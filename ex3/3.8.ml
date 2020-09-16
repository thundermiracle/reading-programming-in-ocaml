(* 3.8 *)
let rec iterpow (x, n, res) =
  if n = 0 then res
  else iterpow (x, n - 1, res * x);;

#trace iterpow;;
iterpow(2, 8, 1);;
#untrace iterpow;;
