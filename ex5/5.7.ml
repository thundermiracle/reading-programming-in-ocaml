(* 5.7 *)
let squares r = 
  let valid_r x y result = 
  if x * x + y * y = r then (x, y)::result else result
  in
  let rec calc_xy y result =
    let x = int_of_float (sqrt (float_of_int (r - y * y))) in
    if x < y then result
    else calc_xy (y + 1) (valid_r x y result)
  in
  calc_xy 0 [];;

squares 48612265;;
