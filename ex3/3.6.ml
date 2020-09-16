(* 3.6.1 *)
let geo_mean (x, y) =
  sqrt (float_of_int (x * y));;
geo_mean (3, 5);;

(* 3.6.2 *)
let bmi (name, height, weight) =
  let bmiIndex = height /. weight ** 2. in
  let bmiMsg (status) = 
    name ^ "さんは" ^ status ^ "です" in
  if bmiIndex < 18.5 then bmiMsg("やせ")
  else if bmiIndex >= 18.5 && bmiIndex < 25. then bmiMsg("標準")
  else if bmiIndex >= 25. && bmiIndex < 30. then bmiMsg("肥満")
  else bmiMsg("高度肥満")
;;
bmi("プー", 40., 0.9);;

(* 3.6.3 *)
let sum_and_diff (x, y) = (x+y, x-y);;
let reverse_sum_and_diff (sum, diff) =
  ((sum + diff) / 2, (sum - diff) / 2);;

reverse_sum_and_diff(sum_and_diff(3, 6));;
