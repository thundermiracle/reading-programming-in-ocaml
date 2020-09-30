(* 5.2.1 *)
let rec downto1 = function
  1 -> [1]
  | n' -> n' :: downto1 (n' - 1);;

downto1 6;;


(* 5.2.2 *)
let rec roman defL = function
  0 -> ""
  | num -> match defL with
    [] -> ""
    | (threshold, alphabet) :: rest ->
      if threshold > num then roman rest num
      else alphabet ^ roman defL (num - threshold);;

roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")] 1984;;
roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984;;
