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


(* 5.2.3 *)
let rec nested_length = function
  [] -> 0
  | [] :: l' -> nested_length l'
  | (x::innerl') :: l' -> 1 + nested_length (innerl' :: l');;

  nested_length [[1; 2; 3]; [4; 5]; [6]; [7; 8; 9; 10]];;


(* 5.2.4 *)
let rec concat = function
  [] -> []
  | [] :: l' -> concat l'
  | (x::innerl') :: l' -> x :: concat (innerl' :: l');;

concat [[0;3;4];[2];[];[5;0]];;


(* 5.2.5 *)
let rec zip l1 l2 =
  match (l1, l2) with
  (([], _) | (_, []))  -> []
  | (x::l1', y::l2') -> (x, y) :: zip l1' l2';;

zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11] [true; true; false; true; false; true; false; false; false; true];;


(* 5.2.6 *)
let rec unzip = function
 [] -> ([], [])
 | (x, y) :: l' -> let (l1, l2) = unzip l' in (x::l1, y::l2);;

unzip (zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11] [true; true; false; true; false; true; false; false; false; true]);;
