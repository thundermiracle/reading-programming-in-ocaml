(* 5.3.1 *)
let rec mem a s =
  match s with
  [] -> false
  | x::s' when x = a -> true
  | x::s' -> mem a s';;

mem 3 [2;4;3;5];;
mem (-3) [2;4;3;5];;


(* 5.3.2 *)
let rec intersect s1 s2 =
  match s1 with
  [] -> []
  | x::s1' when mem x s2 -> x::intersect s1' s2
  | x::s1' -> intersect s1' s2;;

intersect [2;3;4] [7;3;8;2;0];;


(* 5.3.3 *)
let rec union s1 s2 =
  match s1 with
  [] -> s2
  | x::s' when mem x s2 -> union s' s2
  | x::s' -> x::union s' s2;;

union [2;3;4] [7;3;8;2;0];;


(* 5.3.4 *)
let rec diff s1 s2 =
  match s1 with
  [] -> []
  | x::s' when mem x s2 -> diff s' s2
  | x::s' -> x::diff s' s2;;

diff [2;3;4] [7;3;8;2;0];;
