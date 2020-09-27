(* 5.1.1 *)
[[]];;
(* 'a list list *)


(* 5.1.2 *)
[[1;3],["hoge"]];;
(* (int list * string list) list *)


(* 5.1.3 *)
[3] :: [];;
(* [[3]] int list list *)


(* 5.1.4 *)
2 :: [3] :: [];;
(* ERR throws by [3]: expected type of int, but got 'a list *)


(* 5.1.5 *)
[]::[]
(* [[]] 'a list list *)


(* 5.1.6 *)
[(fun x -> x);(fun b -> not b)];;
(* (bool -> bool) list *)
