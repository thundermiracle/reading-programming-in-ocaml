(* 3.11.1 *)
(* Euclid 最大公約数、m ≦ n *)
let rec gcd (m, n) =
  if m = n then m
  else 
    if n -m < m then gcd(n - m, m)
    else gcd(m, n - m);;

gcd(12, 360);;


(* 3.11.2 *)
(* 組み合わせ数、0 ≦ m ≦ n *)
let rec comb (n, m) =
  if m = n || m = 0 then 1
  else comb(n - 1, m) + comb(n - 1, m - 1);;

comb(5, 3);;


(* 3.11.3 *)
(* 末尾再帰的フィボナッチ *)
let fib n =
  let rec iterfib (fn_1, fn_2, cur) =
    if (cur >= n) then fn_1 + fn_2
    else iterfib(fn_1 + fn_2, fn_1, cur + 1)
  in
  iterfib(1, 1, 3)
;;

fib 10;;

(* 3.11.4 *)
(* 文字列のなかでASCIIコードが最も大きい文字を返す *)
let max_ascii s =
  let lastInd = String.length s - 1 in
  let rec itermax_ascii (cur, currentMax) =
    if cur < 0 then currentMax
    else 
      let maxChar = if int_of_char s.[cur] > int_of_char currentMax then s.[cur]
        else currentMax
      in
      itermax_ascii(cur - 1, maxChar)
  in
  itermax_ascii(lastInd, s.[lastInd])
;;

max_ascii "HelloWorld!";;
