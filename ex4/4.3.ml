(* 4.3 *)
let ($) f g x = f (g x);;
let id x = x;;

let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f;;

let add3 x = x + 3;;
let add3_5times = funny add3 5;;

add3_5times 3;;

(* 
funnyは関数f n回を実行させる新しい関数を生成する高階関数（関数fは単一引数）；
しかも、普通のfをn回実行させる再帰関数より、実行回数がn -> log2nとなり、効率が向上（問題3.7.2を参考へ）；
funny f 3 -> f(f(f)) となる。

n = 0の場合、identifyを返すため、値に影響しない
n　は偶数の場合、関数fを2回実行させる新しい関数をcomposeで生成、 -> f(f)
n　は奇数の場合、偶数回の新しい関数＋f (あまりの1回)
 *)
