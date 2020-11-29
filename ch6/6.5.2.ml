(* prime *)
let is_prime x = 
  let rec is_dividable_from_2 n =
    n > 1 && (x mod n = 0 || is_dividable_from_2 (n-1))
  in
  not (is_dividable_from_2 (x-1));;

is_prime 3;;
is_prime 47;;
is_prime 78;;

let rec prime_from x n = 
  if is_prime (x+1) then
    if n = 1 then x+1 else prime_from (x+1) (n-1)
  else
    prime_from (x+1) n;;

prime_from 3 2;;
(* 7 *)


(* method from book *)
type intseq = Cons of int * (int -> intseq);;
let rec nthseq t (Cons(x, f)) =
  if t = 1 then x
  else nthseq (t-1) (f x);;

let rec prime_seq x =
  if is_prime (x+1) then Cons(x+1, prime_seq)
  else prime_seq (x+1);;

nthseq 2 (prime_seq 3);;
(* 7 *)
