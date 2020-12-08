(* 6.14 *)
let t = Sys.time();;


type intseq = Cons of int * (int -> intseq);;
let rec nthseq t (Cons(x, f)) =
  if t = 1 then x
  else nthseq (t-1) (f x);;

let rec prime_seq is_prime x =
  if is_prime (x+1) then Cons(x+1, prime_seq is_prime)
  else prime_seq is_prime (x+1);;

(***************** Original Version *****************)
let is_prime_original x = 
  let rec is_dividable_from_2 n =
    n > 1 && (x mod n = 0 || is_dividable_from_2 (n-1))
  in
  not (is_dividable_from_2 (x-1));;
(* nthseq 3000 (prime_seq is_prime_original 3);; *)
(* 70.62sec *)


(***************** Divide from 2 to x-1 Version *****************)
let is_prime_small_to_big x =
  let rec is_dividable_from_2 n =
    n < x && (x mod n = 0 || is_dividable_from_2 (n+1))
  in
  not (is_dividable_from_2 2);;
(* nthseq 3000 (prime_seq is_prime_small_to_big 3);; *)
(* 10.72sec *)


(***************** Divide from 2 to sqrt x Version *****************)
let is_prime_small_to_sqrt x =
  let xTo = int_of_float (floor (sqrt (float_of_int x))) in
  let rec is_dividable_from_2 n =
    n < xTo && (x mod n = 0 || is_dividable_from_2 (n+1))
  in
  not (is_dividable_from_2 2);;
(* nthseq 3000 (prime_seq is_prime_small_to_sqrt 3);; *)
(* 0.12sec *)

Printf.printf "Execution time: %fs\n" (Sys.time() -. t);;
