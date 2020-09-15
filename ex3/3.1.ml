let rate = 114.32;;

(* 3.1-1 ******************************)
(* round for x.x -> x *)
let frndJ f = floor (f +. 0.5);;

let exchangeD2Y dollar = 
  int_of_float (frndJ (dollar *. rate));;

exchangeD2Y 5.0;;


(* 3.1-2 ******************************)
(* round for x.xxx -> x.xx *)
let frndA f = floor (f *. 100. +. 0.5) /. 100.;;
let exchangeY2D yen =
  frndA(float_of_int(yen) /. rate);;

exchangeY2D 210;;


(* 3.1-3 ******************************)
open Printf
let printExchangeD2Y dollar = 
  let yen = exchangeD2Y(dollar) in
  printf "%.2f dollars are %d yen.\n" dollar yen;;

printExchangeD2Y 100.5;;


(* 3.1-4 ******************************)
let capitalize c =
  if 'a' <= c && 'z' >= c
  then char_of_int(int_of_char(c) - 32)
  else c;;

capitalize 'a';;
capitalize '1';;
