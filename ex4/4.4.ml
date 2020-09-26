(* 4.4 *)
let s x y z = x z (y z);;
let k x y = x;;

s k k 1;;
(* 
  s k k 1
  -> k 1 (k 1)
  -> k 1 省略
  -> 1

  なので：
  s k k z
  -> k z (k z)
  -> k z 省略
  -> z
  -> 恒等関数
 *)
