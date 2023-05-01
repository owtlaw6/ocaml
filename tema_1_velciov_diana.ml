
(* PROBLEMA 1 *)

let a = 5
let b = 9
let c = 2

let minimul = if a < b && a < c then a 
  else if b < a && b < c then b
    else c

let maximul = if a > b && a > c then a 
  else if b > a && b > c then b
    else c

let minim a b c = min a (min b c)

let maxim a b c = max a (max b c)

let x = minim 10 3 5
let y = maxim 3 10 8

let z = minim 0.2 5.3 8.6
let w = minim "abc" "bca" "cab"

;;

(* PROBLEMA 3 *)

let verificare an = if an mod 400 == 0 then true
  else if an mod 4 == 0 && an mod 100 != 0 then true
    else false

let x = verificare 1301

;;

(* PROBLEMA 7 *)

open Printf

let f n = 
  if n = 1. then (printf "restul este 0\n"; 2.75)
  else if n = 2. then (printf "restul este 0\n"; 5.5)
    else (
      printf "restul este: %.2f\n" (0.05 *. ceil (52.38 *. n) -. 0.05 *. 52.38 *. n);
      ceil (52.38 *. n) *. 0.05 
    )

(* let suma = f 5. *) (*rezultatul are mai mult de 2 zecimale*)
let suma = ( float_of_int (int_of_float (f 4. *. 100.) ) /. 100.)

;;