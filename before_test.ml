open Printf
(*let f x a b= 
  if x < b && x > a then printf "x apartine intervalului [a, b]"
  else printf "x nu e in interval";;
f 2 1 4;;

let f x m = 
  if m mod 2 == 0 && m == x then 2 * x
  else if m mod 2 == 0 then (x + m) / 8
  else if m mod 2 == 1 && x == m then 2 * x + 1 
  else (x + m + 1) / 8 ;;
f 2 2;;
f 2 6;;
f 3 3;;
f 3 5;;

let f a b = a + (int_of_float) b
let r = f 1 2.;;

let f a b = 
  let rec ff a b i rez =
    if i == b then rez
    else ff a b (i + 1) (rez + a);
in ff a b 0 0;;
f 4 15;;

(*catul lui d impartit la i adica de cate ori se scade i din d*)
let f d i = d / i;;
f 5 2;;
let f d i = 
  let rec ff d i rez = 
    if d < i then (rez * i)
    else ff (d - i) i (rez + 1)
  in ff d i 0;;
f 5 2;;

let mediearitm a b = (a +. b) /. 2.
let r = mediearitm 2. 6.;;*)

(*interval x/2 2x*)
let f x = 
  let rec ff a b = 
    if a > b then printf " "
    else ( printf "%d " a;
    ff (a + 1) b )
  in ff (x / 2) (2 * x);;
f 4;;

let a = 9;;

let rec comp n f x = 
  if n == 0 then x
  else f(comp (n - 1) f x);;

