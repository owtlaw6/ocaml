let rec ap n = 
  if n == 0 then 3
  else 2 + ap (n - 1);;
ap 3;;

let rec pg x =
  if x == 0 then 1
  else 2 * pg (x - 1);;
#trace pg;;
pg 3;;

let ap baza ratia = 
  let rec ap1 n =
    if n == 0 then baza
    else ratia + ap1(n - 1)
in ap1;;
ap 1 2 3;;

let apb2r5 = ap 2 5;;
let apb3r7 = ap 3 7 ;;
apb2r5 6;;
apb3r7 8;;

let rec sumacifre n =
  if n == 0 then 0
  else n mod 10 + sumacifre (n / 10);;
sumacifre 54;;

let rec sumacifre1 n rez =
  if n == 0 then rez
  else sumacifre1 (n / 10) (rez + n mod 10);;
sumacifre1 54 0;;

let f n =
  let rec sumacifre12 n rez =
    if n == 0 then rez
    else sumacifre12 (n / 10) (rez + n mod 10)
in sumacifre12 n 0;;
f 54 ;;

let f x = x + 2
let rec comp n f x = 
  if n == 0 then x
  else f (comp (n - 1) f x);;
  (*comp (n - 1) f (f x)*)

(*tema ex 3 si 6 + 2*)