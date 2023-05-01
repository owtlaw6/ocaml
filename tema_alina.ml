(*6*)
let f x=x;;
let g y=y;;
let s x y= f x + f y;;
s 1 2;;
let s2 f x  g y=if x>y then (f x + g y) else (f x * g y) ;;
s2 f 6 g 5;;
(*7*)
let suma n= if n>1. then ceil((n*.2.75 -. 0.05*.n*.2.75)*.100.)/.100.+.(5.-.float_of_int(int_of_float(ceil((n*.2.75 -. 0.05*.n*.2.75)*.100.)) mod 5))/.100. else n*.2.75;;
let rest n=if n>1. then float_of_int(5-int_of_float(ceil((n*.2.75 -. 0.05*.n*.2.75)*.100.)) mod 5)/.100. else 0.;;
suma 1.;;
rest 1.;;
suma 7.;;
rest 7.;;
suma 9.;;
rest 9.;;
suma 3.;;
rest 3.;;

