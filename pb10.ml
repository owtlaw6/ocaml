(*10. Fie un dicționar de la șiruri la șiruri reprezentând o funcție parțială f.
a) Scrieți o funcție (având ca parametru un astfel de dicționar) care returnează dicționarul 
reprezentând f2 = f o f, și apoi o funcție care calculează dicționarul pentru fn.
b) Modificați funcția astfel încât să genereze o excepție dacă fn are un punct fix (există x cu fn(x) = x)
c) Aplicați repetat funcția de mai sus pentru a determina dacă funcția f are un ciclu 
(se va genera fie o excepție, fie se ajunge la relația vidă).*)

module Smap = Map.Make(String)
module Sset = Set.Make(String)

let fn = Smap.empty
  |> Smap.add "a" "b"
  |> Smap.add "b" "c"
  |> Smap.add "d" "a"
  |> Smap.add "c" "e"
  |> Smap.add "x" "y"
  |> Smap.add "y" "x"
  |> Smap.add "m" "n"
  |> Smap.add "n" "p"
  |> Smap.add "p" "m"

let pb10 f x = 
  let rec aux nou = 
   if Smap.find nou f = x then failwith "exista un punct fix"
    else
    try 
      aux (Smap.find nou f)
    with Not_found -> "nu exista punct fix"
  in aux x;;

let s = pb10 fn "a";;
let s = pb10 fn "b";;
let s = pb10 fn "x";;
let s = pb10 fn "m";;

;;
