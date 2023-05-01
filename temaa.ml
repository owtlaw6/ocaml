(* PROBLEMA 5 
Scrieți o funcție care ia o mulțime de mulțimi 
(de exemplu, de șiruri), și returnează reuniunea 
(variantă: intersectia) mulțimilor. *)

(*module Is = Set.Make(Int)
module Intset = Set.Make(Is)

let reuni mult = Intset.fold (fun e p -> Is.add 
  (Is.fold (fun elem rez -> Is.add elem rez) e Is.empty) p )
  mult Intset.empty
let r = reuni (Intset.of_list [ [1;2;3]; [4;5]; [1;3;5]; [4;5;6]; [1;2;3;4;5] ]) |> Is.elements;;
*)

(* PROBLEMA 6
Scrieți o funcție care returnează mulțimea cifrelor unui număr. 
Scrieți apoi altă funcție care ia o mulțime de numere și returnează 
reuniunea/intersecția dintre mulțimile cifrelor lor. *)

module Is = Set.Make(Int)

let empty = Is.empty;;
let cifre n = 
  let rec f n mult = if n > 0 then f (n / 10) (Is.add (n mod 10) mult)
    else mult
  in f n empty
let r = cifre 1231 |> Is.elements;;

let unite mult = Is.fold (fun e p -> Is.add (cifre e) p) mult Is.empty;;
let r1 = unite (Is.of_list [231; 456; 11]) |> Is.elements;;