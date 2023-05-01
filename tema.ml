(* 4. Pentru tipurile colecție (liste, mulțimi, dicționare) e util să avem funcții care ne spun dacă 
există un element care satisface o anume condiție, respectiv dacă toate elementele satisfac condiția.
Implementați funcțiile exists și for_all pentru dicționare, folosind fold. Ele iau ca parametru o funcție 
booleană de cheie și valoare (care exprimă condiția) și dicționarul în care se face căutarea. (Ele există 
ca funcții standard, deci puteți vedea tipul lor în documentație).
Încercați să scrieți prelucrarea folosind o excepție pentru a întrerupe parcurgerea dacă răspunsul nu mai 
depinde de restul elementelor (true pentru exists, resp. false pentru for_all). În acest caz puteți folosi 
mai simplu iter. Urmați exemplul cu liste de la curs și din notițe (sec. 5.1). *)

module Int = struct
  type t = int
  let compare = compare
end
module Intmap = Map.Make(Int)

let mymap = Intmap.empty
  |> Intmap.add 1 1
  |> Intmap.add 2 2
  |> Intmap.add 3 3
  |> Intmap.add 4 4
  |> Intmap.add 5 5
  |> Intmap.add 6 6

let exists funct dict = Intmap.fold (fun key value rez -> 
  try Intmap.iter (fun k v -> if funct v then raise Exit) dict; false with Exit -> true) dict false;;

let r = exists (fun h -> h > 6) mymap;;
let r = exists (fun h -> h > 2) mymap;;

let for_all funct dict = Intmap.fold (fun k v r ->
  try Intmap.iter (fun k v -> if not(funct v) then raise Exit) dict; true with Exit -> false) dict true;;

let r = for_all (fun h -> h mod 2 = 0) mymap;;
let r = for_all (fun h -> h mod 13 <> 0) mymap;;


(* 5. Implementați cu ajutorul lui fold funcția standard map care construiește un dicționar în care toate 
valorile au fost transformate folosind o funcție dată ca parametru. *)

let mymap = Intmap.empty
  |> Intmap.add 1 1
  |> Intmap.add 2 2
  |> Intmap.add 3 3
  |> Intmap.add 4 4
  |> Intmap.add 5 5
  |> Intmap.add 6 6

let dosth funct dict = Intmap.fold (fun key value rez -> Intmap.add key (funct value) rez) dict Intmap.empty;;
let r = dosth (fun h -> h * 2) mymap |> Intmap.bindings;;


(* 9. Scrieți o funcție care ia ca parametru un dicționar reprezentând o funcție parțială f de la șiruri la 
șiruri și calculează pentru un șir s numărul maxim n pentru care fn(s) e definit, respectiv generează o 
excepție dacă șirul fn(s) e ciclic (definit pentru orice n).
De exemplu, pentru f("a") = "b", f("b") = "c", f("d") = "e", f("e") = "f", f("f") = "e", avem depth("x") = 0, 
depth("a") = 2, depth("b") = 1, iar depth("d") generează excepție (la fel pentru "e" și "f").
Indicație: La parcurgerea pornind de la s rețineți mulțimea tuturor șirurilor deja întâlnite pentru a detecta 
un eventual ciclu. *)

(*module Stringmap = Map.Make(String)

let mystr = Stringmap.empty
  |> Stringmap.add "a" "b"
  |> Stringmap.add "b" "c"
  |> Stringmap.add "d" "e"
  |> Stringmap.add "e" "f"
  |> Stringmap.add "f" "e";;

let depth maps letter = 
  let rec cautari maps letter ak suma = 
    let aux = Stringmap.find ak maps in
    try aux with
    | Not_found -> Printf.printf "%d" suma; "0"
    | _ -> cautari maps letter aux (suma + 1)
  in cautari maps letter letter 0;;

let r = depth mystr "a";;
*)