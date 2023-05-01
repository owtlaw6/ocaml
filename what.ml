module Stringmap = Map.Make(String)

let mystring = Stringmap.empty |> Stringmap.add "a" "b" |> Stringmap.add "b" "c" |> Stringmap.add "d" "e" |> Stringmap.add "f" "e";;

let depth maps letter = 
  let rec cautari suma ak maps letter = 
    let aux = Stringmap.find ak maps in
    try aux with
    | Not_found -> suma
    | _ -> cautari (suma + 1) aux maps letter
  in cautari 0 letter maps letter;;

let r = Stringmap.bindings mystring;;
let r = depth mystring "a";;
let r = Stringmap.find "f" mystring;;

