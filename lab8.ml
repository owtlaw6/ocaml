module Smap = Map.Make(String)
module Imap = Map.Make(Int)
module Sset = Set.Make(String)

let fn = Smap.empty
  |> Smap.add "a" "b"
  |> Smap.add "b" "c"
  |> Smap.add "d" "a"
  |> Smap.add "c" "b"

let pb9 f x = 
  let rec aux x s seen =
    if Sset.mem x seen then failwith "ciclu"
    else
    try 
      aux (Smap.find x f) (s + 1) (Sset.add x seen)
    with Not_found -> s
  in aux x 0 Sset.empty;;

let s = pb9 fn "a";;
let s = pb9 fn "b";;
let s = pb9 fn "c";;
let s = pb9 fn "e";;

;;

let groupbymodule3 lst =  
  List.fold_left (fun r e -> 
    let aux = e mod 3 in 
    try Imap.add aux (e :: (Imap.find aux r)) r with 
    | Not_found -> Imap.add aux [e] r
    | Imap.add aux (e :: (Imap.find aux r)) r
    )
    Imap.empty
    lst;;

let r = groupbymodule3 [1;2;3;4;5;6;7;8;9;10] |> Imap.bindings;;

;;

let mymap = Imap.empty 
  |> Imap.add 1 2
  |> Imap.add 2 1
  |> Imap.add 3 4
  |> Imap.add 4 3 
  |> Imap.add 5 5

let invol maps = 
  Imap.fold (fun key value rez -> 
  try if (Imap.find value maps = key) then rez
    else false
  with Not_found -> false
  ) maps true

let r = invol mymap;;

let involforall maps = 
  Imap.for_all (fun key value -> 
  try 
    (Imap.find value maps) = key
  with Not_found -> false
  ) maps 

let r = involforall mymap;;

let involdecupat maps = try 
  Imap.fold (fun key value rez -> 
  if (Imap.find value maps = key) then rez
    else false
  ) maps true
  with Not_found -> false

let r = involdecupat mymap;;

let involdecupat2 maps = 
  Imap.fold (fun key value rez -> 
  try if (Imap.find value maps = key) then rez
    else raise Not_found
  with Not_found -> false
  ) maps true

let r = involdecupat2 mymap;;

;;