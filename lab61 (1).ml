(*module StringSet :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t*)

module StringSet = Set.Make (String)
module Int = struct
  type t = int
  (*let compare x y = 
    if x == y then 0
    else if x > y then 1 else -1*)
  let compare = compare
end 
module Intset = Set.Make(Int)

let empty = StringSet.empty;;

let s1 = StringSet.add "1" StringSet.empty;;
let rs1 = StringSet.elements s1;;

let s1 = StringSet.add "2" s1;;
let s1 = StringSet.add "3" s1;;

let s1 = StringSet.add "3"(
  StringSet.add "2" (
    StringSet.add "1" StringSet.empty
  )
)

let s1 = empty
  |> StringSet.add "1"
  |> StringSet.add "2"
  |> StringSet.add "3"

let rs1 = StringSet.elements s1;;

let rec oflist lst = match lst with
  | h :: t -> StringSet.add h (oflist t)
  | [] -> StringSet.empty
let r = oflist ["1"; "2"; "3"; "1"];;
let rs1 = StringSet.elements r;;

let oflist2 lst = List.fold_left
  (fun r e -> StringSet.add e r)
  StringSet.empty
  lst;;
let r = oflist2 ["1"; "2"; "3"; "4"; "1"];;
let rs1 = StringSet.elements r;;

let rs1 = oflist ["1"; "2"; "3"; "1"] |> StringSet.elements;;
let rs1 = oflist2 ["1"; "2"; "3"; "4"; "1"] |> StringSet.elements;;

let r = Intset.of_list [1;2;3;4;3;2;1;13;1] |> Intset.elements;;

let r1 = Intset.of_list [1;2;3;4;3;2;1;13;1] |> Intset.filter(fun h -> h > 2) |> Intset.elements;;
let r1 = Intset.of_list [1;2;3;4;3;2;1;13;1] |> Intset.map(fun h -> h * 2) |> Intset.elements;;
let r1 = Intset.fold (fun e p -> e + p) (Intset.of_list [1;2;3;4;3;2;1;13;1]) 0 ;;
let r1 = Intset.fold ( + ) (Intset.of_list [1;2;3;4;3;2;1;13;1]) 0 ;;

(*implementati filter cu fold*)

let filter pred mult = Intset.fold (fun e p -> if pred e then Intset.add e p else p) (Intset.of_list [1;2;3;4;3;2;1;13;1]) Intset.empty;;

let r1 = Intset.of_list [1;2;3;4;3;2;1;13;1] |> filter (fun e -> e > 2) |> Intset.elements;;

let union multa multb = Intset.fold (fun e r -> Intset.add e r) multa multb;;
let r = union (Intset.of_list [1;2;3]) (Intset.of_list [3;4;5]) |> Intset.elements;;

let intersect multa multb = Intset.fold (fun e r -> if Intset.mem e multb then Intset.add e r else r) multa Intset.empty;;
let r = intersect (Intset.of_list [1;2;3]) (Intset.of_list [3;4;5]) |> Intset.elements;;

;;