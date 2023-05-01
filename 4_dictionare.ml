module Int = struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make(Int)

let dict = IntMap.empty
|> IntMap.add 1 0
|> IntMap.add 2 2
|> IntMap.add 3 2
|> IntMap.add 4 4
|> IntMap.add 5 6
|> IntMap.add 6 4
|> IntMap.add 7 8


let exists pred dic = IntMap.fold (fun k v r -> 
try IntMap.iter (fun k v -> if pred v then raise Exit) dic;
false  
with Exit -> true)
 dic false

let r = exists (fun h -> h > 9) dict;;

let for_all pred dic = IntMap.fold (fun k v r ->
try IntMap.iter (fun k v -> if not(pred v) then raise Exit) dic;
true 
with Exit -> false)
 dic true

let rr = for_all (fun h -> h mod 2 = 0) dict;;