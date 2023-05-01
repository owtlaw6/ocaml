module Int = struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make(Int)

let f g dic = IntMap.fold (fun key value rez ->IntMap.add key (g value) rez ) dic IntMap.empty 

let dict = IntMap.empty
|> IntMap.add 1 1
|> IntMap.add 2 2
|> IntMap.add 3 3
|> IntMap.add 4 4
|> IntMap.add 5 5
|> IntMap.add 6 6
|> IntMap.add 7 7

let r = f (fun h -> 2*h ) dict |> IntMap.bindings;;

