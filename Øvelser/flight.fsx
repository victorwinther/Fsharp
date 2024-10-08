type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list


let route1 = [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]
let LugCat: LuggageCatalogue= [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]);("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]

let rec findRoute (lid:Lid) (lug:LuggageCatalogue): Route =
    match lug with 
    | [] -> failwith "no routes"
    | (a,b)::xs when a = lid -> b
    | (_,_)::xs -> findRoute lid xs

findRoute "DL 016-914" LugCat    

let rec inRoute (f:Flight)(r:Route):bool =
    match r with
    | [] -> false
    | (a,b)::xs when a = f -> true
    | (_,_)::xs -> inRoute f xs 

inRoute "DL 124" route1    

let rec withFlight (f:Flight) (lc:LuggageCatalogue): Lid list =
    match lc with
    | [] -> []
    | (lid,r):: lc when inRoute f r -> lid::withFlight f lc
    | _::lc -> withFlight f lc

withFlight "DL 124" LugCat    

type ArrivalCatalogue = (Airport * Lid list) list

let ac = [("ATL", ["DL 016-914"; "SK 222-142"]); ("BRU", ["DL 016-914"; "SK 222-142"]);("JFK", ["SK 222-142"]);("CPH", ["DL 016-914"])]

let rec extend (lid:Lid)(r:Route)(ac:ArrivalCatalogue):ArrivalCatalogue =
    match ac with
    | [] -> []
    | (airp,l)::xs when List.exists (fun (_,a) -> a = airp) r ->  (airp,l@[lid])::extend lid r xs
    | (airp,l)::xs -> (airp,l)::extend lid r xs

extend "AB VIC-KIN" route1 ac

let toArrivalCatalogue (lg:LuggageCatalogue) =
   lg 
    |> List.map(fun (a,r) -> r )
    |> List.map(fun [a,b] -> (b,[""]))
   //|> List.fold (fun (lid,route) acc -> extend lid route acc) []

toArrivalCatalogue LugCat