type Appliance = string
type Usage = Appliance * int

let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

let rec inv (ul: Usage list) = 
    match ul with
    | [] -> true
    | (_,b)::xs when b > 0 -> inv xs
    | (_,_)::xs -> false

let inv1 (ul: Usage list) =
    match List.tryFind(fun (_,a) -> a < 1) ul with
    | Some value -> false
    | None -> true
   
inv1 ats

let durationOf a ats= 
     List.fold (fun start (ad,i) -> if a = ad then (start + i) else start ) 0 ats

// durationOf "washing machine" ats     

let wellformed ats = 
    let time = List.fold(fun start (_,i) -> i + start) 0 ats
    if time < 24 && inv ats then true else false

// wellformed ats

let delete (a:Appliance) ats =
    List.fold(fun start ((b:Appliance),_) -> if b <> a then b::start else start)  [] ats

delete "coffee machine" ats

type Price = int
type Tariff = Map<Appliance, Price>

let (trf:Tariff) = Map["washing machine",5;"coffee machine",5;"dishwasher", 2] 

let isDefined ats trf = 
    List.forall(fun (app,_) -> Map.containsKey app trf) ats

isDefined ats trf

let priceOf ats trf = 
    List.fold(fun start (app,_) -> start + (Map.find app trf)) 0 ats

priceOf ats trf    

// PROBLEM 3
type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
and Tributaries = River list

let riv1 = R("R1",5,[])
let riv4 = R("R4",2,[])
let riv2 = R("R2",15,[riv4])
let riv3 = R("R3",8,[])
let riv = R("R",10,[riv1;riv2;riv3])

let rec contains name  = 
    function
    | R(name',_,_) when name = name' -> true
    | R(_,_,river) -> List.exists(contains name) river

contains "R1" riv

let rec allNames r = 
    match r with
    | R(n,_,[]) -> [n]
    | R(n,_,river) -> n::List.collect allNames river

allNames riv    

let rec totalFlow r = 
    match r with
    |R(_,flow,[]) -> flow
    |R(_,flow,river)-> flow + List.sumBy totalFlow river

totalFlow riv    

let rec mainSource r = 
    match r with
    |R(n,flow,[]) -> (n,flow)
    |R(_,flow,river)-> List.maxBy mainSource river