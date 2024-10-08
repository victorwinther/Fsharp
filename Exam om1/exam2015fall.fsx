
//Exam 2015 fall
type Appliance = string
type Usage = Appliance * int

let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

let rec inv ats =
    match ats with
    | [] -> true
    | (usage,t)::tail when t > 0 -> inv tail 
    | (_,_)::tail -> false

inv ats

let durationOf apli ats = 
    let rec durationOf' apli ats acc = 
        match ats with 
        | [] -> acc
        | (usage,t)::tail when apli = usage -> durationOf' apli tail (acc+t)
        | (_,_)::tail -> durationOf' apli tail acc
    durationOf' apli ats 0    

durationOf "washing machine" ats     

let wellform ats = 
    let rec wellform' ats acc = 
        match ats with 
        | [] when inv ats && acc < 25 -> true
        | [] -> false
        | (usage,t)::tail -> wellform' tail (acc+t)

    wellform' ats 0    

wellform ats

let rec delete a ats = 
    match ats with
    | [] -> []
    | (usage,_)::tail when usage = a -> delete a tail
    | (usage,t)::tail -> (usage,t)::delete a tail

delete "coffee machine" ats    

// Q2

let rec g1 p = function
            | x::xs when p x -> x :: g1 p xs
            | _ -> [];;
            
let rec g2 f h n x =
        match n with
        | _ when n<0 -> failwith "negative n is not allowed"
        | 0 -> x
        | n -> g2 h f (n-1) (f x);;

// Q3
type Name = string
    type Flow = int
    type River = R of Name * Flow * River list
    type Tributaries = River list

let riv = R("R",10,[R("R1",5,[]);R("R2",15,[R("R4",2,[])]);R("R3",8,[])]) 
let riv3 = R("R3",8,[])

let rec contains n r= 
    match r with
    | R (name, _,_) when name = n -> true
    | R (_,_,t) -> List.exists(fun x -> contains n x) t
   

contains "R5" riv

let rec allNames r = 
    match r with 
    | R (n,_,[]) -> [n]
    | R (n,_,t) -> n::List.collect allNames t

allNames riv     

let rec totalFlow r = 
    match r with
    | R (_,f,[]) -> f
    | R (_,f,t) -> (List.sumBy (fun x -> (totalFlow x)) t) + f

totalFlow riv

