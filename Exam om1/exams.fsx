// 2015 summer problem 2

let mixMap f xs ys = 
    List.map f (List.zip xs ys)


let unMixMap f g xs = 
        let (x,y) = List.unzip xs 
        (List.map f x, List.map g y)   

// 2016 

type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb:Scoreboard = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

let rec inv (sb: Scoreboard) = 
    match sb with
    | [] -> true
    | (_,_,p)::_ when p < 0 -> false
    | (_,_,p)::((_,_,p1)::_) when p < p1 -> false
    | _ :: rest -> inv rest

let rec insert (s:Score) (sb:Scoreboard) : Scoreboard =
    let (_,_,p1) = s
    match sb with
    | [] -> [s]
    | (_,_,p) ::_ when p < p1 -> s::sb
    | x::rest -> x::insert s rest

insert ("Victor","Basket",55) sb 

let get (n:Name,sb:Scoreboard) : (Event * Point) list =
    let filtered = List.filter (fun (name,_,_) -> name = n) sb
    List.map(fun(_,e,p) -> (e,p)) filtered

get("Joe",sb)

let top (k:int) (sb:Scoreboard) : Scoreboard option =
    if k < 0 then None else try Some (List.take k sb) with | _ -> None

 // problem 2
let rec replace a b ls = 
    match ls with 
    | [] -> []
    | x::s when x = a -> b::replace a b s
    | x::s -> x::replace a b s

replace 2 7 [1;2;3;2;4]    

// 2011 Fall Q1
type name = string
type phone = int 
type level = int

type description = phone * level 
type register = (name*description) list

let register:register = [("Joe",(10101010,4));("Sal",(11111111,2));("Sam",(12121212,7));("Jane",(13131313,1))]


let getPhone  (name:name) (register:register) =
        List.pick (fun elem -> 
                    match elem with
                    | (name,(ph,_)) -> Some ph
                    | (_,(_,_)) -> None ) register
                
let rec getPhone1 nm = function
    | (ac',(ph,_))::_ when nm=ac' -> ph
    | _::reg -> getPhone1 nm reg
    | _ ->      failwith(nm + " is an unknown name");;
getPhone1 "Joe" register     

let delete (n:name) (r:register) =
    List.filter (fun (name,(_,_)) -> name <> n ) register

delete "Joe" register   

let getCandidates (l:level) (r:register) =
    let filtered = List.filter (fun (_,(_,level)) -> level = l || (level <= l + 2 && level >= l - 2)) register
    List.map(fun (name,(ph,_)) -> (name,ph)) filtered

getCandidates 5 register

type exp = | C of int
            | BinOp of exp * string * exp 

let a1 = BinOp(BinOp(C 2, "-", C 1), "+", C 5)
let a2 = C 5
let a3 = C(3)

let rec toString (e:exp) : string =
        match e with
        | C numb -> numb.ToString()
        | BinOp(e1, op, e2) -> "("+(toString e1)+op+(toString e2)+")"
toString a2        
        
// 2016 problem 2

let rec replace1 a b xs = 
    match xs with 
    | [] -> []
    | x::tail when x = a -> b::replace1 a b xs
    | x::tail -> x::replace1 a b xs

replace1 2 7 [1; 2; 3; 2; 4]