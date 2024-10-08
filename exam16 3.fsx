type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", -1)];;

let rec inv sb = 
    match sb with
    | [] -> true
    | (_,_,p)::(_,_,p1)::xs when p < p1 || p1 < 0 -> false
    | x::xs -> inv xs

inv sb

let rec insert s sb = 
    let (n1,e2,p1) = s 
    match sb with 
    |[] -> [s]
    | (n,e,p)::xs when p < p1 -> s::(n,e,p)::xs
    | x::xs -> x::insert s xs

insert ("Victor","Basket",33) sb

let top (k:int) (sb:Scoreboard) : Scoreboard option =
        if k < 0 then None else try Some (List.take k sb) with | _ -> None         

// Problem 4
type Tree<'a,'b> = | A of 'a | B of 'b| Node of Tree<'a,'b> * Tree<'a,'b>;;
let t1 = A true
let t2 = B [1]
let t3 = Node (t1,t2)
let t4 = Node (t3,t3)

let rec t = function
    | A _ -> 1
    | B _ -> 0
    | Node(t1,t2) -> t t1 + t t2

t t4

let rec subst a a' b b' = function
    | A a1 when a = a1 -> A a' 
    | B b1 when b1 = b -> B b'
    | A a -> A a
    | B b -> B b
    | Node(t1,t2) -> Node(subst a a' b b' t1, subst a a' b b' t2)

// Problem 5
type T<'a> = N of 'a * T<'a> list;;

let td = N("g", []);;
let tc = N("c", [N("d",[]); N("e",[td])]);;
let tb = N("b", [N("c",[])]);;
let ta = N("a", [tb; tc; N("f",[])])

let rec toList t = 
    match t with
    | N(a,[]) -> [a]
    | N(a,b) -> a::List.collect toList b

toList tc


