//2016 fall
//Problem 1
type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

let rec inv (sb: Scoreboard) = 
    match sb with
    | [] -> true
    | (_,_,p)::_ when p < 0 -> false
    | (_,_,p)::((_,_,p1)::_) when p < p1 -> false
    | _ :: rest -> inv rest

inv sb     

let rec insert s sb = 
    let (_,_,p1) = s
    match sb with
    | (a,b,c)::xs when p1 > c -> s::(a,b,c)::xs
    | (a,b,c)::xs -> (a,b,c)::insert s xs
    | [] -> [s]

insert ("Victor","Basket",2) sb 

let rec get (n,sb) =
    match sb with
    | [] -> []
    | (n1,a,p)::xs when n = n1 -> (a,p)::get (n,xs)
    | x::xs -> get(n,xs)

get(("Joe",sb))

// Problem 2

let rec replace a b xs = 
    match xs with
    | x::xs when x = a -> b::replace a b xs
    | x::xs -> x::replace a b xs
    | [] -> []

replace 2 7 [1; 2; 3; 2; 4]   

// Problem 3
// Pos has the type seq<int> and denotes an infinite number of postive number
// seq1 has the type seq<int*int> and has (0,0),(1,1)(-1,-1) and so on
// val1 has seq<int*int> and is up to five..

// seq2 <int*int> (0,0)(1,1)(2,1)(2,2)(3,1)(3,2(3,3))

// Problem 4
type Tree<'a,'b> = | A of 'a | B of 'b| Node of Tree<'a,'b> * Tree<'a,'b>;;

let v1 = A false
let v2 = B [1,2,3]
let v3 = Node(v1,v2)

let rec countAnodes tree =  
    match tree with 
    | A _ -> 1
    | B _ -> 0
    | Node(a1,b2) -> countAnodes a1 + countAnodes b2

countAnodes v3

let rec subst a a' b b' t = 
    match t with
    | A bo when bo = a -> A a'
    | B bo when bo = b -> A b'
    | Node(a1,a2) -> Node(subst a a' b b' a1,subst a a' b b' a2) 
    | leaf -> leaf


// Problem 5
type T<'a> = N of 'a * T<'a> list;;

let td = N("g", []);;
let tc = N("c", [N("d",[]); N("e",[td])]);;
let tb = N("b", [N("c",[])]);;
let ta = N("a", [tb; tc; N("f",[])])

let rec toList t = 
    match t with
    | N(v,ts) -> v::List.collect toList ts

toList tc

let rec map f t = 
    match t with
    | N(v,ts) -> N(f v,List.map (map f) ts)

map (fun x -> if x = "a" then "b" else "c") tc

type Path = int list;;
