 // 2018 problem 1
let rec f xs ys = 
            match (xs,ys) with
            | (x::xs1, y::ys1) -> x::y::f xs1 ys1
            | _ -> []

f [] []              
(* Part 1
!-> f [1;6;0;8] [0; 7; 3; 3]
!-> 1::0::f [6;0;8] [7; 3; 3]
!-> 1::0::6::7::f [0;8] [3; 3]
                !-> 1::0::6::7::0::3::f [8] [3]
                !-> 1::0::6::7::0::3::8::3::f [] []
                !-> 1::0::6::7::0::3::8::3::[]
                -> 1::0::6::7::0::3::8::[3]
                -> 1::0::6::7::0::3::[8;3]
                -> 1::0::6::7::0::[3;8;3]
                -> 1::0::6::7::[0;3;8;3]
                -> 1::0::6::[7;0;3;8;3]
                -> 1::0::[6;7;0;3;8;3]
                -> 1::[0;6;7;0;3;8;3]
                -> [1;0;6;7;0;3;8;3]
        
            *)

            (* Part 2
        
                f : 'a list -> 'a list -> 'a list
                f computes the zipped list. ie. a new list alternating between elements in 2 lists given. (cutting short when one list no longer has any elements)
            *)

//Part 3

//f is not tail recursive, as each recursive call adds 2 list concatination operations to the callstack
let fTR xs ys = 
    let rec fTR' xs ys acc = 
        match (xs,ys) with
        | (x::xs1, y::ys1) -> fTR' xs1 ys1 (x::y:acc)
        | _ -> acc

    fTR' xs ys []

fTR [] []

let cTR xs ys = 
    let rec cTR' xs ys k = 
        match(xs,ys) with 
        | (x::xs1, y::ys1) -> cTR' xs1 ys1 (fun v -> k x::y::v)
        | _ -> k []

    cTR' xs ys (fun x -> x )

// Problem 2.1 from May 2017 
// f 5 -> negative argument [5,3,1,-1] int -> int list, 
let h s k = seq { for a in s do yield k a }

h (seq [1;2;3;4])

// Problem 3 from May 16
type Container =
| Tank of float * float * float // (length, width, height)
| Ball of float // radiu

let t1 = Tank(5.0,5.0,5.0)
let t2 = Tank(6.0,6.0,6.0)
let b1 = Ball(2.0)
let b2 = Ball(1.0)

let isWF c = 
    match c with
    | Tank(a,b,c) -> a > 0 && b > 0 && c > 0
    | Ball(a) -> a > 0

let volume c = 
    match c with
    | Tank(a,b,c) -> a * b * c
    | Ball(a) -> (4.0/3.0) * 3.14 * a*a*a 

type Name = string
type Contents = string
type Storage = Map<Name, Contents*Container>

let s1 = Storage [("tank1",("oil",Tank(5,5,5)));("ball1", ("water", Ball(5)))]

let rec find n stg = Map.find n stg

find "tank1" s1

// Problem 4 from May 16
