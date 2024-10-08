let rec bin (n,k) = 
    if k = 0 then 1
    else if n = k then 1
    else bin(n-1,k-1) + bin(n-1,k);;

bin(8,4)
    

let rec fact n = 
    if n = 0 then 1
    else n * fact(n-1);;

fact 3

let rec fib n =
    if n = 0 then 0
    else if n = 1 then 1
    else fib(n-1) + fib (n-2);;

fib 5   

//opg 2.1
let divisible x =
    if( x % 2 = 0 || x % 3 = 0) && not (x % 5 = 0) then true 
        else 
            false;;

divisible 30;;


//opg 2.2
let rec pow (s:string,n:int) =
    if n <= 0 then ""
    else if n = 1 then s
    else s + " " + pow(s,n-1);;
    
pow("hej",5)   

// opg 4.3
let evenN n = [ for i in 1 .. n -> (i * 2)]

// opg 4.7
let rec multiplicity (x: int) (xs: int list) =
        match xs with
        | [] -> 0
        | head:: tail when head = x -> 1 + multiplicity x tail
        | head::tail ->  multiplicity x tail
    
multiplicity 5 [1;2;5;5;5] 

let multi (xs: int list) = 
    xs
    |> List.filter(fun x -> x = 5)
    |> List.length

multi [1;2;5;5;5]

// opg 4.8
let split list = 
    let rec split odd even list =
        match list with
        | a::b::tail -> split (a::odd) (b::even) tail
        | a::tail -> split (a::odd) even tail
        | [] -> List.rev odd, List.rev even

    split [] [] list

// opg 4.9
let rec zip (a, b) =
    match(a,b) with
    | aHead :: aTail, bHead::bTail -> (aHead,bHead) :: zip(aTail,bTail)
    | _, _ -> []
    
zip ([1;2;3;4], [4;5;6;7]);;

// opg 4.12

let sum (p, xs) =
    let rec sum' p xs tempSum = 
        match xs with
        | []        -> tempSum
        | x::xs     -> sum' p xs (tempSum + (if p x then x else 0))
    sum' p xs 0

let sum1(p, xs) = 
    xs 
    |> List.filter(fun x -> p x)
    |> List.length

sum1((fun x -> x > 0),[-1;1;2;3;4])

// 5.13


let nList p list = List.fold (fun start elem -> if p elem then (start + 1) else 0 ) 0 list

nList (fun x -> x > 0) [-1;1;2;3;4;5]