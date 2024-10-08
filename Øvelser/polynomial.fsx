type Poly = int list

let rec add (a:Poly) (b:Poly) =
    match a,b with 
    |   [],[] -> []
    |   [],_ -> b
    |    _, [] -> a
    |   x::s, y::ys -> (x + y) :: (add s ys)

let rec mulC (a: int) (b: Poly) =
    match b with
    | [] -> []
    | head::tail -> (head * a) :: (mulC a tail)

let rec sub (a:Poly) (b:Poly) =
    match a,b with 
    |   [],[] -> []
    |   [],x::s -> (-x) :: (sub a s)
    |    _, [] -> a
    |   x::s, y::ys -> (x - y) :: (sub s ys)

let mulX = function
    | [] -> []
    | a -> 0::a

let rec mul (a:Poly) = function 
    | [] -> []
    | x::s -> add (mulC x a) (mulX( mul a s))

let eval (a: float) (b:float list)=
        let rec eval' (a:float) (b:float list) (pow: float) (sum:float) = 
             match b with
             | [] -> sum
             | x::s -> eval' x s (pow + 1.0) (if sum = 0 then x else sum + ((x*a) ** pow)) 
        eval' a b 0 0

eval 2 [2;3;0;1]

let isTrue (a: int list) =
    if a.Head = 0 then false else true
let isLegal (ns: int List) =
    if ns.Length = 0 then true else
    ns 
    |> List.rev
    |> isTrue 

isLegal [0]   

let rec reverse (xs:int list) = 
    xs 
    |> List.rev

let prune (xs:int list) : int list = 
        let rec prune_rec l =
            match l with
            | head::tail when head = 0 -> prune_rec tail
            | _ -> l

        reverse (prune_rec (reverse xs))

prune [1;0;2;0;0;0]       
    