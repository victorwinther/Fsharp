//2011
// Problem 1
type name = string;;
type phone = int;;
type level = int;;

type desription = phone * level;;
type register = (name * desription) list;;

//1
let reg:register = [("Joe",(10101010,4));("Sal",(11111111,2));("Sam",(12121212,7));("Jane",(13131313,1))]

//2

let getPhone n reg  = List.tryFind (fun (a,b) -> a = n) reg

getPhone "Joe" reg

let rec delete (name,register) =
    match register with
    | (n,_)::xs when n = name -> xs
    | (n,d)::xs -> (n,d)::delete(n,xs)
    | [] -> []

delete ("Joe",reg)    

let rec getCandidates l reg =
        match reg with
        | (n,(p,level))::xs when (l-3) < level && (l + 3) > level -> (n,l)::getCandidates l xs
        | x::xs -> getCandidates l xs
        | [] -> []

getCandidates 5 reg        

// Problem 2
type exp = | C of int | BinOp of exp * string * exp

let exp1 = C 5
let exp2 = BinOp (exp1,"*",exp1)
let exp3 = C 7

let rec toString exp = 
        match exp with
        | C a -> string a 
        | BinOp(e1,op,e2) -> "(" + (toString e1) + op + (toString e2) + ")"


toString exp2
