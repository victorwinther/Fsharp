type Rel<'a,'b> = ('a * 'b list) list
let rel: Rel<int,string> = [(1, ["a"; "b"; "c"]); (4,["b"; "e"])];;

let rec apply x rel = 
    match rel with
    | [] -> []
    | (a,b)::xs when a = x -> b
    | (_,b)::xs -> apply x xs

apply 1 rel   

let inRelation x y rel =
  let rec helper = function
    | [] -> false
    | (xi, ysi)::rest ->
      if xi = x then List.exists (fun yi -> yi = y) ysi
      else helper rest
  helper rel    

inRelation 4 "e" rel

let rec insert x y rel = 
    match rel with
    | [] -> []
    | (xi,ysi)::rest when x = xi -> (xi,ysi@[y])::rest
    | (xi,ysi)::rest -> insert x y rest

insert 2 "c" [(1,["a"]); (2,["b"])]

// Problem 2 
let multTable a = 
    let table = seq { for i in 1 .. 10 -> i*a}
    table

multTable 3

let tableOf m n f = 
    seq { for i in 1..m do
                         for j in 1..n -> 
                            (i,j, f i j) }

tableOf 3 4 (+)          

let aSequence = Seq.initInfinite (fun i -> String.replicate (i + 1) "a")

// Problem 3
type T<'a> = N of 'a * T<'a> list

let rec f(N(e,es)) = e :: g es
and g = function
        | [] -> []
        | e::es -> f e @ g es;;
let rec h p t =
    match t with
    | N(e,_) when p e -> N(e,[])
    | N(e,es) -> N(e, List.map (h p) es);;

let rec k (N(_, es)) = 1 + List.fold max 0 (List.map k es)

let v1 = N("Hej",[])
let v2 = N("hej",[N("hej",[])])
let v3 = N("",[])