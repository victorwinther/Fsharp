// 2016 problem 2

let rec replace a b xs = 
    match xs with 
    | [] -> []
    | x::tail when x = a -> b::replace a b tail
    | x::tail -> x::replace a b tail

replace 2 7 [1; 2; 3; 2; 4]

let replaceTail a b xs =
    let rec replace' a b xs acc = 
        match xs with 
        | [] -> List.rev acc
        | x::tail when x = a -> replace' a b tail (b::acc)
        | x::tail -> replace' a b tail (x::acc)
    replace' a b xs []

replaceTail 2 7 [1; 2; 3; 2; 4]    

let replaceTail2 a b xs =
    let rec replace' a b xs acc = 
        match xs with 
        | [] -> acc
        | x::tail when x = a -> replace' a b tail (acc@[b])
        | x::tail -> replace' a b tail (acc@[x])
    replace' a b xs []

replaceTail2 2 7 [1; 2; 3; 2; 4]    


// problem 3 
// seq <int> og seq<int*int>
// val1=(0,0), (1,1),(-1,-1),(2,2)(-2,-2)
// val2= [(0,0),(1,0),(1,1)...]

// problem 4
// A true
// B [1]
// Node(Atrue,B[1])

// 2

type Tree<'a,'b> = | A of 'a | B of 'b| Node of Tree<'a,'b> * Tree<'a,'b>

let rec count = function
    | A x -> 1
    | B x -> 0
    | Node(x1,x2) -> count x1 + count x2 


let rec subt a a' b b' = function
    | A x when x = a -> A a'
    | B x when x = b -> B b'
    | Node(x1,x2) -> Node( subt a a' b b' ...)
    | leaf -> leaf

let pos = Seq.initInfinite (fun i -> i+1)

let seq1 = seq { 
                                            yield (0,0)
                                            for i in pos do
                                            yield (i,i)
                                            yield (-i,-i) }
let val1 = Seq.take 5 seq1;;
let nat = Seq.initInfinite id;;
let seq2 = seq { 
                for i in nat do 
                yield (i,0)
                for j in [1 .. i] do
                yield (i,j) }

let val2 = Seq.toList(Seq.take 10 seq2);;