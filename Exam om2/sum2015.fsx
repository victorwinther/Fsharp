
//Sommer 2015
// Problem 1
let rec repeat (s: string) (n:int) : string = 
    match s,n with
    | _,0 -> ""
    | a,n1 -> "" + a + repeat s (n1-1)

repeat "ab" 4

let rec f s1 s2 n = 
    match s1,s2,n with
    | _,_,0 -> ""
    | a,_,c when c % 2 = 0 -> ""+a + "\n" + f s1 s2 (c-1)
    | _,b,c -> ""+b + "\n" + f s1 s2 (c-1)

f "XO" "OX" 4

// Problem 2

let mixMap f xs ys = List.map f (List.zip xs ys)

let unmixMap f g ys = 
        let x,y = List.unzip ys
        (List.map f x,List.map g y)

 // 3: Most genereal types
    // mixmap : (a'* -> b*-> (c',d')) -> a' list -> b' list -> (c',d') list
    // unmixmap : (a' -> c') -> (b' -> d') -> (a'*b') list -> (c' list, d' list)

type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;

let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)));;           

let rec reflect (tree: Tree<'a>) : Tree<'a> =
    match tree with
    | Lf -> Lf
    | Br(left, value, right) -> 
        Br(reflect right, value, reflect left)


let accumulate (tree: Tree<int>) : Tree<int> =
    let rec accumulateAux (tree: Tree<int>) (total: int) : Tree<int> =
        match tree with
        | Lf -> Lf
        | Br(left, value, right) ->
            let newTotal = total + value
            Br(accumulateAux left newTotal, newTotal, accumulateAux right newTotal)

    accumulateAux tree 0

accumulate t

let rec k i t =
    match t with
    | Lf -> Lf
    | Br(tl,a,tr) -> Br(k (i*i) tl, i*a, k (i*i) tr);;

let rec h n m =
        function
        |Br(tl,a,tr) when n=m -> h n 1 tl @ [a] @ h n 1 tr
        | Br(tl,_,tr) -> h n (m+1) tl @ h n (m+1) tr
        | Lf -> []

let q n t = h n n t;;

// Problem 4
type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS

type CourseBase = Map<CourseNo, CourseDesc>


let isValidCourseDesc ((a:Title),b) = b > 0 && b % 5 = 0

isValidCourseDesc ("Abe",3)

let isValidCourseBase (cd:CourseBase) = Map.forall (fun n s -> isValidCourseDesc s)  cd

type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

let disjoint a b = Set.count (Set.intersect a b) = 0
