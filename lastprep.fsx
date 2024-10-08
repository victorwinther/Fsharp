type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics";"To mock a mockingbird";"What is the name of this book"];;
let ls0 = [("Communication and concurrency", "Bob", 4);("Programming in Haskell", "Paul", 2);("Communicating Sequential processes", "Mary", 7);("Elements of the theory of computation", "Dick", 1)];;

let rec onShelf b sh = 
    match sh with
    | [] -> false
    | a when a = b -> true
    | _::xs -> onShelf b xs

let rec toShelf (b:Book) (sh:Shelf) = 
    match sh with
    | [] -> []
    | a::xs when a[0] > b[0] ->  b::a::xs
    | a::xs -> a::toShelf b xs
    
toShelf "Abekat to meta" sh0        

let rec fromShelf b sh: Shelf option =
    match sh with 
    | [] -> None
    | a::xs when a = b -> Some xs
    | a::xs -> fromShelf b xs

fromShelf "Introduction to meta-mathematics" sh0

let addLoan b n d ls = 
    match ls with
    | a::xs -> (b,n,d)::a::xs
    | [] -> []

let rec removeLoan b n ls = 
    match ls with
    | [] -> []
    | (b',n',d)::xs when b = b' && n = n' -> xs
    | x::xs -> removeLoan b n xs

removeLoan "abe" ls0    

let rec reminders d ls =
    match ls with 
    | (b,l,d1)::xs when d1 < d -> (l,b)::reminders d xs
    | _::xs -> reminders d xs
    | [] -> []

reminders 3 ls0   

let rec toLetters ls = 
    match ls with
    | [] -> []
    | (n,b)::xs -> $"Dear %s{n}! \n Please return \" %s{b} \". \n Kinds regards robin"::toLetters xs 

toLetters [("Paul","LORTEBOG")]    

let rec insertBefore p x xs =
  match xs with
  | [] -> []
  | head :: tail when p head -> x :: head :: tail
  | head :: tail -> head :: insertBefore p x tail

insertBefore (fun x -> x > 5) 3 [2;3;5;6] 

type Name1 = string;;
type Sex = 
| M 
| F 
type YearOfBirth = int;;
type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list;;

let sexToString s = 
    match s with
    | M -> "male"
    | F -> "female"

let male = M
sexToString male

let fam = P("Victor",male,2000,[P("Adam",male, 2005,[])])

let fam2 = P("Larry", M, 1920, [P("May", F, 1955, ([])); P("Joe", M, 1950, ([]));P("Paul", M, 1945, ([]))])

let rec checkChild2 age (children:Children) =
    match children with 
    | P(_,_,age1,[])::[] -> true
    | P(_,_,age)::

let rec isWF2 (tree:FamilyTree) : bool =
    match tree with 
    | P(_,_,_,[]) -> true
    | P(_,_,age,children) -> checkChild2 age children


let makePerson n s y = P(n,s,y,[])

makePerson "Neger" "NON-BINARY" 0000 






let isWF (tree: FamilyTree) : bool =
    // Recursive helper function to check whether a given node in the tree
    // satisfies the well-formedness conditions
    let rec checkNode (node: FamilyTree) =
        // Check whether the node's age is greater than that of its children
        let (P(_, _, age, children)) = node
        if children = [] then
            true
        else
            let rec checkChildren (children: Children) =
                match children with
                | [] -> true
                | h::t ->
                    let (P(_, _, childAge, _)) = h
                    if childAge > age then
                        false
                    else
                        checkChildren t
            checkChildren children
    // Call the helper function on the root node of the tree
    checkNode tree

isWF fam2