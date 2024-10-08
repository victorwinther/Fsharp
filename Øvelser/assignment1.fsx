 
 // Assignment 1 may 2022
 type Book  = string
    type Shelf = Book list   // ordered alphabetically
    type Date  = int
    type Name  = string
    type Loan  = Book * Name * Date
    type LoanList = Loan list

 let sh0 : Shelf = ["Introduction to meta-mathematics";
                   "To mock a mockingbird";
                   "What is the name of this book"];;

let ls0 : LoanList  = [("Communication and concurrency", "Bob", 4);
                   ("Programming in Haskell", "Paul", 2);
                   ("Communicating Sequential processes", "Mary", 7);
                   ("Elements of the theory of computation", "Dick", 1)]

let l1: Loan = ("Communication and concurrency", "Bob", 4)

let rec onShelf (b:Book) (s: Shelf) =
        match s with 
        | [] -> false
        | h::t when h = b -> true 
        | h::t -> onShelf b t

onShelf "Introduction to meta-mathematics" sh0

// 2
let rec toShelf (b:Book) (bs:Shelf) : Shelf= 
        match bs with
        | [] -> [b]
        | h::t when b[0] < h[0] -> b::bs
        | h::t -> h::toShelf b t

toShelf "Abekat to meta" sh0         

let rec fromShelf b = function
        | [] -> None  
        | h::t when h = b -> Some t
        | h::t -> None
        
// fromShelf "To mock a mockingbird" sh0

let addLoan b n d ls = 
    (b,n,d)::ls

let rec removeLoan (b:Book) (n:Name) (ls:LoanList) : LoanList =
        match ls with 
        | [] -> []
        | (book,name,_)::xs when (book = b && name = n) -> removeLoan b n xs
        | (q,w,e)::xs -> (q,w,e)::removeLoan b n xs

removeLoan "Programming in Haskell" "Paul" ls0

let reminders (d:Date)(ll:LoanList) =
       let filtered = List.filter(fun(_,_,date) -> (date < d)) ll
       List.map(fun(name,book,_) -> (name,book)) filtered

reminders 3 ls0

// insane recursion! se lecture 4 side 19
let rec reminders2 d = function
        | [] -> []
        | (book,name,date)::xs when date < d -> (book,name)::reminders2 d xs
        | (_,_,_)::xs -> reminders2 d xs

let loanLL = [("Paul","abebog");("Jens","Junglebogen")]
let rec toLetters2 lb = 
        match lb with
        | [] -> []
        | (name,book)::xs -> $"Dear %s{name}!\n PLEASE RETURN \"%s{book}\" \n Regards din mor"::toLetters2 xs

toLetters2 loanLL

let toLetters loanList = 
        List.map(fun(name,book) -> $"Dear %s{name}!\n PLEASE RETURN \"%s{book}\" \n Regards din mor") loanList

toLetters loanLL

let reminders3 (d:Date)(ll:LoanList) =
        List.foldBack (fun (book,name,date) acc -> if (date < d) then (name,book)::acc else acc) ll []

reminders3 3 ls0       



//Problem 3
type T = | One of int | Two of int * T * int * T

let rec f p t =
        match t with
        | One v when p v -> [v] (* C1 *)
        | Two(v1,t1,_,_) when p v1 -> v1::f p t1 (* C2 *)
        | Two(_,_,v2,t2) -> v2::f p t2 (* C3 *)
        | _ -> [];; (* C4 *)

