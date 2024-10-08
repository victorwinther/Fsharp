type Name = string
type PhoneNumber = string
type YearOfBirth = int
type Interest = string
type ThemesOfInterest = Interest list
type EmployeeDescription = PhoneNumber * YearOfBirth * ThemesOfInterest
type Employee = Name * EmployeeDescription
type Register = Employee list

type ClubArrangment = EmployeeDescription -> bool

let alice: Employee = ("Alice",("123",1988,["soccer";"jazz"]))
let bob : Employee = ("Bob",("456",1967,["soccer";"fishing"]))
let charlie : Employee = ("Charlie",("789",1991,["fishing";"jazz"]))
let register : Register = [alice; bob; charlie]

let rec isMember x = function
    | [] -> false
    | x' :: xs -> x = x' || isMember x xs


let p1(_,yb,ths) = yb > 1982 && isMember "soccer" ths && isMember "jazz" ths

let p2(_,yb,ths) = yb > 1982 && (isMember "soccer" ths || isMember "jazz" ths)

let rec extractInteresed p = function
    | [] -> []
    | (n,(no,yb,ths))::emps -> 
        if p(no,yb,ths) 
        then (n,no) :: extractInteresed p emps
        else extractInteresed p emps
        
let test1 = 
    extractInteresed p1 register = [("Alice","123")]

let test2 = extractInteresed p2 register = [("Alice","123"); ("Charlie","789")]

let test3 = extractInteresed p1 [] = []