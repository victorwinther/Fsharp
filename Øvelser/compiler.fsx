type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int
type Exp = | X | C of int | Add of Exp * Exp | Sub of Exp * Exp | Minus of Exp | Abs of Exp
type Stack = int list
type InstructionList = Instruction list

let intpInstru (s:Stack)(i:Instruction) : Stack = 

    let add (xs: Stack) : Stack =
        match xs with 
        | [] -> []
        | head::tail when not tail.IsEmpty -> (head + tail.Head)::tail.Tail
        | _::tail when tail.IsEmpty -> failwith "tail was empty, only 1 value"
        | _ -> []

    let sub xs =
            match xs with 
            | [] -> []
            | head::tail when not tail.IsEmpty ->  (tail.Head - head)::tail.Tail
            | _::tail when tail.IsEmpty -> failwith "tail was empty, only 1 value"
            | _ -> failwith "could not split"
    let abs xs =
            match xs with
            | [] -> []
            | head::tail when head < 0 -> -head::tail
            | noChange -> noChange

    let sign xs =
            match xs with
            | [] -> []
            | head::tail -> -head::tail                

    let interpret =
        function
        | ADD -> add s
        | PUSH n -> n::s 
        | SIGN -> sign s
        | ABS -> abs s
        | SUB -> sub s

    interpret i

let rec sem (e:Exp) (x:int) : int = 
    match e with
        | X -> x 
        | C i -> i
        | Minus t -> -(sem t x)
        | Add(a,b) -> sem a x + sem b x
        | Sub(a,b) -> sem a x - sem b x
         | Abs a when sem a x < 0 -> - (sem a x)
        | Abs a ->  (sem a x)

let testExp = Minus(Abs(Sub(C 2, Add(C 5, C 10)))) 
let testExp2 = Abs(X)
sem (Minus(C 5)) 5

let compile (exp:Exp) (i:int) : (Instruction list) = 
        
        let rec ToInstruction =
            function
            | C c -> [PUSH c];
            | Add (a,b) -> ToInstruction a @ ToInstruction b @ [ADD]
            | Sub (a,b) -> ToInstruction a @ ToInstruction b @ [SUB]
            | Abs a -> ToInstruction a @ [ABS]
            | Minus a -> ToInstruction a @ [SIGN]
            | X -> []

        ToInstruction exp

// minus(abs(2-(5+10))) => -13

compile testExp2 5
   

let exec (i:InstructionList) : int = (List.fold(fun a -> intpInstru a) [] i).Head

let testStack = [PUSH 3; PUSH 7; ADD; PUSH 4; PUSH 5; SUB; ADD]

let testExecution = 
    printfn "%i" (exec testStack)
    printfn "%i" (exec (compile testExp -13))

        