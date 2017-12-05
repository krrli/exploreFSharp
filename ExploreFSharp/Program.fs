open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// create an active pattern
let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

type PhoneBookEntry(nam:string,nbr:int) =
    let mutable name = nam
    let mutable phonenumber = nbr

    override x.GetHashCode() = 
        hash(name,phonenumber)

    override x.Equals(b)=
        match b with
        | :? PhoneBookEntry as pbe -> (name,phonenumber) = (pbe.Name,pbe.PhoneNumber)
        | _ -> false

    member this.Name
        with get() = name
        and set(value) = name <- value
    member this.PhoneNumber 
        with get() = phonenumber
        and set(value) = phonenumber <- value

let mutable PhoneBook = [
    new PhoneBookEntry("Adam",4711);
    new PhoneBookEntry("Eva",4712)
    ]
let addEntry(newEntry:PhoneBookEntry) = 
    PhoneBook <- newEntry::PhoneBook

let findPhoneNumber(name:string) =
    let isEquals x (y:PhoneBookEntry) = x = y.Name
    let retval = List.tryFind (isEquals name) PhoneBook
    match retval with
    | Some v -> printfn "%i" v.PhoneNumber
    | None -> printfn "#false"

type FibSolver() = 
    static member fib_a (n:int,?_a:int,?_b:int) =
            let a = defaultArg _a 0
            let b = defaultArg _b 1
            match n with
            | 1 | 2 -> b
            | _ -> FibSolver.fib_a(n-1,b, a+b)
    static member fib(n:int) =
            match n with
            | 1 | 2 -> n
            | _ -> FibSolver.fib(n-1) + FibSolver.fib(n-2)
    
let listMatcher(aList:List<String>) =
     match aList with
     | [] -> printfn "the list is empty"
     | [first] -> printfn "the list has one element %A " first
     | [first; second] -> printfn "list is %A and %A" first second
     | _ -> printfn "the list has more than two elements"
  


[<EntryPoint>]
// Contains:
// Bedingte Ausdruecke / Match expressions (Exercise 5 / SW 05) ==> EntryPoint / Unterscheidung Faelle
// Structures / Modules (Exercise 8 / SW 05) ==> type PhoneBookEntry (1.)
// Lokale Definitionen und Lexikalisches Scoping (Exercise 2 / SW 07) ==> (2., 3.)
// Funktionen mit Gedaechtnis (Exercise 7 / SW 07)  ==> (2., 3.)
// Funktionen hoeherer Ordnung (Exercise 6 / SW 06) ==> (4.)
// oder
// Funktionen hoeherer Ordnung (Exercise 6 / SW 08) ==> (4.)

let main argv = 
    Console.WriteLine("Welcome to PCP F# Demo,");
    Console.WriteLine("1. Phone Book Demo (Exercise 7 / SW 07)");
    Console.WriteLine("2. Fibonacci with Akkumulator (Exercise 2 / SW 07)");
    Console.WriteLine("3. Fibonacci without Akkumulator (Excercise 2 / SW 07)");
    Console.WriteLine("4. Structures / Modules (Exercise 8 / SW 05)");
    match Console.ReadLine() with
    | Int i -> match i with
               | 1 -> findPhoneNumber "Adam" |> ignore
                      findPhoneNumber "Erna" |> ignore
                      addEntry (new PhoneBookEntry("Erna",4715))
                      findPhoneNumber "Erna" |> ignore
               | 2 -> let result = FibSolver.fib_a(21) 
                      printfn "%i" result
               | 3 -> let result = FibSolver.fib(19);
                      printfn "%i" result
               | 4 -> listMatcher(["a"; "b"; "c"; "d"])                      
               | _ -> ()
    | _ -> ()

   
    let s = Console.ReadLine()
    0 // return an integer exit code
