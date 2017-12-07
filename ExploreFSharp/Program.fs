open System
open System.Collections

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// create an active pattern
let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None
let (|Long|_|) str =
   match System.Int64.TryParse(str) with
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
    | Some v -> printfn "Phonenumber for %s = %i" name v.PhoneNumber
    | None -> printfn "Phonenumber for %s not found!!!!" name

type FibSolver() = 
    static member fib_a (n:int64,?_a:int64,?_b:int64) =
            let a = defaultArg _a 0L
            let b = defaultArg _b 1L
            match n with
            | 1L | 2L -> a+b
            | _ -> FibSolver.fib_a(n-1L,b, a+b)
    static member fib(n:int64) =
            match n with
            | 1L | 2L -> 1
            | _ -> FibSolver.fib(n-1L) + FibSolver.fib(n-2L)
    
let listSorter(aList:List<String>) =
     printfn "used list: ";
     printfn "%A" aList;
     List.sortBy(fun elem -> elem) aList;          
     


[<EntryPoint>]
// Contains:
// Bedingte Ausdruecke / Match expressions (Exercise 5 / SW 05) ==> EntryPoint / Unterscheidung Faelle
// Structures / Modules (Exercise 8 / SW 05) ==> type PhoneBookEntry (1.)
// Lokale Definitionen und Lexikalisches Scoping (Exercise 2 / SW 07) ==> (2., 3.)
// Funktionen mit Gedaechtnis (Exercise 7 / SW 07)  ==> (2., 3.)
// Funktionen hoeherer Ordnung (Exercise 8 / SW 06) ==> (4.)

let rec main(argv) = 
    Console.WriteLine("\r\nWelcome to PCP F# Demo,");
    Console.WriteLine("1. Phone Book Demo (Exercise 7 / SW 07)");
    Console.WriteLine("2. Fibonacci with Akkumulator (Exercise 2 / SW 07)");
    Console.WriteLine("3. Fibonacci without Akkumulator (Excercise 2 / SW 07)");
    Console.WriteLine("4. Sort List (Exercise 8 / SW 05)");
    Console.WriteLine("q for Quit");
    match Console.ReadKey().KeyChar.ToString()  with
    | Int i -> match i with
               | 1 -> Console.WriteLine()
                      findPhoneNumber "Adam" |> ignore
                      findPhoneNumber "Erna" |> ignore
                      addEntry (new PhoneBookEntry("Erna",4715))
                      findPhoneNumber "Erna" |> ignore
                      main(argv)
               | 2 -> Console.WriteLine()
                      Console.WriteLine("Enter THE number: ");
                      let sn = Console.ReadLine();
                      match sn with
                      | Long n -> let result = FibSolver.fib_a(n)
                                  printfn "Result: %i" result
                                  main(argv)
                      | _ -> main(argv)
               | 3 -> Console.WriteLine()
                      Console.WriteLine("Enter THE number: ");
                      let sn = Console.ReadLine();
                      match sn with
                      | Long n -> let result = FibSolver.fib n
                                  printfn "Result: %i" result
                                  main(argv)
                      | _ -> main(argv)
               | 4 -> Console.WriteLine()
                      let result = listSorter(["b"; "a"; "y"; "x"; "q"; "m"; "c"; "d"]);  
                      printfn "sorted list: ";
                      printfn "%A" result
                      main(argv)
               | _ -> main(argv)
    | "Q" | "q" -> 0
    | _ -> main(argv)