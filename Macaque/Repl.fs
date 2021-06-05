namespace Macaque.Repl

open System
open Macaque
open Macaque.Parsing
open Macaque.Evaluator


module Repl =
    
    let prompt() = printf ">> "      

    let env = Objects.Environment()

    let printTokens (lex:Lexer) = 
        for token in lex.IterateOver() do
            printfn "%A" token
    
    let rec printParserErrors (errors: string list) = 
        match errors with 
        | head :: tail -> 
            printfn " - %s" head
            printParserErrors tail
        | [] ->()

    let parse (input: string) =
        let parser = Parser(Lexer input)
        let program = parser.ParseProgram()
        
        if not parser.Errors.IsEmpty then
            Console.ForegroundColor <- ConsoleColor.Blue
            printfn "\nThere were some parsing error: "
            printParserErrors parser.Errors            
            printfn ""
              
        Console.ForegroundColor <- ConsoleColor.Yellow
        (eval program env) |> (inspect >> printfn "\n%s\n") 
                                   
    let load fileName = 
        try 
            IO.File.ReadAllText fileName
        with
            | exp -> 
                Console.ForegroundColor <- ConsoleColor.Red
                printfn "%A" exp
                $"\"{exp.Message}\""


    let rec nextLine() =
        match Console.ReadLine() with
        | line when line = ";;" -> 0  
        | line when line.StartsWith "#load " -> 
                  line.Substring 6 |> load |> parse
                  Console.ResetColor() 
                  prompt()
                  nextLine()

        | line -> parse line   
                  Console.ResetColor() 
                  prompt()
                  nextLine()

    let start() = 
        printfn "Enter ;; to quit."       
        prompt()
        nextLine()        

    
    
    

