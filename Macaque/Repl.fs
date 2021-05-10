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
        printfn "\n%s\n" ((eval program env).Inspect())
                   

    let rec nextLine() =
        match Console.ReadLine() with
        | line when line = ";;" -> 0      
        | line -> parse line   
                  Console.ResetColor() 
                  prompt()
                  nextLine()

    let start() = 
        printfn "Enter ;; to quit."       
        prompt()
        nextLine()        

    
    
    

