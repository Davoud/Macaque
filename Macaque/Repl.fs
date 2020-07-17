namespace Macaque.Repl

open System
open Macaque.Tokens
open Macaque

module Repl =
    let prompt = ">> "
    
    let printTokens (lex:Lexer) = 
        for token in lex.IterateOver() do
            printfn "%A" token
    
    let rec nextLine (readline:unit -> string) =
        match readline() with
        | line when line = ";;" -> 0      
        | line -> printTokens (Lexer line) 
                  printf "%s" prompt
                  nextLine (readline)

    let start() = 
        printf "%s" prompt
        nextLine Console.ReadLine
        

    
    
    

