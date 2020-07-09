namespace Macaque.Repl

open System
open Macaque.Tokens
open Macaque

module Repl =
    let prompt = ">> "

    let rec interpret (lex:Lexer): unit =
        match lex.NextToken() with
        | tok when tok.Type <> EOF -> printfn "%A" tok
                                      interpret lex
        | _ -> printfn ""

    let rec next (line:string) =
        match line with
        | x when x = ";;" -> 0
        | x when x = "" -> 
            Console.Write prompt
            next (Console.ReadLine())

        | _ -> 
            interpret (Lexer line)
            Console.Write prompt
            next (Console.ReadLine())
            

    let start() = next ""
        

    
    
    

