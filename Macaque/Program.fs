namespace Macaque

open System
open Tokens


module Main = 
     
    
            

    [<EntryPoint>]
    let main argv =
        let lex = Lexer("+=()")
        
        printfn "%A" (lex.NextToken())
        printfn "%A" (lex.NextToken())
        printfn "%A" (lex.NextToken())
       

        0 // return an integer exit code

    