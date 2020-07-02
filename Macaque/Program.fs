namespace Macaque

open System

module Main = 
    
    [<EntryPoint>]
    let main argv =
        printfn "Hello World from F#!"
        printfn "%s" (Tokens.lookupIdent("fn"))
        0 // return an integer exit code
