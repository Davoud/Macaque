namespace Macaque.Tests

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.Tokens


 type ExpectedToken = { Type: TokenType; Literal: string }    
    
 [<TestFixture>]    
 type Tests() =
    
    [<Test>]
    member this.TestNextToken() =
        let input = ",=+();"
        let tests = [|
            { Type = COMMA; Literal = "," };
            { Type = ASSIGN; Literal = "=" };    
            { Type = PLUS; Literal = "+" };
            { Type = LPAREN; Literal = "(" };
            { Type = RPAREN; Literal = ")" };
            { Type = SEMICOLON; Literal = ";" };
        |]
        
        let lexer = Lexer(input)
        
        tests |> Array.map(fun expected ->             
            let tok = lexer.NextToken()            
            tok.Type |> should equal expected.Type
            tok.Literal |> should equal expected.Literal
        ) |> ignore            
        
        
    


