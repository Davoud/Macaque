namespace Macaque.Tests

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.LexerOp

 type ExpectedToken = { expectedType: string; expectedLiteral: string }    
    
 [<TestFixture>]    
 type Tests() =
    
    [<Test>]
    member this.TestNextToken() =
        let input = "=+(),;"
        let tests = [|
            { expectedType = Tokens.ASSIGN; expectedLiteral = "=" };    
            { expectedType = Tokens.PLUS; expectedLiteral = "+" };
            { expectedType = Tokens.LPAREN; expectedLiteral = "(" };
            { expectedType = Tokens.RPAREN; expectedLiteral = ")" };
            { expectedType = Tokens.COMMA; expectedLiteral = "," };
            { expectedType = Tokens.SEMICOLON; expectedLiteral = ";" };
        |]
        
        let lexer = Lexer.New(input)
        
        tests |> Array.map(fun test -> 
            let tok = lexer.NextToken()
            tok.Type |> should equal test.expectedType 
            tok.Literal |> should equal test.expectedLiteral
        ) |> ignore            
        
        
    


