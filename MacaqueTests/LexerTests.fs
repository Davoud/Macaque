namespace Macaque.Tests

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.Tokens

    
 [<TestFixture>]    
 type Tests() =
    
    [<Test>]
    member this.TestNextToken() =
        let input = 
            "let five = 5;             
             let ten = 10;
             let add = fn(x, y) {
                x + y;
             };
             let result = add(five, ten);"

        let tests = [|
            (LET, "let"); (IDENT, "five"); (ASSIGN, "="); (INT, "5"); (SEMICOLON, ";");
            (LET, "let"); (IDENT, "ten"); (ASSIGN, "="); (INT, "10"); (SEMICOLON, ";");
            (LET, "let"); (IDENT, "add"); (ASSIGN, "="); (FUNCTION, "fn"); 
            (LPAREN, "("); (IDENT, "x"); (COMMA, ","); (IDENT, "y"); (RPAREN, ")"); (LBRACE, "{");
            (IDENT, "x"); (PLUS, "+"); (IDENT, "y"); (SEMICOLON, ";"); (RBRACE, "}"); (SEMICOLON, ";");
            (LET, "let"); (IDENT, "result"); (ASSIGN, "="); (IDENT, "add"); 
            (LPAREN, "("); (IDENT, "five"); (COMMA, ","); (IDENT, "ten"); (RPAREN, ")"); (SEMICOLON, ";");
        |]
         
       
        let lexer = Lexer(input)
                                 
        tests |> Array.map(fun (tokenType, literal) ->
            let token = lexer.NextToken()
            // printfn "Expected (%A %s) Got (%A)" tokenType literal token
            token.Type |> should equal tokenType 
            token.Literal |> should equal literal
        ) |> ignore        
    


