namespace Macaque.Tests.Parser

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.Tokens
open Macaque.Ast
open Macaque.Parsing

 [<TestFixture>]    
 type Tests() =
 
  member this.TestLetStatement (s: Statement) (name: string) = 
    s.TokenLiteral() |> should equal "let"
    s |> should be instanceOfType<LetStatement>
    let letStmt = s :?> LetStatement
    letStmt.Name.Value |> should equal name
    (letStmt.Name :> Expression).TokenLiteral() |> should equal name
 
  [<Test>]
  member this.TestLetStatements() =
    let input = 
        "let x = 5;
         let y = 10;
         let foobar = 838383;"
           
    let program = Parser2(Lexer input).ParseProgram()

    program.IsSome |> should equal true
    program.Value.Statements.Count |> should equal 3

    let expectedIdentifiers = [| "x"; "y"; "foobar" |]
    for i in 0 .. expectedIdentifiers.Length - 1 do
        this.TestLetStatement program.Value.Statements.[i] expectedIdentifiers.[i]

  [<Test>]
  member this.TestLetStatementErrors() =
    let input = 
        "let 5;
         let y 10;
         let = 12;"

    let parser = Parser2(Lexer input)
    let program = parser.ParseProgram()
    parser.Errors.Count |> should equal 3
    parser.Errors.[0] |> should equal $"expected next token to be IDENT, got INT instead!"
    parser.Errors.[1] |> should equal $"expected next token to be ASSIGN, got INT instead!"
    parser.Errors.[2] |> should equal $"expected next token to be IDENT, got ASSIGN instead!"
    
  [<Test>]
  member this.TestReturnStatement() =
    let input = 
        "return 5;
         return 10;
         return 99912;"

    let parser = Parser2(Lexer input)
    let program = parser.ParseProgram()

    program.IsSome |> should equal true
    program.Value.Statements.Count |> should equal 3

    for statement in program.Value.Statements do
        statement |> should be instanceOfType<ReturnStatement>
        statement.TokenLiteral() |> should equal "return"
        

 
