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
       
    let program = Parser.New(Lexer input).ParseProgram()

    program.IsSome |> should equal true
    program.Value.Statements.Count |> should equal 3

    let expectedIdentifiers = [| "x"; "y"; "foobar" |]
    for i in 0 .. expectedIdentifiers.Length - 1 do
        this.TestLetStatement program.Value.Statements.[i] expectedIdentifiers.[i]
    
 
