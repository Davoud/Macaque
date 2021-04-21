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
           
    let program = Parser(Lexer input).ParseProgram()

    program.IsSome |> should equal true
    program.Value.Statements.Length |> should equal 3

    let expectedIdentifiers = [| "x"; "y"; "foobar" |]
    for i in 0 .. expectedIdentifiers.Length - 1 do
        this.TestLetStatement program.Value.Statements.[i] expectedIdentifiers.[i]

  //[<Test>]
  member this.TestLetStatementErrors() =
    let input = 
        "let 5;
         let y 10;
         let = 12;"

    let parser = Parser(Lexer input)
    let program = parser.ParseProgram()
    parser.Errors.Length |> should equal 3
    parser.Errors.[0] |> should equal $"expected next token to be IDENT, got INT instead!"
    parser.Errors.[1] |> should equal $"expected next token to be ASSIGN, got INT instead!"
    parser.Errors.[2] |> should equal $"expected next token to be IDENT, got ASSIGN instead!"
    
  [<Test>]
  member this.TestReturnStatement() =
    let input = 
        "return 5;
         return 10;
         return 99912;"

    let parser = Parser(Lexer input)
    let program = parser.ParseProgram()

    program.IsSome |> should equal true
    program.Value.Statements.Length |> should equal 3

    for statement in program.Value.Statements do
        statement |> should be instanceOfType<ReturnStatement>
        statement.TokenLiteral() |> should equal "return"
        
  [<Test>]
  member this.TestStrings() =
    let letStatement = 
        LetStatement(
            Token(TokenType.LET, "let"), 
            Identifier(Token(TokenType.IDENT, "myVar"), "myVar"),
            Some(Identifier(Token(TokenType.IDENT, "anotherVar"), "anotherVar") :> Expression))
    
    (letStatement :> Statement).String() |> should equal "let myVar = anotherVar;"
   
  [<Test>]
  member this.TestIdentifierExpression() =
      let input = "foobar;"
      let p = Parser(Lexer input)
      let program = p.ParseProgram()
      program.IsSome |> should equal true
      program.Value.Statements.Length |> should equal 1
      
      let stmt = program.Value.Statements.Head
      stmt |> should be instanceOfType<ExpressionStatement>
      
      let ident = (stmt :?> ExpressionStatement).Expression
      ident |> should be instanceOfType<Identifier>
      ident.TokenLiteral() |> should equal "foobar"
      (ident :?> Identifier).Value |> should equal "foobar"

  [<Test>]
  member this.TestIntegerLiteralExpression() =
    let input = "5;"
    let p = Parser(Lexer input)
    let program = p.ParseProgram()
    program.IsSome |> should equal true
    program.Value.Statements.Length |> should equal 1

    let stmt = program.Value.Statements.Head
    stmt |> should be instanceOfType<ExpressionStatement>

    let literal = (stmt :?> ExpressionStatement).Expression
    literal |> should be instanceOfType<IntegerLiteral>

    (literal :?> IntegerLiteral).Value |> should equal 5
    literal.TokenLiteral() |> should equal "5"

 
