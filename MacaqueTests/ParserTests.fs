namespace Macaque.Tests.Parser

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.Tokens
open Macaque.Ast
open Macaque.Parsing

 [<TestFixture>]    
 type Tests() =
  
  let parse (input: string) (expectedNumStatments: int): Program =
    let program = Parser(Lexer input).ParseProgram()
    program.IsSome |> should equal true
    program.Value.Statements.Length |> should equal expectedNumStatments
    program.Value

  let parseSingle input = parse input 1

  member _.asInstanceOf<'T>(o: obj): 'T =
    o |> should be instanceOfType<'T>
    o :?> 'T
  
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
           
    let program = parse input 3    
    let expectedIdentifiers = [| "x"; "y"; "foobar" |]
    for i in 0 .. expectedIdentifiers.Length - 1 do
        this.TestLetStatement program.Statements.[i] expectedIdentifiers.[i]

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
         
    for statement in  (parse input 3).Statements do
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
      let stmt = this.asInstanceOf<ExpressionStatement> (parseSingle "foobar;").Statements.Head
      stmt.Expression.TokenLiteral() |> should equal "foobar"
      let ident = this.asInstanceOf<Identifier> stmt.Expression      
      ident.Value |> should equal "foobar"

  [<Test>]
  member this.TestIntegerLiteralExpression() =    
    let stmt = this.asInstanceOf<ExpressionStatement> (parseSingle "5;").Statements.Head
    let literal = this.asInstanceOf<IntegerLiteral>(stmt.Expression)    
    literal.Value |> should equal 5
    stmt.Expression.TokenLiteral() |> should equal "5"

  [<Test>] 
  member this.TestParsingPrefixExpressions() =   
   ["!5;", "!", 5; "-15;", "-", 15] |> List.iter (fun (input, operator, integerValue) ->         
        let stmt = this.asInstanceOf<ExpressionStatement> (parseSingle input).Statements.Head
        let exp = this.asInstanceOf<PrefixExpression>(stmt.Expression)
        exp.Opertor |> should equal operator                
        this.testIntegerLiteral exp.Right integerValue)
   
  member this.testIntegerLiteral (il: Expression) (value: int): unit =
    let integ = this.asInstanceOf<IntegerLiteral>()
    integ.Value |> should equal value
    il.TokenLiteral() |> should equal (sprintf "%i" value)
    
    
    