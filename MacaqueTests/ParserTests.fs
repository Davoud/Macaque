namespace Macaque.Tests.Parser

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.Tokens
open Macaque.Ast
open Macaque.Parsing

 [<TestFixture>]    
 type Tests() =
  
  let rec printErrors (erros: string list) =
    match erros with 
        | head :: tail -> 
            printfn "Error: %s" head
            printErrors tail
        | [] -> ()
              
  let parse (input: string) (expectedNumStatments: int): Program =
    let parser = Parser(Lexer input)
    let program = parser.ParseProgram()
    printErrors parser.Errors
    program.IsSome |> should equal true    
    if expectedNumStatments > 0 then program.Value.Statements.Length |> should equal expectedNumStatments    
    program.Value
  
  member _.asInstanceOf<'T>(o: obj): 'T =
    o |> should be instanceOfType<'T>
    o :?> 'T
  
  member this.asExpression<'T>(input: string): 'T =
    let stmt = (parse input 1).Statements.Head |> this.asInstanceOf<ExpressionStatement> 
    stmt.Expression |> this.asInstanceOf<'T>

  member this.TestLetStatement (s: Statement) (name: string) = 
    s.TokenLiteral() |> should equal "let"
    s |> should be instanceOfType<LetStatement>
    let letStmt = s :?> LetStatement
    letStmt.Name.Value |> should equal name
    (letStmt.Name :> Expression).TokenLiteral() |> should equal name

  member this.testIntegerLiteral (il: Expression) (value: int): unit =
    let integ = il |> this.asInstanceOf<IntegerLiteral>
    integ.Value |> should equal value
    il.TokenLiteral() |> should equal (sprintf "%i" value)
 
  member this.testIdentifer (il: Expression) (value: string): unit =
    let ident = il |> this.asInstanceOf<Identifier>
    ident.Value |> should equal value
    il.TokenLiteral() |> should equal value

  member this.testBoolean (il: Expression) (value: bool): unit =
    let boolExp = il |> this.asInstanceOf<BooleanExpression>
    boolExp.Value |> should equal value
    il.TokenLiteral() |> should equal $"{value}"

  member this.TestLiteralExperssion(exp: Expression) (expected: obj) =
    match expected with 
    | :? int as i -> this.testIntegerLiteral exp i
    | :? bool as b -> this.testBoolean exp b
    | :? int64 as v -> this.testIntegerLiteral exp (int v)
    | :? string as s -> this.testIdentifer exp s
    | _ -> failwith $"type of exp not handled. got {obj}"

  member this.TestInfixExpressions (exp: Expression) (left: obj) (oprator: string) (right: obj): unit =
    let infix = this.asInstanceOf<InfíxExpression> exp
    infix.Operator |> should equal oprator
    this.TestLiteralExperssion (infix.Left) left
    this.TestLiteralExperssion (infix.Right) right
    

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
      let stmt = this.asInstanceOf<ExpressionStatement> (parse "foobar;" 1).Statements.Head
      stmt.Expression.TokenLiteral() |> should equal "foobar"
      let ident = this.asInstanceOf<Identifier> stmt.Expression      
      ident.Value |> should equal "foobar"

  [<Test>]
  member this.TestIntegerLiteralExpression() =        
    let literal = this.asExpression<IntegerLiteral> "5;"    
    literal.Value |> should equal 5
    (literal :> Expression).TokenLiteral() |> should equal "5"

  [<Test>] 
  member this.TestParsingPrefixExpressions() =   
   [| "!5;", "!", 5; "-15;", "-", 15 |] |> Array.iter (fun (input, operator, integerValue) ->                 
        let exp =  this.asExpression<PrefixExpression> input
        exp.Operator |> should equal operator
        this.testIntegerLiteral exp.Right integerValue)
   
   [| "!true;", "!", true; "!false;", "!", false |] |> Array.iter (fun (input, operator, boolValue) -> 
        let exp =  this.asExpression<PrefixExpression> input
        exp.Operator |> should equal operator
        let right = this.asInstanceOf<BooleanExpression> exp.Right
        right.Value |> should equal boolValue)
       
  [<Test>]
  member this.TestBoolanExpression() =
    [| "true;", true, "true"; "false;", false, "false" |]
    |> Array.iter (fun (input, expected, literal) ->         
        let exp = this.asExpression<BooleanExpression> input
        exp.Value |> should equal expected
        (exp :> Expression).TokenLiteral() |> should equal literal)

  [<Test>]
  member this.TestParsingInfixExpressions() =
    [| 
       "5 + 5;",  5, "+",  5; 
       "5 - 5;",  5, "-",  5; 
       "5 * 5;",  5, "*",  5; 
       "5 / 5;",  5, "/",  5; 
       "5 > 5;",  5, ">",  5; 
       "5 < 5;",  5, "<",  5; 
       "5 == 5;", 5, "==", 5; 
       "5 != 5;", 5, "!=", 5;        
     |] 
     |> Array.iter (fun (input, leftValue, operator, rightValue) ->             
            let exp = this.asExpression<InfíxExpression> input
            this.testIntegerLiteral exp.Left leftValue
            exp.Operator |> should equal operator
            this.testIntegerLiteral exp.Right rightValue);
            
     [| 
        "true == true", true, "==", true;
        "true != false", true, "!=", false;
        "false == false", false, "==", false;
     |]
     |> Array.iter (fun (input, leftValue, operator, rightValue) -> 
            let exp = this.asExpression<InfíxExpression> input            
            exp.Operator |> should equal operator
            let left = this.asInstanceOf<BooleanExpression> exp.Left
            left.Value |> should equal leftValue
            let right = this.asInstanceOf<BooleanExpression> exp.Right
            right.Value |> should equal rightValue)

    
  [<Test>]
  member t.TestOperatorPrecedenceParsing() =        
        [| 
           "-a * b",    "((-a) * b)";
           
           "!-a",       "(!(-a))";
           
           "a + b + c", "((a + b) + c)";
           
           "a + b - c", "((a + b) - c)";
           
           "a * b * c", "((a * b) * c)";
           
           "a * b / c", "((a * b) / c)";
           
           "a + b / c", "(a + (b / c))";
           
           "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)";
           
           "3 + 4; -5 * 5", "(3 + 4)((-5) * 5)";
           
           "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))";
           
           "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))";
           
           "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))";
           
           "3 + 4 / 5 != 3 * 1 + 4 / 5", "((3 + (4 / 5)) != ((3 * 1) + (4 / 5)))";

           "true", "true";

           "false", "false";

           "3 > 5 == false", "((3 > 5) == false)";

           "3 < 5 == true", "((3 < 5) == true)";

           "(5 + 5) * 2", "((5 + 5) * 2)";

           "1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)";

           "2 / (5 + 5)", "(2 / (5 + 5))";

           "-(5 + 5)", "(-(5 + 5))";

           "!(true == true)", "(!(true == true))"; 
           
        |]
        |> Array.iter (fun (input, exprected) -> (parse input -1).String() |> should equal exprected)

  [<Test>]
  member t.TestIfExpression() =
      let exp = t.asExpression<IfExpression> "if (x < y) { x }"
      t.TestInfixExpressions (exp.Condition) "x" "<" "y"
      exp.Consequence.Statements |> Seq.length |> should equal 1
      let consequence = t.asInstanceOf<ExpressionStatement> exp.Consequence.Statements.Head
      t.testIdentifer (consequence.Expression) "x"
      exp.Alternative.IsNone |> should equal true

  [<Test>]
  member t.TestIfElseExpression() =
      let exp = t.asExpression<IfExpression> "if (x < y) { x } else { y }"
      t.TestInfixExpressions (exp.Condition) "x" "<" "y"
      exp.Consequence.Statements.Length |> should equal 1
      let consequence = t.asInstanceOf<ExpressionStatement> exp.Consequence.Statements.Head
      t.testIdentifer (consequence.Expression) "x"
      exp.Alternative.IsNone |> should equal false
      exp.Alternative.Value.Statements.Length |> should equal 1
      let alternative = t.asInstanceOf<ExpressionStatement> exp.Alternative.Value.Statements.Head
      t.testIdentifer (alternative.Expression) "y"

  [<Test>]
  member t.TestFunctionLiteralParsing() =
    let fn = t.asExpression<FunctionLiteral> "fn (x, y) { x + y; }"
    fn.Parameters.Length |> should equal 2
    "x" |> t.TestLiteralExperssion fn.Parameters.[0]
    "y" |> t.TestLiteralExperssion fn.Parameters.[1]
    fn.Body.Statements.Length |> should equal 1
    let bodyStmt = t.asInstanceOf<ExpressionStatement> fn.Body.Statements.Head
    t.TestInfixExpressions bodyStmt.Expression "x" "+" "y"
