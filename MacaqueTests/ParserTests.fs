namespace Macaque.Tests.Parser

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.Tokens
open Macaque.Ast
open Macaque.Parsing

 [<TestFixture>]    
 type Tests() =
  
  let quote (s: string) = s.Replace('`', '"')

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
    if expectedNumStatments > 0 then program.Statements.Length |> should equal expectedNumStatments    
    program  

  let parseStr (input: string) (expectedNumStatments: int): string =
      let parser = Parser(Lexer input)
      let program = parser.ParseProgram()
      printErrors parser.Errors    
      if expectedNumStatments > 0 then program.Statements.Length |> should equal expectedNumStatments    
      sprintf "%O" program

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

  member this.testStringLiteral (il: Expression) (value: string): unit =
    let strExp = il |> this.asInstanceOf<StringLiteral>
    strExp.Value |> should equal value
    il.TokenLiteral() |> should equal value

  member this.TestLiteralExperssion(exp: Expression) (expected: obj) =
    match expected with 
    | :? int as i -> this.testIntegerLiteral exp i
    | :? bool as b -> this.testBoolean exp b
    | :? int64 as v -> this.testIntegerLiteral exp (int v)
    | :? string as s -> 
         match exp with 
         | :? Identifier -> this.testIdentifer exp s
         | :? StringLiteral -> this.testStringLiteral exp s
         | _ -> failwith $"type of exp not handled. got {obj}"
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

    let program = parse "let x = 5 * 4;" 1
    let stmt = this.asInstanceOf<LetStatement> program.Statements.Head
    stmt.Name.Value |> should equal "x"   
    this.TestInfixExpressions stmt.Value 5 "*" 4

    let pstr = parse @"let x = ""das gefällt mir"" + ""sehr gut""" 1
    let strStmt = this.asInstanceOf<LetStatement> pstr.Statements.Head
    strStmt.Name.Value |> should equal "x"
    this.TestInfixExpressions strStmt.Value "das gefällt mir" "+" "sehr gut"



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
            Identifier(Token(TokenType.IDENT, "anotherVar"), "anotherVar") :> Expression)
    
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
            
            "a + add(b * c) + d", "((a + add((b * c))) + d)";
            
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))";
            
            "add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))";

            "a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)";

            "add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))";
         |]
         |> Array.iter (fun (input, exprected) -> (parseStr input -1) |> should equal exprected)
                       

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
  
  [<Test>]
  member t.TestFunctionParameterParsing() =
    [| 
        "fn() {};", [];
        "fn(x) {};", ["x"];
        "fn(x, y, z) {};", ["x"; "y"; "z"];
    |]
    |> Array.iter (fun (input, expected) -> 
         let fn = t.asExpression<FunctionLiteral> input
         fn.Parameters.Length |> should equal expected.Length
         for i in 0 .. expected.Length - 1 do
            t.TestLiteralExperssion fn.Parameters.[i] expected.[i])

  [<Test>]
  member t.TestCallExpressionParsing() =
    let exp = t.asExpression<CallExpression> "add(1, 2 * 3, 4 + 5);"
    t.testIdentifer (exp.Function) "add"
    exp.Arguments.Length |> should equal 3
    t.TestLiteralExperssion exp.Arguments.[0] 1
    t.TestInfixExpressions exp.Arguments.[1] 2 "*" 3
    t.TestInfixExpressions exp.Arguments.[2] 4 "+" 5
    
  [<Test>]
  member t.TestStringLiteral() =
    let input = "\"an string example\""
    let exp = t.asExpression<StringLiteral> input
    exp.Token.Type |> should equal STRING
    exp.Value |> should equal "an string example"

  [<Test>]
  member t.TestArrayLiterals() =    
    let arr = t.asExpression<ArrayLiteral> "[1, 2 * 2, 3 + 3]"
    arr.Elements.Length |> should equal 3
    t.testIntegerLiteral   arr.Elements.[0] 1
    t.TestInfixExpressions arr.Elements.[1] 2 "*" 2
    t.TestInfixExpressions arr.Elements.[2] 3 "+" 3
  
  [<Test>]
  member t.TestParsingIndexExpression() =
    let indexExp = t.asExpression<IndexExpression> "myArray[1 + 1]"
    t.testIdentifer indexExp.Left "myArray"
    t.TestInfixExpressions indexExp.Index 1 "+" 1

  [<Test>]
  member t.TestParsingHashLiteralsStringKeys() =
    let hash = t.asExpression<HashLiteral> (quote "{`one`: 1, `two`: 2, `three`: 3}")
    hash.Pairs.Count |> should equal 3    
    let expected = Map [ "one", 1; "two", 2; "three", 3]
    for KeyValue(key, value) in hash.Pairs do
        let literal = t.asInstanceOf<StringLiteral> key
        match expected.TryFind literal.Value with
        | Some(expectedValue) -> t.testIntegerLiteral value expectedValue
        | None -> failwith (sprintf "No value for key %s found" literal.Value)
        
  [<Test>]
  member t.TestParsingEmptyHashLiteral() =
    let hash = t.asExpression<HashLiteral> "{}"
    hash.Pairs.Count |> should equal 0
   
  [<Test>]
  member t.TestParsingHashLiteralsWithExpressions() =
    let hash = t.asExpression<HashLiteral> (quote "{`one`: 0 + 1, `two`: 10 - 8, `three`: 15 / 5}")
    hash.Pairs.Count |> should equal 3
    let tests = Map [
        "one", fun(e: Expression) -> t.TestInfixExpressions e 0 "+" 1;
        "two", fun(e: Expression) -> t.TestInfixExpressions e 10 "-" 8;
        "three", fun(e: Expression) -> t.TestInfixExpressions e 15 "/" 5;]

    for KeyValue(key, value) in hash.Pairs do
        let literal = t.asInstanceOf<StringLiteral> key
        match tests.TryFind (literal.Value) with
        | Some(testFunc) -> testFunc value
        | None -> failwith (sprintf "No test function for key %s found" literal.Value)
    