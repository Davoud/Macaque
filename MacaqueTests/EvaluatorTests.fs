﻿namespace Macaque.Tests.Eval

open NUnit.Framework
open FsUnit
open Macaque.Objects
open Macaque.Parsing
open Macaque.Evaluator
open Macaque
open Macaque.Ast

 [<TestFixture>]    
 type EvaluatorTests() =
        
    member _.asInstanceOf<'T>(o: obj): 'T = o |> should be instanceOfType<'T>; o :?> 'T

    member _.asInstanceOf<'T>(o: obj, id: int): 'T = 
        match o with
        | :? 'T as t -> t
        | _ -> failwith (sprintf "(%i) %O is not an instance of '%s'" id o typeof<'T>.Name)

    member t.testEval(input: string): Object = eval (Parser(Lexer input).ParseProgram() :> Node) (Environment())

    member t.testIntegerObject (obj: Object) (expected: int64) =      
        (t.asInstanceOf<Integer> obj).Value |> should equal expected

    member t.testIntegerObjectWithId (obj: Object) (expected: int64, id: int) =      
        ((t.asInstanceOf<Integer> obj).Value, id) |> should equal (expected, id)

    member t.testBooleanObject (obj: Object) (expected: bool) =
        (t.asInstanceOf<Boolean> obj).Value |> should equal expected

    member t.testStringObject (obj: Object) (expected: string) =
        (t.asInstanceOf<``String``> obj).Value |> should equal expected

    [<Test>]
    member t.TestEvalIntegerExpression() =
        [| 
            "5", 5L; 
            "10", 10L;
            "-5", -5L;
            "-10", -10L;
            "5 + 5 + 5 + 5 - 10", 10L;
            "2 * 2 * 2 * 2 * 2", 32L;
            "-50 + 100 + -50", 0L;
            "5 * 2 + 10", 20L;
            "5 + 2 * 10", 25L;
            "20 + 2 * -10", 0L;
            "50 / 2 * 2 + 10", 60L;
            "2 * (5 + 10)", 30L;
            "3 * 3 * 3 + 10", 37L;
            "3 * (3 * 3) + 10", 37L;
            "(5 + 10 * 2 + 15 / 3) * 2 + -10", 50L;
        |] 
        |> Seq.iter (fun (input, expected) -> t.testIntegerObject (t.testEval input) expected)            

    [<Test>]        
    member t.TestEvalBooleanExpression() =
        [| "true", true; "false", false |] 
        |> Seq.iter (fun (input, expected) -> t.testBooleanObject (t.testEval input) expected)              

    [<Test>]
    member t.TestBooleanOperator() =
        [| 
            "!true", false; 
            "!false", true;
            "!5", false;
            "!!true", true;
            "!!false", false;
            "!!5", true;
            "!!!true", false;
            "1 < 2", true;
            "1 > 2", false;
            "1 < 1", false;
            "1 > 1", false;
            "1 == 1", true;
            "1 != 1", false;
            "1 == 2", false;
            "1 != 2", true;
            "true == true", true;
            "false == false", true;
            "true == false", false;
            "true != false", true;
            "false != true", true;
            "(1 < 2) == true", true;
            "(1 < 2) == false", false;
            "(1 > 2) == true", false;
            "(1 > 2) == false", true;            
        |]
        |> Seq.iter (fun (input, expected) -> t.testBooleanObject (t.testEval input) expected) 
            
    [<Test>]
    member t.TestIfElseExpression() =        
        [| 
            "if (true) { 10 }", 10L;
            "if (1) { 10 }", 10L;
            "if (1 < 2) { 10 }", 10L;
            "if (1 > 2) { 10 } else { 20 }", 20L;
            "if (1 < 2) { 10 } else { 20 }", 10L;
        |]
        |> Seq.iter (fun (input, expected) -> t.testIntegerObject (t.testEval input) expected)            
        
        [|
            "if (false) { 10 }", NULL;            
            "if (1 > 2) { 10 }", NULL;
            "if (1 != 0) { false }", FALSE;
            "if (2 == 2) { true }", TRUE;
            "if (2 + 4 == 2 * 3) { 2 > 0 } else { 2 < 0 }", TRUE;
            "if (2 + 4 != 2 * 3) { 2 > 0 } else { 2 < 0 }", FALSE;
        |]
        |> Seq.iter (fun (input, expected) -> (t.testEval input) |> should equal expected)
            
    [<Test>]                    
    member t.TestReturnStatement() =
        [| 
            "return 10;", 10L;
            "return 10; 9;", 10L;
            "return 2 * 5; 9;", 10L;
            "9; return 2 * 5; 9;", 10L;
            "if (10 > 1) {
              if (10 > 1) {
                return 10;
              }
              return 1;
            }", 10L
        |]
        |> Seq.iter (fun (input, expectd) -> t.testIntegerObject (t.testEval input) expectd)

    [<Test>]
    member t.TestErrorHandling() =
        [|
            1, "5 + true;", "type mismatch: INTEGER + BOOLEAN";            
            2, "5 + true; 5;", "type mismatch: INTEGER + BOOLEAN";
            3, "-true", "unknown operator: -BOOLEAN";            
            4, "true + false;", "unknown operator: BOOLEAN + BOOLEAN";            
            5, "5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN";               
            6, "if (10 > 1)  true + false; }", "unknown operator: BOOLEAN + BOOLEAN";            
            7, "if (10 > 1) { if (10 > 1) { return true + false; }return 1; }", "unknown operator: BOOLEAN + BOOLEAN"; 
            8, "foobar", "identifier not found: foobar";
            9, "5 + \"abc\"", "type mismatch: INTEGER + STRING";
            10, """ -"abc" """, "unknown operator: -STRING";
        |]
        |> Seq.iter (fun (id, input, expectedMessage) -> 
            let evaluated = input |> t.testEval
            let errObj = t.asInstanceOf<Error> (evaluated, id)
            errObj.Message |> should equal expectedMessage)

    [<Test>]
    member t.TestLetStatement() =
        [|
            "let a = 5; a;", 5L;
            "let a = 5 * 5; a;", 25L;
            "let a = 5; let b = a; b;", 5L;
            "let a = 5; let b = a; let c = a + b + 5; c;", 15L;
        |]
        |> Seq.iter (fun (input, expectd) -> t.testIntegerObject (t.testEval input) expectd)

    [<Test>]
    member t.TestFunctionObject() =
        let evaluated = t.testEval "fn(x) { x + 2 };"
        let fn = t.asInstanceOf<Function> evaluated
        fn.Parameters.Length |> should equal 1
        (fn.Parameters.Head :> Expression).String() |> should equal "x"
        (fn.Body :> Statement).String() |> should equal "(x + 2)"

    [<Test>]
    member t.TestFunctionApplication() = 
        [| 
            1, "let identity = fn(x) { x; }; identity(5);", 5L;
            2, "let identity = fn(x) { return x; }; identity(5);", 5L;
            3, "let double = fn(x) { x * 2; }; double(5);", 10L;
            4, "let add = fn(x, y) { x + y; }; add(5, 5);", 10L;
            5, "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20L;
            6, "fn(x) { x; }(5)", 5L;
        |]
        |> Seq.iter (fun (id, input, expected) -> t.testIntegerObjectWithId (t.testEval input) (expected, id))

    [<Test>]
    member t.TestClosures() =
        let input = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);"
        t.testIntegerObject (t.testEval input) 4L

    [<Test>]
    member t.TestFunctionAsParam() =
        let input = "
            let let add = fn(a, b) { a + b };
            let applyFunc = fn(a, b, func) { func(a, b) };
            applyFunc(2, 2, add);"
        
        t.testIntegerObject (t.testEval input) 4L

    [<Test>]
    member t.TestStringExpressions() =
        [| 
            """ "a" + "b" """, "ab";
            """ "a" + "" """, "a";
            """ "" + "b" """, "b";
            """ "a" + "b" + "c" """, "abc";
            """ "a" + ("b" + "c") """, "abc";
        |]
        |> Seq.iter (fun (input, expected) -> t.testStringObject (t.testEval input) expected)

        [|
            """ "a" == "b"; """, false;
            """ "a" == "a"; """, true;
            """ "abc" != "abc"; """, false;
            """ "abc" != "cba"; """, true;
        |]
        |> Seq.iter (fun (input, expected) -> t.testBooleanObject (t.testEval input) expected)