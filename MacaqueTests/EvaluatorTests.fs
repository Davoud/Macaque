namespace Macaque.Tests.Eval

open NUnit.Framework
open FsUnit
open Macaque.Objects
open Macaque.Parsing
open Macaque.Evaluator
open Macaque
open Macaque.Ast


 [<TestFixture>]    
 type EvaluatorTests() =
               
    let quote (s: string) = s.Replace('`', '"')

    member _.asInstanceOf<'T>(o: obj): 'T = o |> should be instanceOfType<'T>; o :?> 'T

    member _.asInstanceOf<'T>(o: obj, id: int): 'T = 
        match o with
        | :? 'T as t -> t
        | _ -> failwith (sprintf "(%i) %O is not an instance of '%s'" id o typeof<'T>.Name)

    member t.testEval(input: string): Object = eval (Parser(Lexer input).ParseProgram() :> Node) (Environment())

    member t.testIntegerObject (obj: Object) (expected: int64) =      
        (t.asInstanceOf<Integer> obj).Value |> should equal expected

    member t.testIntegerObjectWithId (obj: Object) (expected: int64, id: int) =      
        ((t.asInstanceOf<Integer> (obj, id)).Value, id) |> should equal (expected, id)

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
            11, quote "{`name`: `Monkey`}[fn(x) { x }];", "unusable as hash key: FUNCTION"
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
            " `a` + `b` ", "ab";
            " `a` + ``  ", "a";
            " `` + `b`  ", "b";
            " `a` + `b` + `c`   ", "abc";
            " `a` + (`b` + `c`) ", "abc";
        |]
        |> Seq.iter (fun (input, expected) -> t.testStringObject (t.testEval (quote input)) expected)

        [|
            " `a` == `b`; ", false;
            " `a` == `a`; ", true;
            " `abc` != `abc`; ", false;
            " `abc` != `cba`; ", true;
        |]
        |> Seq.iter (fun (input, expected) -> t.testBooleanObject (t.testEval (quote input)) expected)

    

    [<Test>]
    member t.TestBuiltinFunction() =
        [|
            1, "len(``)", 0L :> obj;
            2, "len(`four`)", 4L :> obj;
            3, "len(`hello world`)", 11L :> obj;
            4, "len(1)", "argument to `len` not supported, got INTEGER" :> obj;
            5, "len(`one`, `two`)", "wrong number of arguments. got=2, want=1" :> obj;
            6, "len([1, 2, 4])", 3L :> obj;
            7, "len([])", 0L :> obj;
            8, "len([`one`])", 1L :> obj;
            9, "first([1, 2, 3])", 1L :> obj;
            10, "first([1000])", 1000L :> obj;
            11, "first([])", NULL :> obj; 
            12, "last([1, 2, 3])", 3L :> obj;
            13, "last([1000])", 1000L :> obj;
            14, "last([])", NULL :> obj;
        |]
        |> Seq.iter (fun (id, input, expected) -> 
            let evaluated = t.testEval (quote input)
            match expected with
            | :? int64 as i -> t.testIntegerObjectWithId evaluated (i, id)
            | :? string as errorMsg -> 
                let err = t.asInstanceOf<Error> (evaluated, id)
                err.Message |> should equal errorMsg
            | :? Object as object -> evaluated |> should equal expected
            | _ -> ())

        [|
            "rest([1, 2, 3])", "[2, 3]";
            "rest([1])", "[]";
            "rest([])", "[]";
            "push([], 1)", "[1]";
            "push([1], 2)", "[1, 2]";
            "push([1, 2, 3], 10)", "[1, 2, 3, 10]";
            "push([1, 2], [5])", "[1, 2, [5]]";
            "push([1])", "ERROR: wrong number of arguments. got=1, want=2";
            "push()", "ERROR: wrong number of arguments. got=0, want=2";
            "push(12, [13])", "ERROR: argument to `push` must be ARRAY, got INTEGER";
        |]
        |> Seq.iter (fun (input, expected) -> (t.testEval input).Inspect() |> should equal expected)

    [<Test>]
    member t.TestArrayLiterals() =        
        let result = t.asInstanceOf<Array> (t.testEval "[1, 2 * 2, 3 + 3]")
        result.Elements.Length |> should equal 3
        t.testIntegerObject result.Elements.[0] 1L
        t.testIntegerObject result.Elements.[1] 4L
        t.testIntegerObject result.Elements.[2] 6L

    [<Test>]
    member t.TestArrayIndexExpressions() =
        [|
            1, "[1, 2, 3][0]", 1L;
            2, "[1, 2, 3][1]", 2L;
            3, "[1, 2, 3][2]", 3L;
            4, "let i = 0; [1][i];", 1L;
            5, "[1, 2, 3][1 + 1];", 3L;
            6, "let myArray = [1, 2, 3]; myArray[2];", 3L;
            7, "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", 6L;
            8, "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2L;            
        |]
        |>
        Seq.iter (fun (id, input, exptected) -> t.testIntegerObjectWithId (t.testEval input) (exptected, id))

        [|
            "[1, 2, 3][3]", NULL;
            "[1, 2, 3][-1]", NULL;
        |]
        |> Seq.iter (fun (input, expected) -> (t.testEval input) |> should equal expected)

    [<Test>]
    member t.TestHashLiterals() =
        let input = "
            let two = `two`;
            {
                `one`: 10 - 9,
                 two: 1 + 1,
                 `thr` + `ee`: 6 / 2,
                 4: 4,
                 true: 5,
                  false: 6
            }"

        let evaluated = t.testEval (quote input)
        let result = t.asInstanceOf<Hash> evaluated

        let expected = Map [
            (``String``("one")   :> Hashable).HashKey, 1L;
            (``String``("two")   :> Hashable).HashKey, 2L;
            (``String``("three") :> Hashable).HashKey, 3L;
            (Integer(4L)         :> Hashable).HashKey, 4L;
            (TRUE  :?> Boolean   :> Hashable).HashKey, 5L;
            (FALSE :?> Boolean   :> Hashable).HashKey, 6L]

        result.Pairs.Count |> should equal expected.Count
       
        expected |> Map.iter (fun key value -> 
            match result.Pairs.TryFind key with
            | Some(pair) -> t.testIntegerObject pair.value value
            | None -> failwith "no pair for given key in Pairs")

    [<Test>]
    member t.TestHashIndexExpression() =
        let value = 5L :> obj;
        let nil = NULL :> obj;
        [|
            1, "{`foo`: 5}[`foo`]",  value;
            2, "{`foo`: 5}[`bar`]",  nil;
            3, "let key = `foo`; {`foo`: 5}[key]", value;
            4, "{}[`foo`]", nil;
            5, "{5: 5}[5]", value;
            6, "{true: 5}[true]", value;
            7, "{false: 5}[false]", value;
        |]
        |> Seq.iter (fun (id, input, expected) -> 
            let evaluted = t.testEval (quote input)
            match expected with
            | :? int64 as v -> t.testIntegerObjectWithId evaluted (v, id)
            | _ -> (evaluted,id) |> should equal (nil,id))