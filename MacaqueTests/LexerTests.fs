﻿namespace Macaque.Tests.Lexer

open NUnit.Framework
open FsUnit
open Macaque
open Macaque.Tokens

    
 [<TestFixture>]    
 type Tests() =
    
    let input = 
        "let five = 5;             
         let ten = 10;
         let add = fn(x, y) {
            x + y;
         };
         let result = add(five, ten);

         !-/*5;
         5 < 10 > 5;
         
         if (5 < 10) {
            return true;
         } else {
            return false;
         }
         
         10 == 10;
         10 != 9;
         let str = \"string\";
         \"an string with 28 characters\" +
         [1, 2];
         {\"foo\": \"bar\"}"


    let tests = [|
           (LET, "let");   (IDENT, "five");    (ASSIGN, "=");  (INT, "5");     (SEMICOLON, ";");
           (LET, "let");   (IDENT, "ten");     (ASSIGN, "=");  (INT, "10");    (SEMICOLON, ";");
           (LET, "let");   (IDENT, "add");     (ASSIGN, "=");  (FUNCTION, "fn"); 
           (LPAREN, "(");  (IDENT, "x");       (COMMA, ",");   (IDENT, "y");   (RPAREN, ")");   (LBRACE, "{");
           (IDENT, "x");   (PLUS, "+");        (IDENT, "y");   (SEMICOLON, ";"); (RBRACE, "}"); (SEMICOLON, ";");
           (LET, "let");   (IDENT, "result");  (ASSIGN, "=");  (IDENT, "add"); 
           (LPAREN, "(");  (IDENT, "five");    (COMMA, ",");   (IDENT, "ten"); (RPAREN, ")");  (SEMICOLON, ";");
           (BANG, "!");    (MINUS, "-");       (SLASH, "/");   (ASTRISK, "*"); (INT, "5");     (SEMICOLON, ";");
           (INT, "5");     (LT, "<");          (INT, "10");    (GT, ">");      (INT, "5");     (SEMICOLON, ";");
           (IF, "if");     (LPAREN, "(");      (INT, "5");     (LT, "<");      (INT, "10");    (RPAREN, ")");
           (LBRACE, "{");  (RETURN, "return"); (TRUE, "true"); (SEMICOLON, ";");
           (RBRACE, "}");  (ELSE, "else");     (LBRACE, "{");  (RETURN, "return"); (FALSE, "false"); (SEMICOLON, ";"); 
           (RBRACE, "}");  (INT, "10");        (EQ, "==");     (INT, "10");    (SEMICOLON, ";");
           (INT, "10");    (NOT_EQ, "!=");     (INT, "9");     (SEMICOLON, ";");
           (LET, "let");   (IDENT, "str");     (ASSIGN, "=");  (STRING, "string"); (SEMICOLON, ";");
           (STRING, "an string with 28 characters");           (PLUS, "+");
           (LBRACKET, "["); (INT, "1");        (COMMA, ",");   (INT, "2");     (RBRACKET, "]"); (SEMICOLON, ";");
           (LBRACE, "{");  (STRING, "foo");    (COLON, ":");   (STRING, "bar");(RBRACE, "}");
           (EOF, "")
       |]

    [<Test>]
    member this.TestNextToken() =
        
        let lexer = Lexer(input)
                                 
        tests |> Array.iter (fun (tokenType, literal) ->
            let token = lexer.NextToken()            
            token.Type |> should equal tokenType 
            token.Literal |> should equal literal) 
        
    


