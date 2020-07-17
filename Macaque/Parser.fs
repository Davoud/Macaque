namespace Macaque

open Macaque.Tokens
open Macaque.Ast

module Parsing =

    [<Struct>]
    type Parser = { 
        Lexer: Lexer; 
        CurToken: Token; 
        PeekToken: Token } 
          
    type Parser with

        static member New (lex:Lexer) = 
            let cur = lex.NextToken()
            let peek = lex.NextToken()
            { Lexer = lex; CurToken = cur; PeekToken = peek }

        member p.NextToken() = { Lexer = p.Lexer; CurToken = p.PeekToken; PeekToken = p.Lexer.NextToken() }               

        member p.ParseProgram(): Ast.Program option =             
            let program = Program()
            let rec parse (p: Parser) =
                if p.CurToken.Type <> EOF then                  
                    match p.ParseStatement() with | Some(s) -> program.Append s | None -> ()
                    parse (p.NextToken())            
            parse p

            Some(program)
        
        member p.ParseStatement(): Statement option = 
                   match p.CurToken.Type with
                   | LET -> (match p.ParseLetStatement() with | Some(s) -> Some(s :> Statement) | None -> None)
                   | _ -> None

        member p.ParseLetStatement(): LetStatement option = None
            
            
        
        
        



