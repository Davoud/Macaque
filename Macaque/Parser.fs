namespace Macaque

open Macaque.Tokens
open Macaque.Ast
open System.Collections.Generic

module Parsing =

    [<Struct>]
    type Position = { CurToken: Token; PeekToken: Token } 

    type Parser2 (lexer: Lexer) =                
        
        let errors = new List<string>()
        
        let nextToken p: Position = { CurToken = p.PeekToken; PeekToken = lexer.NextToken() }

        let rec seek (p: Position)(tokenType: TokenType) =
            if p.CurToken.Type = tokenType || p.CurToken.Type = EOF then p 
            else seek (nextToken p) (tokenType)         
        
        let expectError (p: Position)(tType: TokenType) =
            errors.Add $"expected next token to be {tType}, got {p.PeekToken.Type} instead!"

        let expectPeek (p: Position) (tokenType: TokenType): Position option = 
            if p.PeekToken.Type = tokenType then
                 Some(nextToken p) 
            else 
                 expectError p tokenType
                 None    

        member this.Errors = errors
                   
        member this.ParseLetStatement(p: Position): (Position*LetStatement option) = 
            match expectPeek p IDENT with
                | None -> (p, None)
                | Some(identPosition) -> 
                    match expectPeek identPosition ASSIGN with 
                        | None -> (identPosition, None)
                        | Some(assignPosition) -> 
                            (seek assignPosition SEMICOLON, Some(LetStatement(p.CurToken, Identifier(identPosition.CurToken, identPosition.CurToken.Literal), None)))
        
        member this.ParseReturnStatement (p: Position): (Position*ReturnStatement option) = 
            (seek (nextToken p) SEMICOLON, Some(ReturnStatement(p.CurToken, None)))

            
        member this.ParseStatement(p: Position): (Position*Statement option) =
                  match p.CurToken.Type with
                      | LET -> (match this.ParseLetStatement p with | (n, Some(s)) -> (n, Some(s :> Statement)) | (_, None) -> (p, None))
                      | RETURN -> (match this.ParseReturnStatement p with | (n, Some(s)) -> (n, Some(s :> Statement)) | (_, None) -> (p, None))
                      | _ -> (p, None)

        member this.ParseProgram(): Ast.Program option =
            let program = Program();
            let rec parse(p: Position) =
                if p.CurToken.Type <> EOF then
                    let (next, statement) = this.ParseStatement p
                    if statement.IsSome then program.Append statement.Value
                    parse (nextToken next)           
            parse {CurToken = lexer.NextToken(); PeekToken= lexer.NextToken()} 
            Some(program)

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
       
        static member Seek(prs: Parser, tokenType: TokenType) = 
            if prs.CurToken.Type = tokenType || prs.CurToken.Type = EOF then prs 
            else Parser.Seek(prs.NextToken, tokenType)

        member p.NextToken = { Lexer = p.Lexer; CurToken = p.PeekToken; PeekToken = p.Lexer.NextToken() }               
       
        member p.ExpectPeek(t: TokenType): Parser option = 
            match p.PeekToken.Type = t with | true -> Some(p.NextToken) | false -> None
                                       
        member p.ParseProgram(): Ast.Program option =
            let program = Program();
            let rec parse(p: Parser) =
                if p.CurToken.Type <> EOF then
                    let (next, statement) = p.ParseStatement()                    
                    if statement.IsSome then program.Append statement.Value
                    parse next.NextToken
            parse p
            Some(program)

        member p.ParseStatement(): (Parser*Statement option) =
            match p.CurToken.Type with
                | LET -> (match p.ParseLetStatement() with | (parser, Some(s)) -> (parser, Some(s :> Statement)) | (_, None) -> (p, None))
                | _ -> (p, None)

        member p.ParseLetStatement(): (Parser*LetStatement option) =                         
            match p.ExpectPeek(IDENT) with
                | Some(ident) -> 
                    match ident.ExpectPeek(ASSIGN) with                     
                        | Some(assign) -> (Parser.Seek (assign, SEMICOLON), Some(LetStatement(p.CurToken, Identifier(ident.CurToken, ident.CurToken.Literal), None)))
                        | None -> (ident, None)
                | None -> (p, None)
            
       
            
            
            
        
        
        



