namespace Macaque

open Macaque.Tokens
open Macaque.Ast
open System.Collections.Generic

module Parsing =

    [<Struct>]
    type Position = { CurToken: Token; PeekToken: Token } 

    type Parser2 (lexer: Lexer) =                
        
        let mutable errors: string list = []
        
        let nextToken p: Position = { CurToken = p.PeekToken; PeekToken = lexer.NextToken() }

        let rec seek (p: Position)(tokenType: TokenType) =
            if p.CurToken.Type = tokenType || p.CurToken.Type = EOF then p 
            else seek (nextToken p) (tokenType)         
        
        let expectError (p: Position)(tType: TokenType) =
             errors <- errors @ [$"expected next token to be {tType}, got {p.PeekToken.Type} instead!"]

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
                      | LET    -> (match this.ParseLetStatement p with    | (n, Some(s)) -> (n, Some(s :> Statement)) | (_, None) -> (p, None))
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

   
            
       
            
            
            
        
        
        



