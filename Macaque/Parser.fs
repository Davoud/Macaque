namespace Macaque

open Macaque.Tokens
open Macaque.Ast
open System.Collections.Generic

module Parsing =

    [<Literal>] 
    let LOWEST = 1us

    [<Literal>] 
    let EQUALS = 2us        // ==

    [<Literal>] 
    let LESSGREATER = 3us   // > OR <

    [<Literal>] 
    let SUM = 4us           // +

    [<Literal>] 
    let PRODUCT = 5us       // *

    [<Literal>] 
    let PREFIX = 6us        // -X OR !X

    [<Literal>] 
    let CALL = 7us          // myFunction(X)
    

    [<Struct>]
    type Position = { CurToken: Token; PeekToken: Token } 

    type PrefixParseFn = Position -> Expression
     and InfixParseFn = Position * Expression -> Expression

    type Parser (lexer: Lexer) =                
        
        let mutable errors: string list = []
        let mutable prefixParseFns: Map<TokenType,PrefixParseFn> = Map.empty
        let mutable infixParseFns: Map<TokenType,InfixParseFn> = Map.empty

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
               
        let registerPrefix (tokenType: TokenType) (fn: PrefixParseFn) = prefixParseFns <- prefixParseFns.Add(tokenType, fn)
        let registerInfix (tokenType: TokenType) (fn: InfixParseFn) = infixParseFns <- infixParseFns.Add(tokenType, fn)

        let parseIdentifier(p: Position): Expression = Identifier(p.CurToken, p.CurToken.Literal) :> Expression

        do registerPrefix (TokenType.IDENT) (parseIdentifier)

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

        member this.ParseExpression (p: Position) (precdenct: uint16): Expression option =
            match prefixParseFns.TryFind p.CurToken.Type with
                | Some(prefix) -> Some(prefix p)
                | None -> None
    
        member this.ParseExpressionStatement (p: Position): Position * ExpressionStatement option = 
            let expression = this.ParseExpression p LOWEST
            (seek (nextToken p) SEMICOLON, Some(ExpressionStatement(p.CurToken, expression.Value)))
            
        member this.ParseStatement(p: Position): (Position*Statement option) =
                  match p.CurToken.Type with
                      | LET -> (match this.ParseLetStatement p with 
                            | (n, Some(s)) -> (n, Some(s :> Statement)) 
                            | (_, None) -> (p, None))

                      | RETURN -> (match this.ParseReturnStatement p with 
                            | (n, Some(s)) -> (n, Some(s :> Statement)) 
                            | (_, None) -> (p, None))

                      | _ -> (match this.ParseExpressionStatement p with 
                                | (n, Some(s)) -> (n, Some(s :> Statement)) 
                                | (_, None) -> (p, None))

        member this.ParseProgram(): Ast.Program option =
            let program = Program();
            let rec parse(p: Position) =
                if p.CurToken.Type <> EOF then
                    let (next, statement) = this.ParseStatement p
                    if statement.IsSome then program.Append statement.Value
                    parse (nextToken next)           
            parse {CurToken = lexer.NextToken(); PeekToken= lexer.NextToken()} 
            Some(program)

   
            
       
            
            
            
        
        
        



