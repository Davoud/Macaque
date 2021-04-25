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

    type PrefixParseFn = unit -> Expression
    type InfixParseFn = Expression -> Expression
        
    type Parser (lexer: Lexer) =                
        let mutable errors: string list = []
        
        let mutable curToken: Token = lexer.NextToken()
        let mutable peekToken: Token = lexer.NextToken()
       
        let mutable prefixParseFns: Map<TokenType,PrefixParseFn> = Map.empty
        let mutable infixParseFns:  Map<TokenType,InfixParseFn>  = Map.empty
    
        let nextToken(): unit = 
            curToken <- peekToken
            peekToken <- lexer.NextToken()
                         
        let expectError (tType: TokenType) =
             errors <- errors @ [$"expected next token to be {tType}, got {peekToken.Type} instead!"]

        let expectPeek (tokenType: TokenType): bool = 
            match peekToken.Type = tokenType with 
                | true -> nextToken(); true                    
                | false -> expectError tokenType; false
                                     
        let peekTokenIs (tokenType: TokenType) = peekToken.Type = tokenType

        let registerPrefix (tokenType: TokenType) (fn: PrefixParseFn) = prefixParseFns <- prefixParseFns.Add(tokenType, fn)
        let registerInfix (tokenType: TokenType) (fn: InfixParseFn) = infixParseFns <- infixParseFns.Add(tokenType, fn)
                
        let parseIdentifier(): Expression = Identifier(curToken, curToken.Literal) :> Expression

        let parseIntegerLiteral(): Expression =
            match System.Int64.TryParse curToken.Literal with
            | true, value -> IntegerLiteral(curToken, int value) :> Expression
            | _ -> 
                errors <- errors @ [sprintf "could not parse %s as integer" (curToken.Literal)]
                NullExpression() :> Expression                              

        do registerPrefix (TokenType.IDENT) (parseIdentifier)
        do registerPrefix (TokenType.INT)   (parseIntegerLiteral)

        member this.Errors = errors
                   
        member this.ParseLetStatement(): LetStatement option = 
            let currentToken = curToken
            if expectPeek IDENT then 
                let ident = Identifier(curToken, curToken.Literal)
                if expectPeek ASSIGN then
                    nextToken()
                    if peekTokenIs SEMICOLON then nextToken()
                    LetStatement(currentToken, ident, None) |> Some
                else None
            else None
        
        member this.ParseReturnStatement(): ReturnStatement option = 
            let currentToken = curToken
            nextToken() 
            if peekTokenIs SEMICOLON then nextToken()
            ReturnStatement(currentToken, None) |> Some

        member this.ParseExpression (precdence: uint16): Expression option =
            match prefixParseFns.TryFind curToken.Type with
                | Some(prefix) -> Some(prefix())
                | None -> None
    
        member this.ParseExpressionStatement(): ExpressionStatement option = 
            let currentToken = curToken
            let expression = this.ParseExpression LOWEST
            if peekTokenIs SEMICOLON then nextToken()
            ExpressionStatement(currentToken, expression.Value) |> Some
            
        member this.ParseStatement(): Statement option =
                  match curToken.Type with
                      | LET     -> match this.ParseLetStatement()        with | Some(s) -> s :> Statement |> Some | None -> None
                      | RETURN  -> match this.ParseReturnStatement()     with | Some(s) -> s :> Statement |> Some | None -> None
                      | _       -> match this.ParseExpressionStatement() with | Some(s) -> s :> Statement |> Some | None -> None

        member this.ParseProgram(): Ast.Program option =
            let program = Program();            
            while curToken.Type <> EOF do
                match this.ParseStatement() with | Some(statement) -> program.Append statement | _ -> ()                    
                nextToken()            
            Some(program)

   
            
       
            
            
            
        
        
        



