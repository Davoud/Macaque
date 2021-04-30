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

    type Precedence = LOWEST | EQUALS | LESSGREATER | SUM | PRODUCT | PREFIX | CALL

    type PrefixParseFn = unit -> Expression
    type InfixParseFn = Expression -> Expression
        
    type Parser (lexer: Lexer) =                
        let mutable errors: string list = []
        
        let mutable curToken: Token = lexer.NextToken()
        let mutable peekToken: Token = lexer.NextToken()
               
        let mutable prefixParseFns: Map<TokenType,PrefixParseFn> = Map.empty
        let mutable infixParseFns:  Map<TokenType,InfixParseFn>  = Map.empty
    
        let precedences = Map [
            EQ,         EQUALS;
            NOT_EQ,     EQUALS;
            LT,         LESSGREATER;
            GT,         LESSGREATER;
            PLUS,       SUM; 
            MINUS,      SUM;
            ASTRISK,    PRODUCT;
            SLASH,      PRODUCT;]
            
        let nilExpression = { new Expression with
            member this.TokenLiteral() = "???"
            member this.String() = "???" }
    
        let nextToken(): unit = 
            curToken <- peekToken
            peekToken <- lexer.NextToken()
                         
        let expectError (tType: TokenType) =
             errors <- errors @ [$"expected next token to be {tType}, got {peekToken} instead!"]

        let expectPeek (tokenType: TokenType): bool = 
            match peekToken.Type = tokenType with 
            | true -> nextToken(); true                    
            | false -> expectError tokenType; false
                                     
        let peekTokenIs (tokenType: TokenType) = peekToken.Type = tokenType

        let peekPrecedenc(): Precedence = precedences.GetValueOrDefault(peekToken.Type, LOWEST)
        let curPrecedence(): Precedence = precedences.GetValueOrDefault(curToken.Type, LOWEST)

        let rec bind (precedence: Precedence) (left: Expression): Expression =
            if not (peekTokenIs SEMICOLON) && precedence < peekPrecedenc() then
                match infixParseFns.TryFind peekToken.Type with
                | Some(infix) -> nextToken(); bind precedence (infix left)
                | None -> left
            else
                left

        let parseExpression (precdence: Precedence): Expression =
            match prefixParseFns.TryFind curToken.Type with
            | Some(prefix) -> bind precdence (prefix())                      
            | None -> 
                errors <- errors @ [$"no prefix parse function for {curToken.Literal} found!"]
                nilExpression
                         
        let parseInfixExpression (left: Expression): Expression = 
            let currentToken = curToken
            let precedence = curPrecedence()
            nextToken()
            InfíxExpression (currentToken, left, currentToken.Literal, parseExpression precedence) :> Expression

        let registerPrefix (tokenTypes: TokenType list) (fn: PrefixParseFn) =            
            tokenTypes |> List.iter (fun tokenType -> prefixParseFns <- prefixParseFns.Add(tokenType, fn))
        
        let registerInfix (tokenTypes: TokenType list) (fn: InfixParseFn) = 
             tokenTypes |> List.iter (fun tokenType -> infixParseFns <- infixParseFns.Add(tokenType, fn))
                
        let parseIdentifier(): Expression = Identifier(curToken, curToken.Literal) :> Expression

        let parseIntegerLiteral(): Expression =
            match System.Int64.TryParse curToken.Literal with
            | true, value -> IntegerLiteral(curToken, int value) :> Expression
            | _ -> 
                errors <- errors @ [$"could not parse {curToken.Literal} as integer"]
                nilExpression                            
       
        let parsePrefixExpression(): Expression =
            let currentToken = curToken
            nextToken()           
            PrefixExpression(currentToken, currentToken.Literal, parseExpression PREFIX) :> Expression
                
        let parseBooleanExpression(): Expression =
            BooleanExpression(curToken, curToken.Type = TRUE) :> Expression   

        let parseGroupedExpression(): Expression =
            nextToken()
            let exp = parseExpression LOWEST
            if expectPeek RPAREN then exp else nilExpression

        let parseBlockStatement(): BlockStatement = 
            let ct = curToken
            let mutable statement: Statement list = []
            nextToken()
            BlockStatement(ct, statement) 


        let parseIfExpression(): Expression =
            let ct = curToken
            if expectPeek LPAREN then
                nextToken()
                let condition = parseExpression LOWEST
                if expectPeek RPAREN && expectPeek LBRACE then
                    IfExpression(ct, condition, parseBlockStatement(), None) :> Expression
                else
                    nilExpression
            else
                nilExpression
                    


            
        do parseIdentifier        |> registerPrefix [IDENT]
        do parseIntegerLiteral    |> registerPrefix [INT]
        do parsePrefixExpression  |> registerPrefix [BANG; MINUS]
        do parseInfixExpression   |> registerInfix  [PLUS; MINUS; SLASH; ASTRISK; EQ; NOT_EQ; LT; GT]               
        do parseBooleanExpression |> registerPrefix [TRUE; FALSE]
        do parseGroupedExpression |> registerPrefix [LPAREN]
        
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
           
        member this.ParseExpressionStatement(): ExpressionStatement option = 
            let currentToken = curToken           
            let expression = parseExpression LOWEST  
            if(expression <> nilExpression) then
                if peekTokenIs SEMICOLON then nextToken()
                ExpressionStatement(currentToken, expression) |> Some
            else
                None
                        
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

   
            
       
            
            
            
        
        
        



