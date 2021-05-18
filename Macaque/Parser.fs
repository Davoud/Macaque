namespace Macaque

open Macaque.Tokens
open Macaque.Ast
open System.Collections.Generic

module Parsing =
        
    type Precedence = 
        | LOWEST
        | EQUALS
        | LESSGREATER
        | SUM
        | PRODUCT
        | PREFIX
        | CALL
        | INDEX

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
            SLASH,      PRODUCT;
            LPAREN,     CALL;
            LBRACKET,   INDEX]
            
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
            | true, value -> IntegerLiteral(curToken, value) :> Expression
            | _ -> 
                errors <- errors @ [$"could not parse {curToken.Literal} as integer"]
                nilExpression                 
        
        let parseStringLiteral(): Expression = StringLiteral(curToken, curToken.Literal) :> Expression
       
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

        
                    
        let parseLetStatement(): LetStatement option = 
            let currentToken = curToken
            if expectPeek IDENT then 
                let ident = Identifier(curToken, curToken.Literal)          
                if expectPeek ASSIGN then
                    nextToken()
                    let value = parseExpression LOWEST
                    if peekTokenIs SEMICOLON then nextToken()
                    LetStatement(currentToken, ident, value) |> Some
                else None
            else None
        
        let parseReturnStatement(): ReturnStatement = 
            let currentToken = curToken
            nextToken() 
            let returnValue = parseExpression LOWEST
            if peekTokenIs SEMICOLON then nextToken()
            ReturnStatement(currentToken, returnValue)
           
        let parseExpressionStatement(): ExpressionStatement option = 
            let currentToken = curToken           
            let expression = parseExpression LOWEST  
            if(expression <> nilExpression) then
                if peekTokenIs SEMICOLON then nextToken()
                ExpressionStatement(currentToken, expression) |> Some
            else
                None

        let parseStatement(): Statement option =
            match curToken.Type with
                | LET     -> match parseLetStatement() with | Some(s) -> s :> Statement |> Some | None -> None
                | RETURN  -> parseReturnStatement() :> Statement |> Some
                | _       -> match parseExpressionStatement() with | Some(s) -> s :> Statement |> Some | None -> None
            
        let parseBlockStatement(): BlockStatement = 
            let ct = curToken
            let mutable statements: Statement list = []
            nextToken()
            while curToken.Type <> RBRACE && curToken.Type <> EOF do
                match parseStatement() with Some(s) -> statements <- s :: statements | None -> ()
                nextToken()
            BlockStatement(ct, statements |> List.rev)

        let parseElseExpression(): BlockStatement option =
            if peekTokenIs ELSE then 
                nextToken()
                if not (expectPeek LBRACE) then 
                    None 
                else 
                    parseBlockStatement() |> Some
            else
                None
            
        let parseIfExpression(): Expression =
            let ct = curToken
            if expectPeek LPAREN then
                nextToken()
                let condition = parseExpression LOWEST                
                if not (expectPeek RPAREN) || not (expectPeek LBRACE) then
                    nilExpression
                else 
                    IfExpression(ct, condition, parseBlockStatement(), parseElseExpression()) :> Expression             
            else
                nilExpression
        
        let parseFunctionParameters(): Identifier list =            
            if peekTokenIs RPAREN then 
                nextToken(); 
                []
            else
                nextToken()
                let mutable identifiers = [Identifier(curToken, curToken.Literal)]
                while peekTokenIs COMMA do
                    nextToken()
                    nextToken()
                    identifiers <- identifiers @ [Identifier(curToken, curToken.Literal)]
                if expectPeek RPAREN then identifiers else []
            
        let parseFunctionLiteral(): Expression =
            let ct = curToken
            if expectPeek LPAREN then
                let parameters = parseFunctionParameters()
                if expectPeek LBRACE then FunctionLiteral(ct, parameters, parseBlockStatement()) :> Expression else nilExpression
            else nilExpression

        let parseExpressionList(endToken: TokenType): Expression list =
            if peekTokenIs endToken then 
                nextToken() 
                []
            else
                nextToken()
                let mutable args = [parseExpression LOWEST]
                while peekTokenIs COMMA do
                    nextToken()
                    nextToken()
                    args <- args @ [parseExpression LOWEST]
                if expectPeek endToken then args else []
    
        let parseCallExpresion(func: Expression): Expression =             
            CallExpression(curToken, func, parseExpressionList RPAREN) :> Expression

        let parseArrayLiteral(): Expression = ArrayLiteral(curToken, parseExpressionList RBRACKET |> List.toArray) :> Expression

        let parseIndexExpression(left: Expression): Expression =
            let ct = curToken
            nextToken()
            let exp = IndexExpression(curToken, left, parseExpression LOWEST)
            if expectPeek RBRACKET then exp :> Expression else nilExpression


        do parseIdentifier        |> registerPrefix [IDENT]
        do parseIntegerLiteral    |> registerPrefix [INT]
        do parsePrefixExpression  |> registerPrefix [BANG; MINUS]
        do parseInfixExpression   |> registerInfix  [PLUS; MINUS; SLASH; ASTRISK; EQ; NOT_EQ; LT; GT]               
        do parseBooleanExpression |> registerPrefix [TRUE; FALSE]
        do parseGroupedExpression |> registerPrefix [LPAREN]
        do parseIfExpression      |> registerPrefix [IF]
        do parseFunctionLiteral   |> registerPrefix [FUNCTION]
        do parseCallExpresion     |> registerInfix  [LPAREN]
        do parseStringLiteral     |> registerPrefix [STRING]
        do parseArrayLiteral      |> registerPrefix [LBRACKET]
        do parseIndexExpression   |> registerInfix  [LBRACKET]
                                                       
        member this.Errors = errors
       
        member this.ParseProgram(): Ast.Program =
            let program = Program();            
            while curToken.Type <> EOF do
                match parseStatement() with | Some(statement) -> program.Append statement | _ -> ()                    
                nextToken()            
            program

   
            
       
            
            
            
        
        
        



