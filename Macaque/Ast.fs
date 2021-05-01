namespace Macaque

open Macaque.Tokens
open System.Text
open System.Collections.Generic

module Ast =
    
 type Node =
    abstract member TokenLiteral: unit -> string
    abstract member String: unit -> string
    
 type Statement =
    inherit Node 
    
 type Expression =
    inherit Node
    
 type Program() =    
    let mutable statements: Statement list = []
    member this.Statements = statements    
    member this.Append (s: Statement) = statements <- statements @ [s]
    member this.TokenLiteral() = match statements with head :: _ -> head.TokenLiteral() | [] -> ""    
    member this.String() = statements |> List.fold (fun acc elem -> acc + (elem :> Node).String()) ""                 
 
 [<Struct>]
 type Identifier (token: Token, value: string) =   
    member this.Token = token
    member this.Value = value        
    override this.ToString() = value
    interface Expression with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.ToString()

 [<Struct>]
 type LetStatement (token: Token, name: Identifier, value: Expression option) =
    member this.Token = token
    member this.Name = name
    member this.Value = value        
    interface Statement with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.ToString()
    override this.ToString() = 
        let exp = match this.Value with Some(exp) -> (sprintf "%A" exp) | None -> ""
        sprintf "%s %A = %s;" ((this :> Statement).TokenLiteral()) this.Name exp        

 [<Struct>]                
 type ReturnStatement (token: Token, value: Expression option) =    
    member this.Token = token
    member this.ReturnValue = value
    override this.ToString() = sprintf "%s %s;" ((this :> Statement).TokenLiteral()) (match this.ReturnValue with | Some(exp) -> (sprintf "%A" exp) | None -> "")
    interface Statement with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.ToString()
  
 [<Struct>]
 type ExpressionStatement (token: Token, expression: Expression) =
    member this.Token = token
    member this.Expression = expression
    override this.ToString() = (this :> Statement).ToString()
    interface Statement with 
           member this.TokenLiteral() = this.Token.Literal
           member this.String() = this.Expression.String()

 [<Struct>]
 type IntegerLiteral (token: Token, value: int) = 
    member this.Token = token
    member this.Value = value
    override this.ToString() = (this :> Expression).ToString()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.Value.ToString()
  
 [<Struct>]
 type PrefixExpression (token: Token, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Right = right
    override this.ToString() = (this :> Expression).ToString()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"({this.Operator}{this.Right.String()})" 
  
 [<Struct>]
 type InfíxExpression (token: Token, left: Expression, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Left = left 
    member this.Right = right 
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = sprintf "(%s %s %s)"  (this.Left.String()) this.Operator (this.Right.String())

 [<Struct>]  
 type BooleanExpression (token: Token, value: bool) =
    member this.Token = token
    member this.Value = value
    override this.ToString() = (this :> Expression).String()
    interface Expression with   
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.Token.Literal
  
 type BlockStatement(token: Token, statements: Statement list) =
    member this.Token = token
    member this.Statements = statements
    override this.ToString() = (this :> Statement).String();
    interface Statement with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = statements |> List.fold (fun str stmt -> $"{str}{stmt}") "" 

 [<Struct>]
 type IfExpression(token: Token, condition: Expression, consequence: BlockStatement, alternative: BlockStatement option) =
    member this.Token = token
    member this.Condition = condition
    member this.Consequence = consequence
    member this.Alternative = alternative
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() =           
            match this.Alternative with 
            | Some(bstmt) -> $"if{this.Condition} {this.Consequence} else {bstmt}"
            | None -> $"if{this.Condition} {this.Consequence}"

 [<Struct>]            
 type FunctionLiteral(token: Token, paramters: Identifier list, body: BlockStatement) =
    member this.Token = token
    member this.Parameters = paramters
    member this.Body = body
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = 
            let paraList = this.Parameters |> List.map (fun p -> $"{p}") |> String.concat ", "
            $"{this.Token.Literal}({paraList}) {this.Body}"