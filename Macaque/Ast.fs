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
    member this.String() = statements |> List.map (fun s -> $"{s}") |> String.concat ""        
 
 [<Struct>]
 type Identifier (token: Token, value: string) =   
    member this.Token = token
    member this.Value = value        
    override this.ToString() = (this :> Expression).String()
    interface Expression with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = value

 [<Struct>]
 type LetStatement (token: Token, name: Identifier, value: Expression) =
    member _.Token = token
    member _.Name = name
    member _.Value = value    
    override this.ToString() = (this :> Statement).String()
    interface Statement with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"{this.Token.Literal} {this.Name} = {value};"
           
             
 [<Struct>]                
 type ReturnStatement (token: Token, value: Expression) =    
    member this.Token = token
    member this.ReturnValue = value
    override this.ToString() = (this :> Statement).String()
    interface Statement with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"{this.Token.Literal} {this.ReturnValue}" 
  
 [<Struct>]
 type ExpressionStatement (token: Token, expression: Expression) =
    member this.Token = token
    member this.Expression = expression
    override this.ToString() = (this :> Statement).String()
    interface Statement with 
           member this.TokenLiteral() = this.Token.Literal
           member this.String() = $"{this.Expression}"

 [<Struct>]
 type IntegerLiteral (token: Token, value: int) = 
    member this.Token = token
    member this.Value = value
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"%i{this.Value}"
  
 [<Struct>]
 type PrefixExpression (token: Token, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Right = right
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"({this.Operator}{this.Right})" 
  
 [<Struct>]
 type InfíxExpression (token: Token, left: Expression, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Left = left 
    member this.Right = right 
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"({this.Left} {this.Operator} {this.Right})"

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
            | Some(bstmt) -> $"if {this.Condition} {this.Consequence} else {bstmt}"
            | None -> $"if {this.Condition} {this.Consequence}"

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

  [<Struct>]
  type CallExpression(token: Token, func: Expression, arguments: Expression list) = 
    member this.Token = token
    member this.Function = func
    member this.Arguments = arguments
    override this.ToString() = (this :> Expression).String()
    interface Expression with   
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = 
            let args = this.Arguments |> List.map (fun arg -> $"{arg}") |> String.concat ", "
            $"{this.Function}({args})"