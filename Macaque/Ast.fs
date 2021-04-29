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
 
 type NullExpression() =
    static let instance = NullExpression() :> Expression
    static member Instance = instance
    interface Expression with
         member this.TokenLiteral() = failwith "Null Expression has no token!"
         member this.String() = "<NULL EXPRESSION>"

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
    interface Statement with 
           member this.TokenLiteral() = this.Token.Literal
           member this.String() = this.Expression.String()

  [<Struct>]
  type IntegerLiteral (token: Token, value: int) = 
    member this.Token = token
    member this.Value = value
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.Value.ToString()
  
  [<Struct>]
  type PrefixExpression (token: Token, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Right = right
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = sprintf "(%s%s)" this.Operator (this.Right.String())
  
  [<Struct>]
  type InfíxExpression (token: Token, left: Expression, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Left = left 
    member this.Right = right 
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = sprintf "(%s %s %s)"  (this.Left.String()) this.Operator (this.Right.String())

    