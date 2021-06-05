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
    inherit System.IComparable
    
    
 type Program() =    
    let mutable statements: Statement list = []
    member this.Statements = statements    
    member this.Append (s: Statement) = statements <- statements @ [s]
    override this.ToString() = (this :> Node).String()
    interface Node with 
        member this.TokenLiteral() = match statements with head :: _ -> head.TokenLiteral() | [] -> ""    
        member this.String() = statements |> List.map (sprintf "%O") |> String.concat ""        
 
 type Identifier (token: Token, value: string) =   
    member this.Token = token
    member this.Value = value        
    override this.ToString() = (this :> Expression).String()
    interface Expression with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = value
        member this.CompareTo(other: obj) =
            match other with
            | :? Identifier as idnt -> if idnt.Value = this.Value then 0 else 1
            | _ -> -1
                 
 type LetStatement (token: Token, name: Identifier, value: Expression) =
    member _.Token = token
    member _.Name = name
    member _.Value = value    
    override this.ToString() = (this :> Statement).String()
    interface Statement with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"{this.Token.Literal} {this.Name} = {value};"
                         
 type ReturnStatement (token: Token, value: Expression) =    
    member this.Token = token
    member this.ReturnValue = value
    override this.ToString() = (this :> Statement).String()
    interface Statement with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"{this.Token.Literal} {this.ReturnValue}" 
   
 type ExpressionStatement (token: Token, expression: Expression) =
    member this.Token = token
    member this.Expression = expression
    override this.ToString() = (this :> Statement).String()
    interface Statement with 
           member this.TokenLiteral() = this.Token.Literal
           member this.String() = $"{this.Expression}"

 type IntegerLiteral (token: Token, value: int64) = 
    member this.Token = token
    member this.Value = value
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"%i{this.Value}"
        member this.CompareTo(other: obj) =
            match other with
            | :? IntegerLiteral as intLit -> if intLit.Value = this.Value then 0 else 1
            | _ -> -1
   
 type PrefixExpression (token: Token, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Right = right
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"({this.Operator}{this.Right})" 
        member this.CompareTo(other: obj) = 
            match other with
            | :? PrefixExpression as pf -> if pf = this then 0 else 1
            | _ -> -1
   
 type InfíxExpression (token: Token, left: Expression, operator: string, right: Expression) =
    member this.Token = token
    member this.Operator = operator
    member this.Left = left 
    member this.Right = right 
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = $"({this.Left} {this.Operator} {this.Right})"
        member this.CompareTo(other: obj) = 
            match other with
            | :? InfíxExpression as infExp -> if infExp = this then 0 else 1
            | _ -> -1

 
 type BooleanExpression (token: Token, value: bool) =
    member this.Token = token
    member this.Value = value
    override this.ToString() = (this :> Expression).String()
    interface Expression with   
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.Token.Literal
        member this.CompareTo(other: obj) = 
            match other with
            | :? BooleanExpression as b -> if b.Value = this.Value then 0 else 1
            | _ -> -1

  
 type BlockStatement(token: Token, statements: Statement list) =
    member this.Token = token
    member this.Statements = statements
    override this.ToString() = (this :> Statement).String();
    interface Statement with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = statements |> List.fold (fun str stmt -> $"{str}{stmt}") "" 
        
 
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
        member this.CompareTo(other: obj) = 
            match other with
            | :? IfExpression as ifExp -> if ifExp = this then 0 else 1
            | _ -> -1
 
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
        member this.CompareTo(other: obj) = 
            match other with
            | :? FunctionLiteral as fl -> if fl = this then 0 else 1
            | _ -> -1

  
  type CallExpression(token: Token, func: Expression, arguments: Expression list) = 
    member this.Token = token
    member this.Function = func
    member this.Arguments = arguments
    override this.ToString() = (this :> Expression).String()
    interface Expression with   
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = 
            let args = this.Arguments |> List.map (sprintf "%O") |> String.concat ", "
            $"{this.Function}({args})"
        member this.CompareTo(other: obj) = 
            match other with
            | :? CallExpression as callExp -> if callExp = this then 0 else 1
            | _ -> -1
  
  type StringLiteral(token: Token, value: string) =
    member this.Token = token
    member this.Value = value
    override this.ToString() = (this :> Expression).String()
    interface Expression with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = value
        member this.CompareTo(other: obj) = 
            match other with
            | :? StringLiteral as str -> if str.Value = this.Value then 0 else 1
            | _ -> -1
            

  type ArrayLiteral(token: Token, elements: Expression array) =
    member this.Token = token
    member this.Elements = elements
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = sprintf "[%s]" (elements |> Seq.map(sprintf "%O") |> String.concat ", ")
        member this.CompareTo(other: obj) = 
            match other with
            | :? ArrayLiteral as arr -> if arr = this then 0 else 1
            | _ -> -1

  type IndexExpression(token: Token, left: Expression, index: Expression) =
    member this.Token = token
    member this.Left = left
    member this.Index = index
    override this.ToString() = (this :> Expression).String()
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = sprintf "(%O[%O])" left index
        member this.CompareTo(other: obj) = 
            match other with
            | :? IndexExpression as indExp -> if indExp = this then 0 else 1                
            | _ -> -1
 
  type HashPairs = Map<Expression, Expression>
 
  type HashLiteral(token: Token, pairs: HashPairs) =
    member this.Token = token
    member this.Pairs = pairs
    interface Expression with
        member this.TokenLiteral() = this.Token.Literal
        member this.String() =
            let keyValues = pairs |> Seq.map(fun item -> item.Key.String() + ":" + item.Value.String()) 
            sprintf "{%s}" (keyValues |> String.concat ", ")
        member this.CompareTo(other: obj) =
            match other with
            | :? HashLiteral as hl -> if hl = this then 0 else 1
            | _ -> -1