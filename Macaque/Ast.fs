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
    member this.String() = statements |> List.fold (fun acc elem -> (elem :> Node).String() + "\n" + acc) ""                 
        
 type Identifier (token: Token, value: string) =   
    member this.Token = token
    member this.Value = value        
    override this.ToString() = value
    interface Expression with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.ToString()

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
                
 type ReturnStatement (token: Token, value: Expression option) =    
    member this.Token = token
    member this.ReturnValue = value
    override this.ToString() = sprintf "%s %s;" ((this :> Statement).TokenLiteral()) (match this.ReturnValue with | Some(exp) -> (sprintf "%A" exp) | None -> "")
    interface Statement with 
        member this.TokenLiteral() = this.Token.Literal
        member this.String() = this.ToString()
        
  type ExpressionStatement (token: Token, expression: Expression option) =
    member this.Token = token
    member this.Expression = expression
    interface Statement with 
           member this.TokenLiteral() = this.Token.Literal
           member this.String() = match this.Expression with | Some(exp) -> exp.String() | None -> ""



