namespace Macaque

open Macaque.Tokens
open System.Text
open System.Collections.Generic

module Ast =
    
 type Node =
    abstract member TokenLiteral: unit -> string
    
 type Statement =
    inherit Node 
    
 type Expression =
    inherit Node
    

 type Program() =
    let statements = new List<Statement>()  
    member this.Statements = statements    
    member this.Append (s: Statement) = statements.Add s
    member this.TokenLiteral() =
        if this.Statements.Count > 0 then this.Statements.[0].TokenLiteral() else ""

 type Identifier (token: Token, value: string) =   
    member this.Token = token
    member this.Value = value        
    override this.ToString() = value
    interface Expression with 
        member this.TokenLiteral() = this.Token.Literal

 type LetStatement (token: Token, name: Identifier, value: Expression option) =

    member this.Token = token
    member this.Name = name
    member this.Value = value
        
    interface Statement with
        member this.TokenLiteral() = this.Token.Literal

    override this.ToString() = 
        let append (value: string) (buffer: StringBuilder) = buffer.Append value   
        new StringBuilder() 
            |> append (sprintf "%s " ((this :> Statement).TokenLiteral())) 
            |> append (sprintf "%A" this.Name) 
            |> append " = "
            |> append (match this.Value with | Some(exp) -> (sprintf "%A" exp) | None -> "")
            |> append ";"
            |> sprintf "%A"
        
        
    
        



