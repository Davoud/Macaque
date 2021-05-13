namespace Macaque

 open System.Collections.Generic


 module Objects =
    
    type ObjectType = INTEGER | BOOLEAN | NULL | RETURN_VALUE | ERROR | FUNCTION
        
    type Object =
        abstract member Type: ObjectType
        abstract member Inspect: unit -> string

    [<Struct>]
    type Integer(value: int64) =
        member self.Value = value
        interface Object with   
            member self.Type = INTEGER
            member self.Inspect() = sprintf "%d" value

    [<Struct>]
    type Boolean(value: bool) =
        member self.Value = value
        interface Object with 
            member self.Type = BOOLEAN
            member self.Inspect() = sprintf "%b" value
   
    type Null =
      struct
        interface Object with
            member self.Type = NULL
            member self.Inspect() = "null"
      end

    [<Struct>]
    type ReturnValue(value: Object) =
        member self.Value = value
        interface Object with
            member self.Type = RETURN_VALUE
            member self.Inspect() = sprintf "%s" (value.Inspect())
        
    [<Struct>]
    type Error(message: string) =
        member self.Message = message
        interface Object with
            member self.Type = ERROR
            member self.Inspect() = sprintf "ERROR: %s" message

    type Environment(outer: Environment option) =
        let mutable store: Map<string, Object> = Map.empty
        new() = Environment(None)
        member self.Outer = outer
        member self.Get(name: string) = 
            match store.TryFind(name) with
            | Some(value) -> Some(value)
            | None -> match outer with | Some(env) -> env.Get(name) | None -> None

        member self.Set(name: string, value: Object) = store <- store.Add(name, value);
      
    [<Struct>]
    type Function(parameters: Ast.Identifier list, body: Ast.BlockStatement, env: Environment) =
        member self.Parameters = parameters
        member self.Body = body
        member self.Env = env
        interface Object with
            member self.Type = FUNCTION
            member self.Inspect() =
                let paramList = parameters |> Seq.map (sprintf "%O") |> String.concat ", "
                $"fn({paramList}) {{\n{body}\n}}"
                