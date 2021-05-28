namespace Macaque

 open System.Collections.Generic


 module Objects =
    
    type ObjectType = INTEGER | BOOLEAN | NULL | RETURN_VALUE | ERROR | FUNCTION | STRING | BUILTIN | ARRAY | HASH
        
    type HashKey = { Type: ObjectType; Value: uint64 }
        
    type Hashable =        
        abstract member HashKey: HashKey

    type Object =
        abstract member Type: ObjectType
        abstract member Inspect: unit -> string

    type BuiltinFunction = Object array -> Object

    type HashPair = { key: Object; value: Object }

    type Integer(value: int64) =
        let hashKey = { Type = INTEGER; Value = uint64 (value) }  
        member self.Value = value
        interface Object with   
            member self.Type = INTEGER
            member self.Inspect() = sprintf "%d" value
        interface Hashable with
            member self.HashKey = hashKey           

    type ``String``(value: string) =
        let hashKey = { Type = STRING; Value = uint64 (value.GetHashCode()) }            
        member self.Value = value
        interface Object with
            member self.Type = STRING
            member self.Inspect() = value
        interface Hashable with
            member self.HashKey = hashKey            

    type Boolean(value: bool) =
        let hashKey = { Type = BOOLEAN; Value = if value then 1UL else 0UL }
        member self.Value = value
        interface Object with 
            member self.Type = BOOLEAN
            member self.Inspect() = sprintf "%b" value
        interface Hashable with
            member self.HashKey = hashKey            
   
    type Null =
      struct
        interface Object with
            member self.Type = NULL
            member self.Inspect() = "null"
      end

    type ReturnValue(value: Object) =
        member self.Value = value
        interface Object with
            member self.Type = RETURN_VALUE
            member self.Inspect() = sprintf "%s" (value.Inspect())
        
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
      
    type Function(parameters: Ast.Identifier list, body: Ast.BlockStatement, env: Environment) =
        member self.Parameters = parameters
        member self.Body = body
        member self.Env = env
        interface Object with
            member self.Type = FUNCTION
            member self.Inspect() =
                let paramList = parameters |> Seq.map (sprintf "%O") |> String.concat ", "
                $"fn({paramList}) {{\n{body}\n}}"
        
    type Builtin(fn: BuiltinFunction) =
        member self.Fn = fn
        interface Object with
            member self.Type = BUILTIN
            member self.Inspect() = "builtin function"

    type Array(elements: Object array) =
        member self.Elements = elements
        interface Object with
            member self.Type = ARRAY
            member self.Inspect() = elements |> Seq.map (fun el -> el.Inspect()) |> String.concat ", " |> sprintf "[%s]"

    type Hash(pairs: Map<HashKey,HashPair>) =
        new() = Hash(Map.empty)
        member selt.Pairs = pairs        
        member self.Append (hashable: Hashable) (key: Object) (value: Object) = 
            Hash(pairs.Add(hashable.HashKey, {key = key; value = value}))
        
        interface Object with
            member self.Type = HASH
            member self.Inspect() = 
                pairs |> Seq.map(fun i -> sprintf "%s: %s" (i.Value.key.Inspect()) (i.Value.value.Inspect()))
                      |> String.concat ", " 
                      |> sprintf "{%s}" 
               