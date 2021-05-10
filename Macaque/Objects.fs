namespace Macaque


 module Objects =
    
    type ObjectType = INTEGER | BOOLEAN | NULL | RETURN_VALUE | ERROR
        
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

