namespace Macaque


 module Objects =
    
    [<Literal>] 
    let INTEGER_OBJ = "INTEGER"

    [<Literal>]
    let BOOLEAN_OBJ = "BOOLEAN"

    [<Literal>]
    let NULL_OBJ = "NULL"

    [<Literal>]
    let RETURN_VALUE_OBJ = "RETURN_VALUE"

    type ObjectType = INTEGER_OBJ | BOOLEAN_OBJ | NULL_OBJ | RETURN_VALUE_OBJ
        
    type Object =
        abstract member Type: unit -> ObjectType
        abstract member Inspect: unit -> string

    [<Struct>]
    type Integer(value: int64) =
        member self.Value = value
        interface Object with   
            member self.Type() = INTEGER_OBJ
            member self.Inspect() = sprintf "%d" value

    [<Struct>]
    type Boolean(value: bool) =
        member self.Value = value
        interface Object with 
            member self.Type() = BOOLEAN_OBJ
            member self.Inspect() = sprintf "%b" value
   
    type Null =
      struct
        interface Object with
            member self.Type() = NULL_OBJ
            member self.Inspect() = "null"
      end

    [<Struct>]
    type ReturnValue(value: Object) =
        member self.Value = value
        interface Object with
            member self.Type() = RETURN_VALUE_OBJ
            member self.Inspect() = sprintf "%s" (value.Inspect())
        
