namespace Macaque

open Macaque
open Macaque.Objects
open Macaque.Ast

module rec Evaluator =
    
    let NULL = Null() :> Object
    let TRUE = Boolean(true) :> Object
    let FALSE = Boolean(false) :> Object

    let evalProgram (program: Program): Object =
        let mutable result: Object = NULL
       
        let rec loop (statements: Statement list) =
            match statements with 
            | statement :: rest -> 
                result <- eval statement
                match result with :? ReturnValue as rv -> rv.Value | _ -> loop rest
            | _ -> result

        loop program.Statements
    
    let evalBlockStatement (block: BlockStatement): Object = 
        let mutable result: Object = NULL

        let rec loop (statements: Statement list) =
            match statements with
            | statements :: rest ->
                result <- eval statements
                if result <> NULL && result.Type() = ObjectType.RETURN_VALUE_OBJ then result else loop rest
            | _ -> result

        loop block.Statements
    
    let nativeBoolToBooleanObject(value: bool): Object = if value then TRUE else FALSE

    let evalBangOperatorExpression (right: Object): Object =
        match right with
        | :? Boolean as b -> nativeBoolToBooleanObject(not b.Value)
        | :? Null -> TRUE        
        | _     -> FALSE

    let evalMinusPrefixOperationExpression (right: Object): Object =
        match right with
        | :? Integer as i -> Integer(-i.Value) :> Object
        | _ -> NULL

    let evalPrefixExpression (operator: string) (right: Object): Object =
        match operator with
        | "!" -> evalBangOperatorExpression right
        | "-" -> evalMinusPrefixOperationExpression right
        | _ -> NULL

    let evalIntegerInfixExpression (operator: string) (left: Integer) (right: Integer): Object =
        match operator with
        | "+" -> Integer(left.Value + right.Value) :> Object
        | "-" -> Integer(left.Value - right.Value) :> Object
        | "*" -> Integer(left.Value * right.Value) :> Object
        | "/" -> Integer(left.Value / right.Value) :> Object
        | "<" -> nativeBoolToBooleanObject(left.Value < right.Value) 
        | ">" -> nativeBoolToBooleanObject(left.Value > right.Value) 
        | "==" -> nativeBoolToBooleanObject(left.Value = right.Value)
        | "!=" -> nativeBoolToBooleanObject(left.Value <> right.Value)
        | _ -> NULL
    

    let evalInfixExpression (operator: string) (left: Object) (right: Object) = 
        match left, right with
        | (:? Integer as leftInt), (:? Integer as rightInt) -> evalIntegerInfixExpression operator leftInt rightInt                    
        | _ -> match operator with
               | "==" -> nativeBoolToBooleanObject(left = right)
               | "!=" -> nativeBoolToBooleanObject(left <> right)
               | _ -> NULL
    
    let isTruthy(object: Object): bool = 
        if object = NULL then false
        else if object = TRUE then true
        else if object = FALSE then false
        else true        
                               
    let evalIfExpresion (ie: IfExpression) =        
        if ie.Condition |> eval |> isTruthy then eval ie.Consequence
        else if ie.Alternative.IsSome then eval ie.Alternative.Value
        else NULL

    let rec eval (node: Node): Object = 
      match node with
      | :? Program              as pg -> evalProgram pg
      | :? ExpressionStatement  as xp -> eval xp.Expression
      | :? IntegerLiteral       as il -> Integer(il.Value) :> Object 
      | :? BooleanExpression    as bx -> if bx.Value then TRUE else FALSE
      | :? PrefixExpression     as px -> evalPrefixExpression px.Operator (eval px.Right)
      | :? InfíxExpression      as ix -> evalInfixExpression ix.Operator (eval ix.Left) (eval ix.Right)
      | :? BlockStatement       as bs -> evalBlockStatement bs
      | :? IfExpression         as fx -> evalIfExpresion fx
      | :? ReturnStatement      as rs -> ReturnValue(eval rs.ReturnValue) :> Object
      | _                             -> NULL
        
