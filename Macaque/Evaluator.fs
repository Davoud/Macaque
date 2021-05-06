namespace Macaque

open Macaque
open Macaque.Objects
open Macaque.Ast

module rec Evaluator =
    
    let NULL = Null() :> Object
    let TRUE = Boolean(true) :> Object
    let FALSE = Boolean(false) :> Object

    let evalStatements (stmt: Statement list): Object = eval(stmt.Head)

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
        

    let rec eval (node: Node): Object = 
      match node with
      | :? Program              as p -> evalStatements p.Statements
      | :? ExpressionStatement  as x -> eval x.Expression
      | :? IntegerLiteral       as i -> Integer(i.Value) :> Object 
      | :? BooleanExpression    as b -> if b.Value then TRUE else FALSE
      | :? PrefixExpression     as px -> evalPrefixExpression px.Operator (eval px.Right)
      | :? InfíxExpression      as ix -> evalInfixExpression ix.Operator (eval ix.Left) (eval ix.Right)
      | _ -> NULL
        
