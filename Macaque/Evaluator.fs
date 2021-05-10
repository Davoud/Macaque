namespace Macaque

open Macaque
open Macaque.Objects
open Macaque.Ast

module rec Evaluator =
    
    let NULL = Null() :> Object
    let TRUE = Boolean(true) :> Object
    let FALSE = Boolean(false) :> Object
    
    let newError message = Error(message) :> Object

    let isError (object: Object) = object.Type = ERROR 

    let evalProgram (program: Program): Object =                       
        let rec evalAll (statements: Statement list) = 
            match statements with
            | statement :: rest ->
                match eval statement with
                | :? ReturnValue as r -> Some(r.Value)
                | e when e.Type = ERROR -> Some(e) 
                | last -> match evalAll rest with Some(o) -> Some(o) | None -> Some(last)
            | [] -> None
        
        match evalAll program.Statements with Some(obj) -> obj | None -> NULL

    
    let evalBlockStatement (block: BlockStatement): Object =  
        let inline isReturnOrError objType = objType = RETURN_VALUE || objType = ERROR
        let rec evalAll (statements: Statement list) =
            match statements with
            | statement :: rest ->
                let result = eval statement
                if result <> NULL && isReturnOrError (result.Type) then Some(result) 
                else match evalAll rest with Some(o) -> Some(o) | None -> Some(result)
            | [] -> None
        
        match evalAll block.Statements with Some(lastObject) -> lastObject | None -> NULL
    

    let nativeBoolToBooleanObject(value: bool): Object = if value then TRUE else FALSE

    let evalBangOperatorExpression (right: Object): Object =
        match right with
        | :? Boolean as b -> nativeBoolToBooleanObject(not b.Value)
        | :? Null -> TRUE        
        | _     -> FALSE

    let evalMinusPrefixOperationExpression (right: Object): Object =
        match right with
        | :? Integer as i -> Integer(-i.Value) :> Object
        | _ -> newError (sprintf "unknown operator: -%O" (right.Type))

    let evalPrefixExpression (operator: string) (right: Object): Object =
        match operator with
        | "!" -> evalBangOperatorExpression right
        | "-" -> evalMinusPrefixOperationExpression right
        | _ -> newError (sprintf "unknown operator: %s%O" operator (right.Type))

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
        | _ -> newError (sprintf "unknown operator: %O %s %O" ((left :> Object).Type) operator ((left :> Object).Type))
    

    let evalInfixExpression (operator: string) (left: Object) (right: Object) = 
        match left, right with
        | (l, r) when l.Type <> r.Type -> newError (sprintf "type mismatch: %O %s %O" (l.Type) operator (r.Type))
        | (:? Integer as leftInt), (:? Integer as rightInt) -> evalIntegerInfixExpression operator leftInt rightInt                    
        | _ -> match operator with
               | "==" -> nativeBoolToBooleanObject(left = right)
               | "!=" -> nativeBoolToBooleanObject(left <> right)
               | _ -> newError (sprintf "unknown operator: %O %s %O" (left.Type) operator (right.Type))
    
    let isTruthy(object: Object): bool = 
        if object = NULL then false
        else if object = TRUE then true
        else if object = FALSE then false
        else true        
                               
    let evalIfExpresion (ie: IfExpression) =
        match eval ie.Condition with
        | cond when isError cond -> cond
        | cond -> 
            if isTruthy cond then eval ie.Consequence
            else if ie.Alternative.IsSome then eval ie.Alternative.Value
            else NULL

    let rec eval (node: Node): Object = 
      match node with
      | :? Program as program -> evalProgram program
      | :? ExpressionStatement as expressionStmt -> eval expressionStmt.Expression
      | :? IntegerLiteral as integer -> Integer(integer.Value) :> Object 
      | :? BooleanExpression as boolean -> if boolean.Value then TRUE else FALSE
      | :? BlockStatement as blockStmt -> evalBlockStatement blockStmt
      | :? IfExpression as ifExp -> evalIfExpresion ifExp

      | :? PrefixExpression as prefixExp -> 
            match eval prefixExp.Right with 
            | right when isError right -> right 
            | right -> evalPrefixExpression prefixExp.Operator right

      | :? InfíxExpression as infixExp -> 
            match eval infixExp.Left with
            | left when isError left -> left
            | left -> 
                match eval infixExp.Right with
                | right when isError right -> right
                | right -> evalInfixExpression infixExp.Operator left right
                  
      | :? ReturnStatement as returnStmt -> 
            match eval returnStmt.ReturnValue with 
            | value when isError value -> value 
            | value -> ReturnValue(value) :> Object  
      
      | _ -> NULL
        
