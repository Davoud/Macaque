namespace Macaque

open Macaque
open Macaque.Objects
open Macaque.Ast

module rec Evaluator =
    
    let NULL = Null() :> Object
    let TRUE = Boolean(true) :> Object
    let FALSE = Boolean(false) :> Object
    
    let inline newError message = Error(message) :> Object
      
    let evalProgram (program: Program) (env: Environment): Object =                       
        let rec evalAll (statements: Statement list) = 
            match statements with
            | statement :: rest ->
                match eval statement env with
                | :? ReturnValue as r -> Some(r.Value)
                | e when e.Type = ERROR -> Some(e) 
                | last -> match evalAll rest with Some(o) -> Some(o) | None -> Some(last)
            | [] -> None
        
        match evalAll program.Statements with Some(obj) -> obj | None -> NULL

    
    let evalBlockStatement (block: BlockStatement) (env: Environment): Object =  
        let inline isReturnOrError objType = objType = RETURN_VALUE || objType = ERROR
        let rec evalAll (statements: Statement list) =
            match statements with
            | statement :: rest ->
                let result = eval statement env
                if result <> NULL && isReturnOrError (result.Type) then Some(result) 
                else match evalAll rest with Some(o) -> Some(o) | None -> Some(result)
            | [] -> None
        
        match evalAll block.Statements with Some(lastObject) -> lastObject | None -> NULL
    

    let inline nativeBoolToBooleanObject(value: bool): Object = if value then TRUE else FALSE

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
    
    let inline isTruthy(object: Object): bool = 
        if object = NULL then false
        else if object = TRUE then true
        else if object = FALSE then false
        else true        

    let ifNotError (object: Object) (f: Object -> Object): Object = 
        match object with 
        | obj when obj.Type = ERROR -> obj
        | obj -> f obj
               
               
    let evalIfExpresion (ie: IfExpression) (env: Environment) =
        ifNotError (eval ie.Condition env) (fun cond -> 
            if isTruthy cond then eval ie.Consequence env
            else if ie.Alternative.IsSome then eval ie.Alternative.Value env
            else NULL)

    let evalIdentifier (node: Identifier) (env: Environment): Object =
        match env.Get(node.Value) with
        | Some(value) -> value
        | None -> newError(sprintf "identifier not found: %s" node.Value)

    let rec eval (node: Node) (env: Environment): Object = 
      match node with
      | :? Program as program -> evalProgram program env
      | :? ExpressionStatement as expressionStmt -> eval expressionStmt.Expression env
      | :? IntegerLiteral as integer -> Integer(integer.Value) :> Object 
      | :? BooleanExpression as boolean -> if boolean.Value then TRUE else FALSE
      | :? BlockStatement as blockStmt -> evalBlockStatement blockStmt env
      | :? IfExpression as ifExp -> evalIfExpresion ifExp env
      | :? PrefixExpression as prefixExp -> ifNotError (eval prefixExp.Right env) (evalPrefixExpression prefixExp.Operator)
      | :? ReturnStatement as returnStmt -> ifNotError (eval returnStmt.ReturnValue env) (fun value -> ReturnValue(value) :> Object)                              
      | :? InfíxExpression as infixExp -> 
            ifNotError (eval infixExp.Left env) (fun left -> ifNotError (eval infixExp.Right env) (fun right -> evalInfixExpression infixExp.Operator left right))                                                          
      | :? LetStatement as letStmt -> ifNotError (eval letStmt.Value env) (fun value -> env.Set(letStmt.Name.Value, value); value)
      | :? Identifier as ident -> evalIdentifier ident env
      | _ -> NULL
        
