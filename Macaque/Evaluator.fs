namespace Macaque

open Macaque
open Macaque.Objects
open Macaque.Ast

module rec Evaluator =
    
    type EObject = 
        | Err of Object
        | Ok of Object

    let NULL = Null() :> Object
    let TRUE = Boolean(true) :> Object
    let FALSE = Boolean(false) :> Object
    
    let inline newError message = Error(message) :> Object
    
    let inline exactlyOne (args: Object array): EObject =
        match args with
        | [| arg |] -> arg |> Ok
        | _ -> (sprintf "wrong number of arguments. got=%i, want=1" args.Length) |> newError |> Err

    let len (args: Object array): Object =                
       match exactlyOne args with
       | Err err -> err
       | Ok arg -> 
            match arg with
            | :? ``String`` as sl -> Integer(int64 sl.Value.Length) :> Object
            | :? Array as arr -> Integer(int64 arr.Elements.Length) :> Object
            | other -> newError(sprintf "argument to `len` not supported, got %O" other.Type)
       
                
    let first (args: Object array): Object =
        match exactlyOne args with
        | Err err -> err
        | Ok arg -> 
            match arg with
            | :? Array as arr -> match arr.Elements |> Array.tryHead with Some(head) -> head | None -> NULL
            | _ -> newError (sprintf "argument to `first` must be ARRAY, got %O" arg.Type)            
        
    let last (args: Object array): Object =
        match exactlyOne args with
        | Err err -> err
        | Ok arg -> 
            match arg with
            | :? Array as arr -> match arr.Elements |> Array.tryLast with Some(lastElem) -> lastElem | None -> NULL
            | _ -> newError (sprintf "argument to `last` must be ARRAY, got %O" arg.Type)            

    let rest (args: Object array): Object =
        match exactlyOne args with
        | Err err -> err
        | Ok arg -> 
            match arg with
            | :? Array as arr -> Array(if arr.Elements.Length > 0 then arr.Elements |> Array.tail else Array.empty) :> Object
            | _ -> newError (sprintf "argument to `rest` must be ARRAY, got %O" arg.Type)        

    let push (args: Object array): Object =
        if args.Length <> 2 then 
            (sprintf "wrong number of arguments. got=%i, want=2" args.Length) |> newError         
        else match args.[0] with
             | :? Array as arr -> Array(Array.append arr.Elements [| args.[1] |]) :> Object
             | other -> (sprintf "argument to `push` must be ARRAY, got %O" other.Type) |> newError                     


    let builtIns = Map [ 
        nameof len,   Builtin(len); 
        nameof first, Builtin(first);
        nameof last,  Builtin(last);
        nameof rest,  Builtin(rest);
        nameof push,  Builtin(push); ]
         
    let (|IsError|_|) (object: Object) = if object.Type = ERROR then Some(object) else None

    let evale (node: Node) (env: Environment) : EObject = 
        match eval node env with
        | obj when obj.Type = ERROR -> Err obj
        | obj -> Ok obj        

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
    
    let evalStringInfixExpression (operator: string) (left: ``String``) (right: ``String``) =
        match operator with
        "+" -> ``String``(left.Value + right.Value) :> Object
        | "==" -> nativeBoolToBooleanObject(left.Value = right.Value)
        | "!=" -> nativeBoolToBooleanObject(left.Value <> right.Value)
        | _ -> newError (sprintf "unknown operator: %O %s %O" ((left :> Object).Type) operator ((left :> Object).Type))

    let evalInfixExpression (operator: string) (left: Object) (right: Object) = 
        match left, right with
        | (l, r) when l.Type <> r.Type -> newError (sprintf "type mismatch: %O %s %O" (l.Type) operator (r.Type))
        | (:? Integer as leftInt), (:? Integer as rightInt) -> evalIntegerInfixExpression operator leftInt rightInt 
        | (:? ``String`` as leftStr), (:? ``String`` as rightStr) -> evalStringInfixExpression operator leftStr rightStr
        | _ -> match operator with
               | "==" -> nativeBoolToBooleanObject(left = right)
               | "!=" -> nativeBoolToBooleanObject(left <> right)
               | _ -> newError (sprintf "unknown operator: %O %s %O" (left.Type) operator (right.Type))
    
    let inline isTruthy(object: Object): bool = 
        if object = NULL then false
        else if object = TRUE then true
        else if object = FALSE then false
        else true           
               
    let evalIfExpresion (ie: IfExpression) (env: Environment) =
        match evale ie.Condition env with
        | Err err -> err
        | Ok cond -> 
            if isTruthy cond then 
                eval ie.Consequence env
            else if ie.Alternative.IsSome then 
                eval ie.Alternative.Value env
            else NULL

    let evalIdentifier (node: Identifier) (env: Environment): Object =
        match env.Get(node.Value) with
        | Some(value) -> value
        | None -> 
            match builtIns.TryFind(node.Value) with
            | Some(builtin) -> builtin :> Object
            | None -> newError(sprintf "identifier not found: %s" node.Value)

    let inline evalInfexExpression (infixExp: InfíxExpression) (env: Environment): Object = 
        match evale infixExp.Left env with
        | Err err -> err
        | Ok left -> 
            match evale infixExp.Right env with
            | Err err -> err
            | Ok right -> evalInfixExpression infixExp.Operator left right
                                                  
    let evalExpressions (exps: Expression list) (env: Environment): EObject list =        
        let rec evalAll (expressions) = 
            expressions |> function
            | exp :: rest ->
                match evale exp env with 
                | Err err -> [Err err] 
                | Ok evaluated ->  match evalAll rest with | [Err err] -> [Err err] | res -> (Ok evaluated) :: res 
            | [] -> []        
        
        evalAll exps 
        
    let unwrapReturnValue (obj: Object): Object = 
        match obj with 
        | :? ReturnValue as returnValue -> returnValue.Value
        | _ -> obj

    let extendFunctionEnv (fn: Function) (args: Object list): Environment = 
        let env = Environment(Some(fn.Env))
      
        let rec loop (prms: Identifier list) (args: Object list) =
            match prms, args with
            | prm :: restParams, arg :: restArg -> 
                env.Set(prm.Value, arg)
                loop restParams restArg
            | _ -> ()
                 
        loop fn.Parameters args

        env

    let applyFunction (fn: Object) (args: Object list) =
        match fn with
        | :? Function as func ->                                 
            let extendedEnv = extendFunctionEnv func args
            eval func.Body extendedEnv |> unwrapReturnValue
        | :? Builtin as builtin ->
            let func = builtin.Fn
            func(args |> List.toArray)
        | _ -> newError (sprintf "not a function: %O" fn.Type)

    let evalCallExpression (call: CallExpression) (env: Environment): Object =
        match evale call.Function env with
        | Err err -> err
        | Ok fn ->             
            match evalExpressions call.Arguments env with
            | [Err err] -> err
            | args -> applyFunction fn (args |> List.map (function Ok o -> o | Err e -> e))
           
    let evalArrayIndexExpression (array: Array) (index: Integer): Object =
        let max = array.Elements.Length - 1
        let idx = int index.Value
        if idx < 0 || idx > max then NULL else array.Elements.[idx]
        
    let evalIndexExpression (left: Object) (index: Object): Object =
        match left.Type, index.Type with 
        | ARRAY, INTEGER -> evalArrayIndexExpression (left :?> Array) (index :?> Integer)
        | _ -> newError(sprintf "index operator not supported: %O" left.Type)

    let rec eval (node: Node) (env: Environment): Object = 
      match node with
      | :? Program as program -> evalProgram program env
      
      | :? ExpressionStatement as expressionStmt -> eval expressionStmt.Expression env
      
      | :? IntegerLiteral as integer -> Integer(integer.Value) :> Object 
      
      | :? StringLiteral as str -> ``String``(str.Value) :> Object
      
      | :? BooleanExpression as boolean -> if boolean.Value then TRUE else FALSE
      
      | :? BlockStatement as blockStmt -> evalBlockStatement blockStmt env
      
      | :? IfExpression as ifExp -> evalIfExpresion ifExp env
      
      | :? PrefixExpression as prefixExp -> 
            match evale prefixExp.Right env with 
            | Err err -> err
            | Ok right -> evalPrefixExpression prefixExp.Operator right

      | :? ReturnStatement as returnStmt -> 
            match evale returnStmt.ReturnValue env with
            | Err err -> err 
            | Ok value -> ReturnValue(value) :> Object
            
      | :? InfíxExpression as infixExp -> evalInfexExpression infixExp env            
      
      | :? LetStatement as letStmt -> 
            match evale letStmt.Value env with 
            | Err err -> err
            | Ok value -> env.Set(letStmt.Name.Value, value); NULL      

      | :? Identifier as ident -> evalIdentifier ident env
      
      | :? FunctionLiteral as funLiteral -> Function(funLiteral.Parameters, funLiteral.Body, env) :> Object
      
      | :? CallExpression as callExpression -> evalCallExpression callExpression env
      
      | :? ArrayLiteral as arrLiteral ->
            match evalExpressions (Array.toList arrLiteral.Elements) env with
            | [Err err] -> err
            | res -> Array(res |> List.map(function Ok o -> o | Err e -> e) |> List.toArray) :> Object

      | :? IndexExpression as indexExp ->
            match evale indexExp.Left env with
            | Err err -> err
            | Ok left -> 
                match evale indexExp.Index env with
                | Err err -> err
                | Ok index -> evalIndexExpression left index
                 
      | _ -> NULL
        
