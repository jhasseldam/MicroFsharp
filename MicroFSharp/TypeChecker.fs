module TypeChecker
open Ast
open Util
open System


let isTailRecursive expr funName = 
    let rec traverseAst expr = 
        match expr with
        | TypedExpr(ex, _) -> traverseAst ex
        | PrimOp(_, lOper, rOper) -> noFunCallExpr lOper && noFunCallExpr rOper
        | Constructor(ex) ->
            match ex with
            | Cons(ex1, ex2) -> noFunCallExpr ex1 && noFunCallExpr ex2
            | Node(ex1, ex2, ex3) -> noFunCallExpr ex1 && noFunCallExpr ex2 && noFunCallExpr ex3
            | Nil -> true
            | Leaf -> true
        | If(ifEx, thenEx, elseEx) -> traverseAst ifEx && traverseAst thenEx && traverseAst elseEx
        | Match(ex, cases) -> traverseAst ex && List.forall (fun (_, e) -> traverseAst e) cases
        | Let(_, valEx, bodyEx) -> traverseAst valEx && traverseAst bodyEx
        | _ -> true

    and noFunCallExpr expr = 
        match expr with
        | TypedExpr(ex, _) -> noFunCallExpr ex
        | If(ifEx, thenEx, elseEx) -> noFunCallExpr ifEx && noFunCallExpr thenEx && noFunCallExpr elseEx
        | Match(ex, cases) -> noFunCallExpr ex && List.forall (fun (_, e) -> noFunCallExpr e) cases
        | Constructor(ex) -> 
            match ex with
            | Cons(ex1, ex2) -> noFunCallExpr ex1 && noFunCallExpr ex2
            | Node(ex1, ex2, ex3) -> noFunCallExpr ex1 && noFunCallExpr ex2 && noFunCallExpr ex3
            | Nil -> true
            | Leaf -> true
        | Let(_, valEx, bodyEx) -> noFunCallExpr valEx && noFunCallExpr bodyEx
        | FunCall(Var name, _) -> name <> funName // Recursive call here can't be tail recursive
        | FunCall(innerCall, _) -> noFunCallExpr innerCall
        | _ -> true
    traverseAst expr

let printFailArg msg arg =
    printfn msg arg 
    failwith "Type-check error"

let printFail msg =
    printfn msg
    failwith "Type-check error"

let addFunEnv env funcDef = 
    match funcDef with
    | FuncDef(name, _, typeDef, _) -> Map.add name typeDef env

let getOutputType env funName = 
    let funType = Map.find funName env
    match funType with
    | FuncType(input, output) -> output
    | _ -> printFailArg "Wrong output type for: %A" funName

let getInputType env funName = 
    let funType = Map.find funName env
    match funType with
    | FuncType(input, output) -> input
    | _ ->  printFailArg "Wrong input type for: %A" funName

let addParamType env param = 
    let (Parameter(name, paramType)) = param
    Map.add name paramType env

let rec flattenCall innerCall = 
    match innerCall with
    | FunCall((Var _), _) -> innerCall
    | FunCall(innerCall, argExpr) ->
       let inner = flattenCall innerCall
       let outerName = createRandomName "fun"
       let outerCall = FunCall((Var outerName), argExpr)
       Let(outerName, inner, outerCall)

// Finds the function name of a function call
let rec getFunCallName expr = 
    match expr with
    | FunCall((Var name), _) -> name
    | FunCall(innerExpr, _) -> getFunCallName innerExpr

// Finds the outer most let name in flattened funcall
let outerName expr = 
    match expr with
    | Let(name, _, _) -> name

let checkExpr env expr funName tailRecursive =
    let rec enrichExpr env expr flatFunName = 
        match expr with
        | ConstBool b -> (BoolType, TypedExpr(expr, BoolType))
        | ConstInt i -> (IntType, TypedExpr(expr, IntType))
        | Var name -> let varType = Map.tryFind name env
                      match varType with
                      | Some(varType) -> (varType, TypedExpr(expr, varType))
                      | None -> printFailArg "Could not find: %A" name
    
        | PrimOp("+", lOper, rOper) -> checkIntOp env lOper rOper "+" flatFunName
        | PrimOp("-", lOper, rOper) -> checkIntOp env lOper rOper "-" flatFunName
        | PrimOp("*", lOper, rOper) -> checkIntOp env lOper rOper "*" flatFunName
        | PrimOp("/", lOper, rOper) -> checkIntOp env lOper rOper "/" flatFunName
        | PrimOp(">", lOper, rOper) -> checkIntOp env lOper rOper ">" flatFunName
        | PrimOp("<", lOper, rOper) -> checkIntOp env lOper rOper "<" flatFunName
        | PrimOp("=", lOper, rOper) -> checkBinOp env lOper rOper "=" flatFunName
        | PrimOp("%", lOper, rOper) -> checkBinOp env lOper rOper "%" flatFunName
        | PrimOp("<>", lOper, rOper) -> checkBinOp env lOper rOper "<>" flatFunName
        | PrimOp("<=", lOper, rOper) -> checkBinOp env lOper rOper "<=" flatFunName
        | PrimOp(">=", lOper, rOper) -> checkBinOp env lOper rOper ">=" flatFunName
        | If(ifExpr, thenExpr, elseExpr) -> checkIf env ifExpr thenExpr elseExpr flatFunName
        | Let(name, valExpr, restExpr) -> checkLet env name valExpr restExpr flatFunName
        | FunCall(_, _) -> checkFunCall env expr flatFunName
        | Match (expr, matchCases)  -> checkMatchCases env expr matchCases flatFunName
        | Constructor(constructor') -> checkConstructor env constructor' flatFunName
        | wrongExpr -> printFailArg "Does not recognize: %A" wrongExpr
    
    and checkConstructor env cTor flatFunName = 
        match cTor with 
        | Cons(x, xs) -> 
            let (valType, valTypedExpr) = enrichExpr env x flatFunName
            let (listType, listTypedExpr) = enrichExpr env xs flatFunName
            if valType <> IntType then printFail "List value must be of type Integer"
            if listType <> ListType then printFail "List item must be of type List"
            (ListType, TypedExpr(Constructor(Cons(valTypedExpr, listTypedExpr)), ListType))
        | Nil -> (ListType, TypedExpr(Constructor(Nil), ListType))
        | Node(value, left, right) -> 
            let (valType, valTypedExpr) = enrichExpr env value flatFunName
            let (leftType, leftTypedExpr) = enrichExpr env left flatFunName
            let (rightType, rightTypedExpr) = enrichExpr env right flatFunName
            if valType <> IntType then printFail "Tree value must be of type Integer"
            if leftType <> TreeType then printFail "Tree item must be og type Tree"
            if rightType <> TreeType then printFail "Tree item must be og type Tree"
            (TreeType, TypedExpr(Constructor(Node(valTypedExpr, leftTypedExpr, rightTypedExpr)), TreeType))
        | Leaf -> (TreeType, TypedExpr(Constructor(Leaf), TreeType))
    
    and checkListMatch env valName restName consActExpr nilActExpr flatFunName =
        let env' = Map.add valName IntType env
        let env'' = Map.add restName ListType env' 
        let (nilActExprType, typedNilActExpr) = enrichExpr env'' nilActExpr flatFunName
        let (consActExprType, typedConsActExpr) = enrichExpr env'' consActExpr flatFunName
        if nilActExprType <> consActExprType then printFail "List match cases must have same type"
        (nilActExprType, [(ConsMatch(valName, restName), typedConsActExpr); (NilMatch, typedNilActExpr)])       
    
    and checkTreeMatch env valName lName rName nodeAction leafAction flatFunName = 
        let env' = Map.add valName IntType env
        let env'' = Map.add lName TreeType env'
        let env''' = Map.add rName TreeType env''
        let (leafExprType, typedLeafAction) = enrichExpr env''' leafAction flatFunName
        let (nodeExprType, typedNodeAction) = enrichExpr env''' nodeAction flatFunName
        if leafExprType <> nodeExprType then printFail "Tree match cases must have same type"
        (leafExprType, [(NodeMatch(valName, lName, rName), typedNodeAction); (LeafMatch, typedLeafAction)])
    
    and checkMatchCases env matchExpr matchCases flatFunName = 
        let (matchExprType, typedMatchExpr) = enrichExpr env matchExpr flatFunName
        match matchCases with
        | [(ConsMatch(valueName, listName), consAction); (NilMatch, nilAction)] -> 
            let (matchType, matchCases) = checkListMatch env valueName listName consAction nilAction flatFunName
            (matchType, TypedExpr(Match(typedMatchExpr, matchCases), matchType))
        | [(NilMatch, nilAction); (ConsMatch(valueName, listName), consAction)] -> 
            let (matchType, matchCases) = checkListMatch env valueName listName consAction nilAction flatFunName
            (matchType, TypedExpr(Match(typedMatchExpr, matchCases), matchType))
        | [(NodeMatch(valName, leftName, rightName), nodeAction); (LeafMatch, leafAction)]  -> 
            let (matchType, matchCases) = checkTreeMatch env valName leftName rightName nodeAction leafAction flatFunName
            (matchType, TypedExpr(Match(typedMatchExpr, matchCases), matchType))
        | [(LeafMatch, leafAction); (NodeMatch(valName, lName, rName), nodeAction)]  -> 
            let (matchType, matchCases) = checkTreeMatch env valName lName rName nodeAction leafAction flatFunName
            (matchType, TypedExpr(Match(typedMatchExpr, matchCases), matchType))
        | wrongMatchCase -> printFailArg "Match case: '%A' not supported" wrongMatchCase
        
    and checkBinOp env left right op flatFunName = 
        let (lType, leftTyped) = enrichExpr env left flatFunName
        let (rType, rigthTyped) = enrichExpr env right flatFunName
        if (not (lType = IntType) && not(lType = BoolType)) && not(lType = rType) 
        then printFailArg "Type error near: %A" op
        (lType, TypedExpr(PrimOp(op, leftTyped, rigthTyped), lType))
    
    and checkIntOp env lOper rOper op flatFunName = 
        let (leftType, leftTypedExpr) = enrichExpr env lOper flatFunName
        let (rightType, rightTypedExpr) = enrichExpr env rOper flatFunName
        if leftType <> IntType || rightType <> IntType 
        then printFailArg "Type error near: %A" op
        (IntType, TypedExpr(PrimOp(op, leftTypedExpr, rightTypedExpr), IntType))
    
    and checkIf env ifExpr thenExpr elseExpr flatFunName = 
        let (ifType, ifTypedExpr) = enrichExpr env ifExpr flatFunName
        let (thenType, thenTypedExpr) = enrichExpr env thenExpr flatFunName
        let (elseType, elseTypedExpr) = enrichExpr env elseExpr flatFunName
        if (ifType <> BoolType && ifType <> IntType) || thenType <> elseType 
        then printFail "Type error in if-then-else expression"
        (thenType, (TypedExpr(If(ifTypedExpr, thenTypedExpr, elseTypedExpr), thenType)))
    
    and checkLet env name valExpr restExpr flatFunName = 
        let (valType, valTypedExpr) = enrichExpr env valExpr flatFunName
        let updatedEnv = Map.add name valType env
        let (restType, restTypedExpr) = enrichExpr updatedEnv restExpr flatFunName
        (restType, TypedExpr(Let(name, valTypedExpr, restTypedExpr), restType))
    
    and checkFunCall env funCall flatFunName = 
        match funCall with
        | FunCall(Var "print", argExpr) -> 
            let (argExprType, argTypedExpr) = enrichExpr env argExpr flatFunName
            if argExprType <> BoolType && argExprType <> IntType 
            then printFail "Incorrent arg type for print function"
            (IntType, TypedExpr(FunCall(Var "print", argTypedExpr), IntType))
        | FunCall(Var "printAscii", argExpr) -> 
            let (argExprType, argTypedExpr) = enrichExpr env argExpr flatFunName
            if argExprType <> IntType 
            then printFail "Incorrent arg type for printAscii function"
            (IntType, TypedExpr(FunCall(Var "printAscii", argTypedExpr), IntType))
        | FunCall(Var funCallName, argExpr) -> 
            let (argExprType, argTypedExpr) = enrichExpr env argExpr flatFunName
            let funOutputType = getOutputType env funCallName
            let funInputType = getInputType env funCallName
            if funInputType <> argExprType 
            then printFailArg "Incorrent arg type near: %A" argExpr
            let funCall = if funCallName = flatFunName && tailRecursive then TailFunCall(Var funCallName, argTypedExpr) else FunCall(Var funCallName, argTypedExpr)
            (funOutputType, TypedExpr(funCall, funOutputType))
            //(funOutputType, TypedExpr(FunCall(Var name, argTypedExpr), funOutputType))
        | _ -> 
            let flatFunCall = flattenCall funCall
            let funCallName = getFunCallName funCall
            let updatedFunName = if funCallName = flatFunName then outerName flatFunCall else flatFunName
            let (flatFunCallType, typedFlatFunCall) = enrichExpr env flatFunCall updatedFunName
            (flatFunCallType, TypedExpr(typedFlatFunCall, flatFunCallType))
    enrichExpr env expr funName

let checkFun env funDef = 
    let (FuncDef(funName, paramList, typeDef, funExpr)) = funDef
    let paramTypes = getParamTypes funName paramList typeDef
    let updatedEnv = List.fold addParamType env paramTypes
    let tailRecursive = isTailRecursive funExpr funName
    let (funExprType, funTypedExpr) = checkExpr updatedEnv funExpr funName tailRecursive
    FuncDef(funName, paramList, typeDef, funTypedExpr)

let checkProgram program = 
    match program with
    | Program(funDefs, mainExpr) -> 
        let env = List.fold addFunEnv Map.empty funDefs
        let checkedFuns = List.map (checkFun env) funDefs
        let (mainExprType, mainTypedExpr) = checkExpr env mainExpr "main" false
        Program(checkedFuns, mainTypedExpr)