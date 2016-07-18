module MFCompiler
open Ast
open MFIL
open Util

type InvokeType = InvokeType of string * TypeDef * TypeDef

let rec getInvokeTypes parameters funTypeDef = 
    match parameters, funTypeDef with
    | ([], _) -> []
    | (p::ps, FuncType(left, right)) 
        -> InvokeType(p, left, right) :: getInvokeTypes ps right
    | _ -> failwith "Illegal function type"

let rec getILType funcType = 
    match funcType with
    | IntType -> ILInt
    | BoolType -> ILBool
    | ListType -> ILList
    | TreeType -> ILTree
    | FuncType (left, right) -> ILFunc(getILType left, getILType right)

let rec compileExpr expr =
    match expr with
    | ConstInt i  -> [PushInt i]
    | ConstBool b -> [PushBool b]
    | Var name -> [Load name] 
    | PrimOp ("+", lOper, rOper) -> compileNumExpr Add lOper rOper
    | PrimOp ("%", lOper, rOper) -> compileNumExpr Mod lOper rOper
    | PrimOp ("-", lOper, rOper) -> compileNumExpr Sub lOper rOper
    | PrimOp ("*", lOper, rOper) -> compileNumExpr Mul lOper rOper
    | PrimOp ("/", lOper, rOper) -> compileNumExpr Div lOper rOper
    | PrimOp (">", lOper, rOper) -> compileNumExpr Gt lOper rOper
    | PrimOp ("<", lOper, rOper) -> compileNumExpr Lt lOper rOper
    | PrimOp ("=", lOper, rOper) -> compileNumExpr Eq lOper rOper
    | PrimOp ("<>", lOper, rOper) -> compileNumExpr Neq lOper rOper
    | PrimOp ("<=", lOper, rOper) -> compileNumExpr Le lOper rOper
    | PrimOp (">=", lOper, rOper) -> compileNumExpr Ge lOper rOper
    | FunCall (funName, TypedExpr(arg, argType)) 
        -> compileFunCall funName arg argType false
    | TailFunCall (funName, TypedExpr(arg, argType)) // Tail recursive function call
        -> compileFunCall funName arg argType true
    | If (ifExpr, thenExpr, elseExpr ) 
        -> compileITE ifExpr thenExpr elseExpr
    | Match (TypedExpr(expr, exprType), matchCases) 
        -> compileMatchCases expr exprType matchCases
    | Constructor(cTor) -> compileConstructor cTor
    | Let(name, TypedExpr(valExpr, valType), restExpr) 
        -> compileLetExpr name valType valExpr restExpr
    | TypedExpr(expr, _) -> compileExpr expr
    | _ -> failwith "Syntax error"  

and compileConstructor constructor' =
    match constructor' with
    | Nil -> [CallConstructVariant(ILNil)]
    | Cons(valExpr, listExpr) -> 
        CallConstructVariant(ILCons) ::(compileExpr listExpr @ compileExpr valExpr) 
    | Leaf -> [CallConstructVariant(ILLeaf)]
    | Node(valExpr, leftExpr, rightExpr) -> 
        let cTor = CallConstructVariant(ILNode)
        let rightInstrs = compileExpr rightExpr
        let leftInstrs = compileExpr leftExpr
        let valInstrs = compileExpr valExpr
        cTor::(rightInstrs @ leftInstrs @ valInstrs)

and compileListMatch matchExpr matchExprType valName restName consAction nilAction =
    let result = compileExpr matchExpr 
    let name = createRandomName "match"
    let storeResult = StoreLocal (name, getILType matchExprType) :: result
    let loadResult = Load name
    let isNilInstr = CallCheckVariant(ILNil)
    let nilBranchInstrs = compileExpr nilAction |> List.rev
    let bindValue = [loadResult; ExtractVariantVal(ListValue); StoreLocal(valName, ILInt)]
    let bindNext = [loadResult; ExtractVariantVal(Next); StoreLocal(restName, ILList)]
    let consActionInstrs = compileExpr consAction |> List.rev
    let consBranchInstrs = bindNext @ bindValue @ consActionInstrs
    [Branch(nilBranchInstrs, consBranchInstrs); isNilInstr; loadResult] @ storeResult

and compileTreeMatch matchExpr matchExprType valName leftName rightName nodeAction leafAction = 
    let name = createRandomName "match"
    let result = compileExpr matchExpr 
    let storeResult = StoreLocal (name, getILType matchExprType) :: result
    let loadResult = Load name
    let result = compileExpr matchExpr 
    let isLeafInstrs = CallCheckVariant(ILLeaf)
    let leafBranchInstrs = compileExpr leafAction |> List.rev
    let bindValue = [loadResult; ExtractVariantVal(TreeValue); StoreLocal(valName, ILInt)]
    let bindLeft = [loadResult; ExtractVariantVal(Left); StoreLocal(leftName, ILTree)]
    let bindRight = [loadResult; ExtractVariantVal(Right); StoreLocal(rightName, ILTree)]
    let nodeActionInstrs = compileExpr nodeAction |> List.rev
    let nodeBranchInstrs = bindLeft @ bindRight @ bindValue @ nodeActionInstrs
    [Branch(leafBranchInstrs, nodeBranchInstrs); isLeafInstrs; loadResult] @ storeResult

and compileMatchCases expr exprType matchCases = 
    match matchCases with
    | [(ConsMatch(valueName, listName), consAction); (NilMatch, nilAction)] 
        -> compileListMatch expr exprType valueName listName consAction nilAction
    | [(NilMatch, nilAction); (ConsMatch(valueName, listName), consAction)] 
        -> compileListMatch expr exprType valueName listName consAction nilAction
    | [(NodeMatch(valName, leftName, rightName), nodeAction); (LeafMatch, leafAction)] 
        -> compileTreeMatch expr exprType valName leftName rightName nodeAction leafAction
    | [(LeafMatch, leafAction); (NodeMatch(valName, leftName, rightName), nodeAction)] 
        -> compileTreeMatch expr exprType valName leftName rightName nodeAction leafAction
    | _ -> failwith "An error occured in compiling match expression"

and compileNumExpr op lOper rOper =
    let left = compileExpr  lOper
    let right = compileExpr  rOper
    op :: (right @ left)

and compileLetExpr name typeDef valExpr restExpr =
    let valInstr = compileExpr valExpr
    let storeInstr = StoreLocal(name, (getILType typeDef))
    let restInstr = compileExpr restExpr
    restInstr @ (storeInstr :: valInstr)

and compileITE ifExpr thenExpr elseExpr = 
    let ifInstr = compileExpr  ifExpr
    let thenInstr = compileExpr thenExpr |> List.rev
    let elseInstr = compileExpr  elseExpr |> List.rev
    Branch (thenInstr, elseInstr) :: ifInstr

and compileFunCall name expr exprType tailRecursive =
    let argument = compileExpr expr |> List.rev
    match name, exprType with 
    | (Var "print"), BoolType -> Print ILBool :: argument
    | (Var "print"), IntType  -> Print ILInt :: argument
    | (Var "printAscii"), _   -> PrintAscii :: argument
    | (Var funName), _        -> [CallInvoke (funName, tailRecursive, argument)]
    | _                       -> failwith "Syntax error: expected (Var name)"   
   
let rec dupFieldInstrs className fields loadObjRef = 
    match fields with
    | field::rest -> 
        let (Field(fieldName,_)) = field
        let loadValue = Load(fieldName)
        let storeField = StoreField(className, fieldName)
        loadObjRef::loadValue::storeField::dupFieldInstrs className rest loadObjRef
    | [] -> []

let genFunClassName funName lvl =
    if lvl = 0 then funName else funName + "_step" + string lvl

let getILTypes input output = 
    (getILType input, getILType output)

let createStepClassInstrs invokeType nextLvlName nextLvlFields =
    let (InvokeType(paramName, inType, outType)) = invokeType
    let localIlType = getILType (FuncType(inType, outType))
    let newObj = CreateObject(nextLvlName)
    let tempName = createRandomName "local"
    let storeObjRef = StoreLocal(tempName, localIlType)
    let loadObjRef = Load(tempName)
    let copyFields = dupFieldInstrs nextLvlName nextLvlFields loadObjRef
    newObj::storeObjRef::copyFields@[loadObjRef]

let compileFunDef funName invokeTypes expr = 
    //printfn "Tail Recursive: %A" (tailRecursive expr)
    let rec buildFunClasses remInvokeTypes reqFields lvl = 
        match remInvokeTypes with
        | nextParam::[] -> 
            let (InvokeType(paramName, inType, outType)) = nextParam 
            let (inILType, outILType) = getILTypes inType outType
            let stepClassName = genFunClassName funName lvl
            let funcInstrs = compileExpr expr |> List.rev
            [FunClass(stepClassName, paramName, (inILType, outILType), reqFields, funcInstrs)]
        | nextParam::rest -> 
            let (InvokeType(paramName, inType, outType)) = nextParam
            let classes = buildFunClasses rest (Field(paramName, getILType inType)::reqFields) (lvl+1)
            let (FunClass(className, _, _, nextStepFields, _)) =  List.head classes
            let (inILType, outILType) = getILTypes inType outType
            let stepClassName = genFunClassName funName lvl
            let instrs = createStepClassInstrs nextParam className nextStepFields
            FunClass(stepClassName, paramName, (inILType, outILType), reqFields, instrs)::classes
    buildFunClasses invokeTypes List.empty 0 |> List.rev

let rec compileFunDefs funcDefs = 
    match funcDefs with 
    | nextFun::rest -> let (FuncDef(name, funcParams, typeDef, expr)) = nextFun
                       let invokeTypes = getInvokeTypes funcParams typeDef
                       (compileFunDef name invokeTypes expr) @ compileFunDefs rest 
    | [] -> []

let compileProgram program =
    match program with
    | Program (funcDefs, expr) -> 
        let evaluatedFuncDefs = compileFunDefs funcDefs
        let entryBodyInstrs = compileExpr expr |> List.rev
        evaluatedFuncDefs @ [EntryClass("Program", entryBodyInstrs)]

let compile program = 
    compileProgram program