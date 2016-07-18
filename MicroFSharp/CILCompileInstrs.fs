module CILCompileInstrs

open System.Reflection.Emit
open Env
open MFIL
open System
open CILTyping

let rec compileInstrs meta ilGen instrs = 
    List.fold (fun meta value -> compileInstr meta ilGen value) meta instrs |> ignore

and compileInstr meta (ilGen : ILGenerator) instr = 
    match instr with
    | PushInt i -> ilGen.Emit(OpCodes.Ldc_I4, i); meta
    | PushBool b -> if b then ilGen.Emit(OpCodes.Ldc_I4_1); meta 
                    else ilGen.Emit(OpCodes.Ldc_I4_0); meta
    | Add -> ilGen.Emit(OpCodes.Add); meta
    | Sub -> ilGen.Emit(OpCodes.Sub); meta
    | Mod -> ilGen.Emit(OpCodes.Rem); meta
    | Div -> ilGen.Emit(OpCodes.Div); meta
    | Mul -> ilGen.Emit(OpCodes.Mul); meta 
    | Eq -> ilGen.Emit(OpCodes.Ceq); meta
    | Neq -> compileNotEqual ilGen; meta 
    | Gt -> ilGen.Emit(OpCodes.Cgt); meta
    | Lt -> ilGen.Emit(OpCodes.Clt); meta
    | Le -> compileLessOrEqual ilGen; meta
    | Ge -> compileGreaterOrEqual ilGen; meta
    | Load name -> compileLoad meta ilGen name; meta
    | StoreLocal (name, ilType)-> compileStoreLocal meta ilGen name ilType
    | PrintAscii -> 
        ilGen.EmitCall(OpCodes.Call, typeof<Console>.GetMethod("Write", [| typeof<char> |]), null)
        ilGen.Emit(OpCodes.Ldc_I4_0)
        meta
    | Print ilType -> 
        let sysType = concretizeType meta.preDefTypes ilType
        ilGen.EmitCall(OpCodes.Call, typeof<Console>.GetMethod("Write", [| sysType |]), null)
        ilGen.Emit(OpCodes.Ldc_I4_0)
        meta
    | Branch(a, b) -> compileBranch meta ilGen a b; meta
    | CallInvoke(name, tailRecursive, instrs) -> compileInvokeCall meta ilGen name tailRecursive instrs; meta
    | CreateObject name -> compileCreateObj meta ilGen name; meta
    | StoreField (className, fieldName) -> compileStoreField meta ilGen className fieldName; meta
    | ExtractVariantVal variant -> compileExtractVariantVal ilGen meta variant; meta
    | CallCheckVariant variant -> compileCallCheckVariant ilGen meta variant; meta 
    | CallConstructVariant variant -> compileCallConstructVariant ilGen meta variant; meta

// x >= y -> x < y = false
and compileGreaterOrEqual ilGen = 
    ilGen.Emit(OpCodes.Clt)
    ilGen.Emit(OpCodes.Ldc_I4_0)
    ilGen.Emit(OpCodes.Ceq)

// x <= y -> x > y = false
and compileLessOrEqual ilGen =
    ilGen.Emit(OpCodes.Cgt)
    ilGen.Emit(OpCodes.Ldc_I4_0)
    ilGen.Emit(OpCodes.Ceq)

// x = y  -> (x = y) = false       
and compileNotEqual ilGen =
    ilGen.Emit(OpCodes.Ceq)
    ilGen.Emit(OpCodes.Ldc_I4_0)
    ilGen.Emit(OpCodes.Ceq) 
     
and compileStoreField meta ilGen className fieldName =
    let { classBuilder = typeBuilder } = Map.find className meta.classInfoMap
    let fieldInfo = typeBuilder.GetField(fieldName)
    ilGen.Emit(OpCodes.Stfld, fieldInfo)

and compileCreateObj meta ilGen name =
    let { ctorBuilder = ctor } = Map.find name meta.classInfoMap
    ilGen.Emit(OpCodes.Newobj, ctor)

and compileInvokeCall meta ilGen name tailCall instrs =  
    let element = Env.tryGetElement name meta.currentEnv
    match element with
    | Some(Env.Local(index, ilType)) -> ilGen.Emit(OpCodes.Ldloc, index)
                                        compileInstrs meta ilGen instrs
                                        let intfType = concretizeType meta.preDefTypes ilType
                                        if tailCall then ilGen.Emit(OpCodes.Tailcall)
                                        ilGen.Emit(OpCodes.Callvirt, intfType.GetMethod("Invoke"))
                                        if tailCall then ilGen.Emit(OpCodes.Ret)
                                     
    | Some(Env.Arg(index, ilType)) -> ilGen.Emit(OpCodes.Ldarg, index)
                                      compileInstrs meta ilGen instrs
                                      let intfType = concretizeType meta.preDefTypes ilType
                                      ilGen.Emit(OpCodes.Callvirt, intfType.GetMethod("Invoke"))
                                      
    
    | Some(Env.Field(fieldInfo, ilType)) -> ilGen.Emit(OpCodes.Ldarg_0)
                                            ilGen.Emit(OpCodes.Ldfld, fieldInfo)    
                                            compileInstrs meta ilGen instrs
                                            let intfType = concretizeType meta.preDefTypes ilType
                                            ilGen.Emit(OpCodes.Callvirt, intfType.GetMethod("Invoke"))
    
    | None -> let { classType = sysType; ctorBuilder = ctor } = Map.find name meta.classInfoMap
              ilGen.Emit(OpCodes.Newobj, ctor)
              compileInstrs meta ilGen instrs
              if tailCall then ilGen.Emit(OpCodes.Tailcall)
              ilGen.Emit(OpCodes.Callvirt, sysType.GetMethod("Invoke"))
              if tailCall then ilGen.Emit(OpCodes.Ret)

and compileLoad meta ilGen name = 
    let element = Env.tryGetElement name meta.currentEnv
    match element with
    | Some(Env.Arg (index, _)) -> ilGen.Emit(OpCodes.Ldarg, index)
    | Some(Env.Local(index, _)) -> ilGen.Emit(OpCodes.Ldloc, index)
    | Some(Env.Field(fieldInfo, _)) -> ilGen.Emit(OpCodes.Ldarg_0) // Load "this" pointer
                                       ilGen.Emit(OpCodes.Ldfld, fieldInfo)
    | None -> compileCreateObj meta ilGen name

and compileStoreLocal meta ilGen name ilType = 
    let (updatedEnv, index) = Env.addLocal name ilType meta.currentEnv
    let localType = concretizeType meta.preDefTypes ilType
    ilGen.DeclareLocal(localType) |> ignore
    ilGen.Emit(OpCodes.Stloc, index) |> ignore
    { meta with currentEnv = updatedEnv }

and compileBranch meta ilGen thenInstr elseInstr = 
    let afterThenLabel = ilGen.DefineLabel()
    ilGen.Emit(OpCodes.Brfalse, afterThenLabel)
    compileInstrs meta ilGen thenInstr |> ignore
    let afterElseLabel = ilGen.DefineLabel()
    ilGen.Emit(OpCodes.Br, afterElseLabel)
    ilGen.MarkLabel(afterThenLabel)
    compileInstrs meta ilGen elseInstr |> ignore
    ilGen.MarkLabel(afterElseLabel)

and compileExtractVariantVal ilGen meta variantVal =
    let loadFld (variantType : Type) name = ilGen.Emit(OpCodes.Call, variantType.GetMethod(name))
    match variantVal with
    | ListValue -> loadFld meta.preDefTypes.listType "GetValue"
    | TreeValue -> loadFld meta.preDefTypes.treeType "GetValue"
    | Next ->  loadFld meta.preDefTypes.listType "GetNext"
    | Left ->  loadFld meta.preDefTypes.treeType "GetLeft"
    | Right -> loadFld meta.preDefTypes.treeType "GetRight"

and compileCallCheckVariant ilGen meta variant = 
    let callCheck (variantType : Type) name = ilGen.Emit(OpCodes.Call, variantType.GetMethod(name))
    match variant with
    | ILNil     -> callCheck meta.preDefTypes.listType "IsNil"
    | ILCons    -> callCheck meta.preDefTypes.listType "IsCons"
    | ILLeaf    -> callCheck meta.preDefTypes.treeType "IsLeaf"
    | ILNode    -> callCheck meta.preDefTypes.treeType "IsNode"

and compileCallConstructVariant ilGen meta variant = 
    let callConstruct (variantType : Type) name = ilGen.Emit(OpCodes.Call, variantType.GetMethod(name))
    match variant with
    | ILNil     -> callConstruct meta.preDefTypes.listType "Nil"
    | ILCons    -> callConstruct meta.preDefTypes.listType "Cons"
    | ILLeaf    -> callConstruct meta.preDefTypes.treeType "Leaf"
    | ILNode    -> callConstruct meta.preDefTypes.treeType "Node"