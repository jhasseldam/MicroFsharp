module CILMethodBuilder

open System.Reflection
open System.Reflection.Emit
open CILCompileInstrs
open CILTyping
open MFIL
open Env

let rec addFieldsToEnv env (fields : (FieldBuilder * ILType) list) =
    match fields with
    | (field, ilType)::rest -> let (updatedEnv, _) = addField field.Name field ilType env
                               addFieldsToEnv updatedEnv rest
    | [] -> env

let populateInvokeMethod meta name (methodInfo : MethodBuilder) paramName inILType fields instrs = 
    let populateIntrs ilGen = 
        let argEnv = Env.ofArg paramName inILType
        let argsAndFieldsEnv = addFieldsToEnv argEnv fields
        let updatedMeta = { meta with currentEnv = argsAndFieldsEnv }
        compileInstrs updatedMeta ilGen instrs |> ignore
        ilGen.Emit(OpCodes.Ret)
    populateIntrs (methodInfo.GetILGenerator())

let populateMainMethod meta name (methodInfo : MethodBuilder) instrs =
    let populateIntrs ilGen = 
        let argEnv = Env.empty
        let updatedMeta = { meta with currentEnv = argEnv }
        compileInstrs updatedMeta ilGen instrs |> ignore
        ilGen.Emit(OpCodes.Ret)
    populateIntrs (methodInfo.GetILGenerator())

let createInvokeMethod (tb : TypeBuilder) name paramType returnType param =
    let methodAttributes =
        MethodAttributes.Public ||| MethodAttributes.Virtual |||
        MethodAttributes.HideBySig ||| MethodAttributes.NewSlot ||| MethodAttributes.Final
    let newMethod = tb.DefineMethod(name, methodAttributes, returnType, [|paramType|])
    let paramIndex = 1
    newMethod.DefineParameter(paramIndex, ParameterAttributes.None, name) |> ignore
    newMethod

let defineInvokeMethod (tb : TypeBuilder) meta inILType outILType paramName =
    let name = "Invoke"
    let paramType = concretizeType meta.preDefTypes inILType
    let returnType = concretizeType meta.preDefTypes outILType
    createInvokeMethod tb name paramType returnType paramName

let createMainMethod (tb : TypeBuilder) name =
    let methodAttributes =
        MethodAttributes.Public ||| MethodAttributes.Static
    let newMethod = tb.DefineMethod(name, methodAttributes, typeof<int>, [||])
    newMethod

let defineMainMethod (ab : AssemblyBuilder) (tb : TypeBuilder) meta =
    let name = "Main"
    let newMethod = createMainMethod tb name
    ab.SetEntryPoint(newMethod)    
    newMethod

// The Following are used in creating the built in types: MFList and MFTree
let methodAtrs = MethodAttributes.Public
let callConv = CallingConventions.Standard

let createGetSubFieldMethod (superC : TypeBuilder) (subC : TypeBuilder) (fldInfo : FieldInfo) name = 
    let newMethod = superC.DefineMethod(name, methodAtrs, callConv, fldInfo.FieldType, [||])
    let ilGen = newMethod.GetILGenerator()
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Castclass, subC)
    ilGen.Emit(OpCodes.Ldfld, fldInfo)
    ilGen.Emit(OpCodes.Ret)

let defineDefaultCTor (targetClass : TypeBuilder) = 
    targetClass.DefineDefaultConstructor(methodAtrs)

let defineEmptyCTor (targetClass : TypeBuilder) = 
    let cTor = targetClass.DefineConstructor(methodAtrs, callConv, [||])
    let ilGen = cTor.GetILGenerator()
    ilGen.Emit(OpCodes.Ret)
    cTor