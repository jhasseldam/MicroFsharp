module CILListBuilder
open System
open CILClassBuilder
open CILMethodBuilder
open System.Reflection.Emit
open System.Reflection

let callStandard = CallingConventions.Standard
let callHasThis = CallingConventions.HasThis
let pub = MethodAttributes.Public
let pubStatic = pub ||| MethodAttributes.Static
let pubStaticHide = pubStatic ||| MethodAttributes.HideBySig

let defineConsCTor (consClass : TypeBuilder) listClass (listCtor : ConstructorBuilder)
     (valFld : FieldBuilder) (nextFld : FieldBuilder) = 
    let inTypes = [|typeof<int>; listClass.GetType()|]
    let consConstruct = consClass.DefineConstructor(pub, callStandard, inTypes)
    consConstruct.DefineParameter(1, ParameterAttributes.None, "value") |> ignore
    consConstruct.DefineParameter(2, ParameterAttributes.None, "next") |> ignore
    let ilGen = consConstruct.GetILGenerator()
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Call, listCtor)
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Ldarg_1)
    ilGen.Emit(OpCodes.Stfld, valFld)
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Ldarg_2)
    ilGen.Emit(OpCodes.Stfld, nextFld)
    ilGen.Emit(OpCodes.Ret)
    consConstruct

let defineDefaultCTor (targetClass : TypeBuilder) = 
    targetClass.DefineDefaultConstructor(MethodAttributes.Public)

let defineEmptyCTor (targetClass : TypeBuilder) = 
    let cTor = targetClass.DefineConstructor(pub, callStandard, [||])
    let ilGen = cTor.GetILGenerator()
    ilGen.Emit(OpCodes.Ret)
    cTor

let createIsTypeMethod (sourceClass : TypeBuilder) (compareClass : TypeBuilder) name = 
    let checkTypeMethod = sourceClass.DefineMethod(name, pub, callHasThis, typeof<bool>, [||])
    let ilGen = checkTypeMethod.GetILGenerator()
    ilGen.Emit(OpCodes.Ldarg_0) 
    ilGen.Emit(OpCodes.Isinst, compareClass)
    ilGen.Emit(OpCodes.Ldnull)
    ilGen.Emit(OpCodes.Cgt_Un)
    ilGen.Emit(OpCodes.Ret)

let createNewNilMethod (listClass : TypeBuilder) (nilClassCTor : ConstructorBuilder) = 
    let newNilMethod = listClass.DefineMethod("Nil", pubStatic, callStandard, listClass, [||])
    let ilGen = newNilMethod.GetILGenerator()
    ilGen.Emit(OpCodes.Newobj, nilClassCTor)
    ilGen.Emit(OpCodes.Ret)

let createNewConsMethod (listClass : TypeBuilder) (consClassCTor : ConstructorBuilder) =
    let inTypes = [|typeof<int>; listClass.GetType()|]
    let newConsMethod = 
        listClass.DefineMethod("Cons", pubStaticHide, callStandard, listClass, inTypes)
    newConsMethod.DefineParameter(1, ParameterAttributes.None, "value") |> ignore
    newConsMethod.DefineParameter(2, ParameterAttributes.None, "next") |> ignore
    let ilGen = newConsMethod.GetILGenerator()
    ilGen.DeclareLocal(listClass) |> ignore
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Ldarg_1)
    ilGen.Emit(OpCodes.Newobj, consClassCTor)
    ilGen.Emit(OpCodes.Stloc_0)
    ilGen.Emit(OpCodes.Ldloc_0)
    ilGen.Emit(OpCodes.Ret)

let createListStructure (mb : ModuleBuilder) =
    // Create MFLIst class
    let listClass = defineSuperClass mb "MFList"
    let listClassCTor = defineDefaultCTor listClass
    // Create Cons class
    let consClass = defineSubClass mb "Cons" listClass
    let valueField = addField consClass "value" typeof<int>
    let nextField = addField consClass "next" listClass
    let consClassCTor = defineConsCTor consClass listClass listClassCTor valueField nextField
    // Crteate Nil class    
    let nilClass = defineSubClass mb "Nil" listClass
    let nilClassCTor = defineEmptyCTor nilClass
    // Populate MFList class
    createIsTypeMethod listClass consClass "IsCons"
    createIsTypeMethod listClass nilClass "IsNil"
    createNewNilMethod listClass nilClassCTor
    createNewConsMethod listClass consClassCTor
    createGetSubFieldMethod listClass consClass valueField "GetValue"
    createGetSubFieldMethod listClass consClass nextField "GetNext"
    // Create types
    let listType = listClass.CreateType()
    consClass.CreateType() |> ignore
    nilClass.CreateType() |> ignore
    listType

let buildListType mb = 
    createListStructure mb