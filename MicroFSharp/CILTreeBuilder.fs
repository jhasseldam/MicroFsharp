module CILTreeBuilder

open Env
open CILClassBuilder
open CILMethodBuilder
open System.Reflection.Emit
open System.Reflection
open System

let callStandard = CallingConventions.Standard
let callHasThis = CallingConventions.HasThis
let pub = MethodAttributes.Public
let pubStatic = pub ||| MethodAttributes.Static
let pubStaticHide = pubStatic ||| MethodAttributes.HideBySig
   
let defineNodeCTor (nodeClass : TypeBuilder) treeClass (treeCtor : ConstructorBuilder) 
    (valFld : FieldBuilder) (leftFld : FieldBuilder) (rightFld : FieldBuilder) = 
    let inTypes = [|typeof<int>; treeClass.GetType(); treeClass.GetType()|]
    let consConstruct = nodeClass.DefineConstructor(pub, callStandard, inTypes)
    consConstruct.DefineParameter(1, ParameterAttributes.None, "value") |> ignore
    consConstruct.DefineParameter(2, ParameterAttributes.None, "left") |> ignore
    consConstruct.DefineParameter(3, ParameterAttributes.None, "right") |> ignore
    let ilGen = consConstruct.GetILGenerator()
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Call, treeCtor)
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Ldarg_1)
    ilGen.Emit(OpCodes.Stfld, valFld)
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Ldarg_2)
    ilGen.Emit(OpCodes.Stfld, leftFld)
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Ldarg_3)
    ilGen.Emit(OpCodes.Stfld, rightFld)
    ilGen.Emit(OpCodes.Ret)
    consConstruct

let createIsTypeMethod (sourceClass : TypeBuilder) (compareClass : TypeBuilder) name = 
    let checkTypeMethod = sourceClass.DefineMethod(name, pub, callHasThis, typeof<bool>, [||])
    let ilGen = checkTypeMethod.GetILGenerator()
    ilGen.Emit(OpCodes.Ldarg_0) 
    ilGen.Emit(OpCodes.Isinst, compareClass)
    ilGen.Emit(OpCodes.Ldnull)
    ilGen.Emit(OpCodes.Cgt_Un)
    ilGen.Emit(OpCodes.Ret)

let createNewLeafMethod (treeClass : TypeBuilder) (leafClassCTor : ConstructorBuilder) = 
    let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static
    let newLeafMethod = treeClass.DefineMethod("Leaf", methodAttributes, callStandard, treeClass, [||])
    let ilGen = newLeafMethod.GetILGenerator()
    ilGen.Emit(OpCodes.Newobj, leafClassCTor)
    ilGen.Emit(OpCodes.Ret)

let createNewNodeMethod (treeClass : TypeBuilder) (nodeClassCTor : ConstructorBuilder) =
    let inTypes = [|typeof<int>; treeClass.GetType(); treeClass.GetType()|]
    let newNodeMethod = treeClass.DefineMethod("Node", pubStaticHide, callStandard, treeClass, inTypes)
    newNodeMethod.DefineParameter(1, ParameterAttributes.None, "value") |> ignore
    newNodeMethod.DefineParameter(2, ParameterAttributes.None, "left") |> ignore
    newNodeMethod.DefineParameter(3, ParameterAttributes.None, "right") |> ignore
    let ilGen = newNodeMethod.GetILGenerator()
    ilGen.Emit(OpCodes.Ldarg_0)
    ilGen.Emit(OpCodes.Ldarg_1)
    ilGen.Emit(OpCodes.Ldarg_2)
    ilGen.Emit(OpCodes.Newobj, nodeClassCTor)
    ilGen.Emit(OpCodes.Ret)

let createTreeStructure (mb : ModuleBuilder) =
     // Create MFTree class
    let treeClass = defineSuperClass mb "MFTree"
    let treeClassCTor = defineDefaultCTor treeClass
    // Create Node class
    let nodeClass = defineSubClass mb "Node" treeClass
    let valFld = addField nodeClass "value" typeof<int>
    let leftFld = addField nodeClass "left" treeClass
    let rightFld = addField nodeClass "right" treeClass
    let nodeClassCTor = defineNodeCTor nodeClass treeClass treeClassCTor valFld leftFld rightFld
    // Crteate Leaf class    
    let leafClass = defineSubClass mb "Leaf" treeClass
    let nilClassCTor = defineEmptyCTor leafClass
    // Populate MFTree class
    createIsTypeMethod treeClass nodeClass "IsNode"
    createIsTypeMethod treeClass leafClass "IsLeaf"
    createNewLeafMethod treeClass nilClassCTor
    createNewNodeMethod treeClass nodeClassCTor
    createGetSubFieldMethod treeClass nodeClass valFld "GetValue"
    createGetSubFieldMethod treeClass nodeClass leftFld "GetLeft"
    createGetSubFieldMethod treeClass nodeClass rightFld "GetRight"
    // Create types
    let treeType = treeClass.CreateType()
    nodeClass.CreateType() |> ignore
    leafClass.CreateType() |> ignore
    treeType

let buildTreeType mb = 
    createTreeStructure mb