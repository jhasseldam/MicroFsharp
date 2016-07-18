module CILClassBuilder
open System.Reflection.Emit
open System.Reflection
open Env
open CILTyping
open CILMethodBuilder
open MFIL

let populateFuncClass meta name paramName inILType outILType instrs = 
    let { classBuilder = funcClass; fields = fields } = Map.find name meta.classInfoMap
    let invokeMethod = defineInvokeMethod funcClass meta inILType outILType paramName
    populateInvokeMethod meta name invokeMethod paramName inILType fields instrs
    funcClass.CreateType()

let populateEntryClass assemblyBuilder meta name instrs = 
    let { classBuilder = entryClass; fields = fields } = Map.find name meta.classInfoMap
    let mainMethod = defineMainMethod assemblyBuilder entryClass meta
    populateMainMethod meta name mainMethod instrs
    entryClass.CreateType()

let createField (tb : TypeBuilder) meta field =
    let (Field(name, ilType)) = field
    let sysType = concretizeType meta.preDefTypes ilType
    let fieldBuilder = tb.DefineField(name, sysType, FieldAttributes.Public)
    (fieldBuilder, ilType)

let createClass (mb : ModuleBuilder) name = 
    mb.DefineType(name, TypeAttributes.Public)

let createAbstractClass (mb : ModuleBuilder) name =
    mb.DefineType(name, TypeAttributes.Public ||| TypeAttributes.Abstract)

let addClassToMeta meta newClass ctor name fields funcType =
    let currentClassInfoMap = meta.classInfoMap
    let classInfo = { classBuilder = newClass; fields = fields; classType = funcType; ctorBuilder = ctor}
    { meta with classInfoMap = Map.add name classInfo currentClassInfoMap }

let defineFuncClass (mb : ModuleBuilder) meta name inType outType fields =
    let newClass = createClass mb name
    let newClassCtor = defineDefaultCTor newClass
    let funcType = getIntfType meta.preDefTypes inType outType
    newClass.AddInterfaceImplementation(funcType)
    let fieldBuilders = List.map (createField newClass meta) fields
    addClassToMeta meta newClass newClassCtor name fieldBuilders funcType

let defineEntryClass (mb : ModuleBuilder) meta name fields =
    let newClass = createClass mb name
    let newClassCtor = defineDefaultCTor newClass
    addClassToMeta meta newClass newClassCtor name fields typeof<int>

// The Following are used in creating the built in types: MFList and MFTree
let defineSuperClass (mb : ModuleBuilder) name =
    createAbstractClass mb name
    
let defineSubClass (mb : ModuleBuilder) name listClass = 
    let subClass = createClass mb name
    subClass.SetParent(listClass)
    subClass

let addField (subClass : TypeBuilder) name fieldType =
     subClass.DefineField(name, fieldType, FieldAttributes.Public)