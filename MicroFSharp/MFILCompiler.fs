module MFILCompiler

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open Env
open MFIL
open CILClassBuilder
open CILInterfaceBuilder
open CILListBuilder
open CILTreeBuilder

let createAssembly name = 
    let assemblyName = new AssemblyName(Path.GetFileNameWithoutExtension(name))
    AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save)

let rec defineClasses moduleBuilder meta classes = 
    match classes with
    | FunClass(name, _, (inType, outType), fields, _)::rest -> 
        defineClasses moduleBuilder (defineFuncClass moduleBuilder meta name inType outType fields) rest
    | EntryClass(name, _)::rest -> 
        defineClasses moduleBuilder (defineEntryClass moduleBuilder meta name []) rest
    | [] -> meta

let rec populateClasses (assemblyBuilder : AssemblyBuilder) meta classes =
    match classes with
    | FunClass(name, paramName, (inILType, outILType), _, instrs)::rest  -> 
        populateFuncClass meta name paramName inILType outILType instrs |> ignore
        populateClasses assemblyBuilder meta rest
    | EntryClass(name, instrs)::rest -> 
        populateEntryClass assemblyBuilder meta name instrs |> ignore
        populateClasses assemblyBuilder meta rest
    | [] -> []

let generateIL meta moduleBuilder (assemblyBuilder : AssemblyBuilder) constructs = 
    let updatedMeta = defineClasses moduleBuilder meta constructs
    populateClasses assemblyBuilder updatedMeta constructs

let produceIL assemblyName constructs = 
    let assemblyBuilder = createAssembly assemblyName
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName)
    
    let preDefTypes =
        { funcInterface = createInterface moduleBuilder
          listType = buildListType moduleBuilder
          treeType = buildTreeType moduleBuilder } 
    
    let meta = 
        { currentEnv = Env.empty
          classInfoMap = Map.empty
          preDefTypes = preDefTypes }

    generateIL meta moduleBuilder assemblyBuilder constructs |> ignore
    moduleBuilder.CreateGlobalFunctions() |> ignore
    assemblyBuilder.Save(assemblyName) |> ignore