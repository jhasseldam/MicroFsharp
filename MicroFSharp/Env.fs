module Env
open MFIL
open System.Reflection
open System.Reflection.Emit
open System

type Element = 
    | Arg of int * ILType
    | Local of int * ILType
    | Field of FieldInfo * ILType

type Env = 
    | Env of int * Map<string, Element>

let empty = Env(0, Map.empty)

let ofArg argName ilType =
    let instanceEnvOffset = 1
    let updatedMap = Map.add argName (Arg(instanceEnvOffset, ilType)) Map.empty
    Env(0, updatedMap)

let addLocal name ilType env = 
    match env with
    | Env(next, map) -> 
        let map' = Map.add name (Local (next, ilType)) map
        Env(next + 1, map'), next

let addField name fieldinfo ilType env = 
    match env with
    | Env(next, map) -> 
        let map' = Map.add name (Field (fieldinfo, ilType)) map
        Env(next, map'), next

let tryGetElement name env = 
    match env with
    | Env(_, map) -> Map.tryFind name map

type PreDefTypes = 
    { funcInterface : Type 
      listType : Type
      treeType : Type }

type ClassInfo = 
    { classBuilder : TypeBuilder
      fields : (FieldBuilder * ILType) list
      classType : Type
      ctorBuilder : ConstructorBuilder }

type Meta = 
    { currentEnv : Env
      classInfoMap : Map<string, ClassInfo> 
      preDefTypes : PreDefTypes } 
