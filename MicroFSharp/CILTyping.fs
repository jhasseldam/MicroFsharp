module CILTyping
open System
open MFIL
open Env

type PredefinedTypes =
    { funcInterface : Type
      list : Type }

let concretizeType (predefTypes : PreDefTypes) ilType =
    let rec getSysType ilType =
        let createIFunc inType outType = predefTypes.funcInterface.MakeGenericType([| inType; outType |]) // IFunc<Type, Type>
        match ilType with
        | ILInt -> typeof<int>
        | ILBool -> typeof<bool>
        | ILList -> predefTypes.listType
        | ILTree -> predefTypes.treeType
        | ILFunc(inILType, outILType) -> createIFunc (getSysType inILType) (getSysType outILType)
    getSysType ilType

let getIntfType preDefTypes inILType outILType = 
    let tempType = concretizeType preDefTypes (ILFunc(inILType, outILType))
    tempType