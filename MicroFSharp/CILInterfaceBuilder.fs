module CILInterfaceBuilder

open System.Reflection.Emit
open System.Reflection

let createInterface (mb : ModuleBuilder) =
    let interfaceAtrs = 
        TypeAttributes.Interface ||| TypeAttributes.Public ||| 
        TypeAttributes.Abstract
    let newInterface = mb.DefineType("IFunc", interfaceAtrs)

    let methodAtrs =
        MethodAttributes.Abstract ||| MethodAttributes.Public ||| 
        MethodAttributes.Virtual ||| MethodAttributes.HideBySig ||| MethodAttributes.NewSlot
    
    let [| tinput; toutput |] = newInterface.DefineGenericParameters([|"TInput"; "TOutput"|])
    let inType = tinput.AsType()
    let outType = toutput.AsType() 
    
    newInterface.DefineMethod("Invoke", methodAtrs, outType, [| inType |]) |> ignore
    newInterface.CreateType()  