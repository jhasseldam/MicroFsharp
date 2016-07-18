module Util
open Ast
open System


type Parameter = Parameter of string * TypeDef


let rec getParamTypes funName parameters funcType = 
    match parameters, funcType with
    | ([], _)                            -> []
    | (arg::rest, FuncType(left, right)) -> Parameter(arg, left) :: getParamTypes funName rest right
    | _                                  -> printfn "Illegal type signature in function: %A" funName
                                            failwith "Illegal function type"


//Generating "random" names for hidden values like: doLet or temp lets in multiple arg funCalls 
let createRandomName prefixTxt = 
    let guid = Guid.NewGuid().ToString().Replace("-", "")
    let firstFiveChars = guid.Substring(0, 5)
    prefixTxt + "_" + firstFiveChars