module Program
open System
open SourceCode

[<EntryPoint>]
let main argv = 
    let evalCmdArgs (args : string[]) =
         let argList = List.ofArray args
         let sourceCode = try List.nth argList 0 with 
            | :? Exception -> failwith "Please supply an argument"
         compileProgram sourceCode (List.exists (fun s -> s = "-v") argList)        
    evalCmdArgs argv
    0 // return an integer exit code