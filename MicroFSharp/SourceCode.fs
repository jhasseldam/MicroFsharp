module SourceCode

open Microsoft.FSharp.Text.Lexing
open System.IO
open System
open TypeChecker

let parse sourceCode =
    let lexbuf = LexBuffer<char>.FromString(sourceCode)
    try 
      Parser.Main Lexer.Token lexbuf
    with 
      | exn -> let pos = lexbuf.EndPos 
               failwithf "%s near line %d, column %d\n" 
                  (exn.Message) (pos.Line+1) pos.Column

let loadAst path = File.ReadAllLines(path) |> String.concat " " |> parse

let compileMFIL showDetails mfilSourceCode = 
    if showDetails then printfn "\n#CIL instructions:\n %A\n" mfilSourceCode
    MFILCompiler.produceIL "MyProgram.exe" mfilSourceCode

let compileMF showDetails mfSourceCode =
    if showDetails then printfn "\n#AST:\n %A\n" mfSourceCode
    MFCompiler.compile mfSourceCode

let compileProgram path showDetails = 
    let untypedMfSource = loadAst path
    let typedMfSourceCode = checkProgram untypedMfSource
    compileMF showDetails typedMfSourceCode |> compileMFIL showDetails