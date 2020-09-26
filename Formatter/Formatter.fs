module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Parser

let private example =
    "namespace Foo {\
         function Bar() : Int {\
             let x = 7;\
             return x;\
         }\
     }"

let format () =
    let parser =
        example
        |> AntlrInputStream
        |> QSharpLexer
        |> CommonTokenStream
        |> QSharpParser
    let program = parser.program ()
    program.GetText () |> printfn "%s"
