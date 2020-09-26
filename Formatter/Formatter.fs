module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Formatter.ParseTree
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
    parser.program () |> toProgramNode |> printfn "%A"
