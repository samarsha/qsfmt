module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Formatter.ParseTree
open QsFmt.Parser

let private example =
    "namespace Foo {
         function Bar() : Int {
             let x = 7;
             return x;
         }
     }"

let format () =
    let tokens =
        example
        |> AntlrInputStream
        |> QSharpLexer
        |> CommonTokenStream
    let parser = QSharpParser tokens
    parser.program () |> toProgramNode tokens |> printfn "%A"
