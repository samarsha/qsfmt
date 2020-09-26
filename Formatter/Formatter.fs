module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Formatter.ParseTree
open QsFmt.Parser

let format (source : string) =
    let tokens = source |> AntlrInputStream |> QSharpLexer |> CommonTokenStream
    let parser = QSharpParser tokens
    parser.program () |> toProgramNode tokens |> printfn "%A"
