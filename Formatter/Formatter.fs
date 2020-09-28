module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Formatter.Error
open QsFmt.Formatter.ParseTree
open QsFmt.Formatter.Printer
open QsFmt.Formatter.Rules
open QsFmt.Parser
open System.Collections.Immutable

let format (source : string) =
    let tokenStream = source |> AntlrInputStream |> QSharpLexer |> CommonTokenStream
    let parser = QSharpParser tokenStream
    let errorListener = ErrorListListener ()
    parser.AddErrorListener errorListener
    let program = parser.program ()
    let tokens =
        tokenStream.GetTokens ()
        |> hideTokens errorListener.ErrorTokens
        |> ImmutableArray.CreateRange
    program
    |> toProgramToken tokens
    |> collapseSpaces
    |> singleSpaceAfterLetBinding
    |> printProgram
