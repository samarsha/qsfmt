module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Formatter.Errors
open QsFmt.Formatter.Lexer
open QsFmt.Formatter.ParseTree
open QsFmt.Formatter.Printer
open QsFmt.Formatter.Rules
open QsFmt.Parser
open System.Collections.Immutable

let format (source : string) =
    let tokenStream = source |> AntlrInputStream |> Lexer |> CommonTokenStream
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
