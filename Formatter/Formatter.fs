﻿module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Formatter.Errors
open QsFmt.Formatter.ParseTree.Namespace
open QsFmt.Formatter.Printer
open QsFmt.Formatter.Rules
open QsFmt.Parser
open System.Collections.Immutable

let private curry f x y = f (x, y)

[<CompiledName "Format">]
let format (source: string) =
    let tokenStream =
        source
        |> AntlrInputStream
        |> QSharpLexer
        |> CommonTokenStream

    let parser = QSharpParser tokenStream
    let errorListener = ErrorListListener()
    parser.AddErrorListener errorListener
    let program = parser.program ()

    let tokens =
        tokenStream.GetTokens()
        |> hideTokens errorListener.ErrorTokens
        |> ImmutableArray.CreateRange

    program
    |> toProgram tokens
    |> curry collapsedSpaces.Program ()
    |> curry operatorSpacing.Program ()
    |> curry newLines.Program ()
    |> curry indentation.Program 0
    |> printer.Program
