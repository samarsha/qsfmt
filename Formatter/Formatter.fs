﻿module QsFmt.Formatter.Formatter

open Antlr4.Runtime
open QsFmt.Formatter.Errors
open QsFmt.Formatter.ParseTree.Namespace
open QsFmt.Formatter.Printer
open QsFmt.Formatter.Rules
open QsFmt.Parser
open System.Collections.Immutable

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
    |> fun program' -> collapsedSpaces.Program((), program')
    |> fun program' -> operatorSpacing.Program((), program')
    |> fun program' -> indentation.Program(0, program')
    |> printer.Program
