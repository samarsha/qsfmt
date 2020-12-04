﻿module internal QsFmt.Formatter.ParseTree.Statement

open QsFmt.Formatter.ParseTree.Expression
open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Parser

type private SymbolBindingVisitor(tokens) =
    inherit QSharpParserBaseVisitor<SymbolBinding>()

    override _.DefaultResult = failwith "Unknown symbol binding."

    override _.VisitSymbolName context =
        { Name = context.name |> toTerminal tokens
          Type = None }
        |> SymbolDeclaration

    override visitor.VisitSymbolTuple context =
        let bindings =
            context._bindings |> Seq.map visitor.Visit

        let commas =
            context._commas |> Seq.map (toTerminal tokens)

        { OpenParen = context.openParen |> toTerminal tokens
          Items = tupleItems bindings commas
          CloseParen = context.closeParen |> toTerminal tokens }
        |> SymbolTuple

type StatementVisitor(tokens) =
    inherit QSharpParserBaseVisitor<Statement>()

    let expressionVisitor = ExpressionVisitor tokens

    let symbolBindingVisitor = SymbolBindingVisitor tokens

    override _.DefaultResult = failwith "Unknown statement."

    override _.VisitReturnStatement context =
        { ReturnKeyword = context.``return`` |> toTerminal tokens
          Expression = expressionVisitor.Visit context.value
          Semicolon = context.semicolon |> toTerminal tokens }
        |> Return

    override _.VisitLetStatement context =
        { LetKeyword = context.``let`` |> toTerminal tokens
          Binding = symbolBindingVisitor.Visit context.binding
          Equals = context.equals |> toTerminal tokens
          Value = expressionVisitor.Visit context.value
          Semicolon = context.semicolon |> toTerminal tokens }
        |> Let

    override visitor.VisitIfStatement context =
        { IfKeyword = context.``if`` |> toTerminal tokens
          OpenParen = context.openParen |> toTerminal tokens
          Condition = expressionVisitor.Visit context.condition
          CloseParen = context.closeParen |> toTerminal tokens
          Block =
              { OpenBrace = context.body.openBrace |> toTerminal tokens
                Items =
                    context.body._statements
                    |> Seq.map visitor.Visit
                    |> List.ofSeq
                CloseBrace = context.body.closeBrace |> toTerminal tokens } }
        |> If

    override visitor.VisitElseStatement context =
        { ElseKeyword = context.``else`` |> toTerminal tokens
          Block =
              { OpenBrace = context.body.openBrace |> toTerminal tokens
                Items =
                    context.body._statements
                    |> Seq.map visitor.Visit
                    |> List.ofSeq
                CloseBrace = context.body.closeBrace |> toTerminal tokens } }
        |> Else
