﻿module internal QsFmt.Formatter.ParseTree.Namespace

open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.ParseTree.Statement
open QsFmt.Formatter.ParseTree.Type
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Parser

type private NamespaceItemVisitor(tokens) =
    inherit QSharpParserBaseVisitor<NamespaceItem>()

    let typeVisitor = TypeVisitor tokens

    let statementVisitor = StatementVisitor tokens

    override _.DefaultResult = failwith "Unknown namespace element."

    override _.VisitCallableElement context =
        let scope = context.callable.body.scope () // TODO

        CallableDeclaration
            { CallableKeyword = context.callable.keyword |> toTerminal tokens
              Name = context.callable.name |> toTerminal tokens
              Colon = context.callable.colon |> toTerminal tokens
              ReturnType = typeVisitor.Visit context.callable.returnType
              Block =
                  { OpenBrace = scope.openBrace |> toTerminal tokens
                    Items =
                        scope._statements
                        |> Seq.map statementVisitor.Visit
                        |> List.ofSeq
                    CloseBrace = scope.closeBrace |> toTerminal tokens } }

let private toNamespace tokens (context: QSharpParser.NamespaceContext) =
    let visitor = NamespaceItemVisitor tokens

    { NamespaceKeyword = context.keyword |> toTerminal tokens
      Name =
          { Prefix = prefix tokens context.name.Start.TokenIndex
            Text = context.name.GetText() }
      Block =
          { OpenBrace = context.openBrace |> toTerminal tokens
            Items =
                context._elements
                |> Seq.map visitor.Visit
                |> List.ofSeq
            CloseBrace = context.closeBrace |> toTerminal tokens } }

let toProgram tokens (context: QSharpParser.ProgramContext) =
    let namespaces =
        context.``namespace`` ()
        |> Array.toList
        |> List.map (toNamespace tokens)

    { Namespaces = namespaces
      Eof = context.eof |> toTerminal tokens }
