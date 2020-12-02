module internal QsFmt.Formatter.ParseTree.Statement

open QsFmt.Formatter.ParseTree.Expression
open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Parser

type private SymbolBindingVisitor(tokens) =
    inherit QSharpParserBaseVisitor<SymbolBinding Node>()

    override _.DefaultResult = failwith "Unknown symbol binding."

    override _.VisitSymbolName context =
        context.name
        |> toTerminal tokens
        |> SymbolName
        |> toNode tokens context
        |> Node.withoutPrefix

    override visitor.VisitSymbolTuple context =
        context._bindings
        |> Seq.map visitor.Visit
        |> List.ofSeq
        |> SymbolTuple
        |> toNode tokens context
        |> Node.withoutPrefix

type StatementVisitor(tokens) =
    inherit QSharpParserBaseVisitor<Statement Node>()

    let expressionVisitor = ExpressionVisitor tokens

    let symbolBindingVisitor = SymbolBindingVisitor tokens

    override _.DefaultResult = failwith "Unknown statement."

    override _.VisitReturnStatement context =
        Return
            { ReturnKeyword = context.``return`` |> toTerminal tokens
              Expression = expressionVisitor.Visit context.value
              Semicolon = context.semicolon |> toTerminal tokens }
        |> toNode tokens context
        |> Node.withoutPrefix

    override _.VisitLetStatement context =
        Let
            { LetKeyword = context.``let`` |> toTerminal tokens
              Binding = symbolBindingVisitor.Visit context.binding
              Equals = context.equals |> toTerminal tokens
              Value = expressionVisitor.Visit context.value
              Semicolon = context.semicolon |> toTerminal tokens }
        |> toNode tokens context
        |> Node.withoutPrefix
