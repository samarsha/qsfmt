module internal QsFmt.Formatter.ParseTree.Statement

open QsFmt.Formatter.ParseTree.Expression
open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Parser

type private SymbolBindingVisitor(tokens) =
    inherit QSharpParserBaseVisitor<SymbolBinding>()

    override _.DefaultResult = failwith "Unknown symbol binding."

    override _.VisitSymbolName context =
        context.name |> toTerminal tokens |> SymbolName

    override visitor.VisitSymbolTuple context =
        context._bindings
        |> Seq.map visitor.Visit
        |> List.ofSeq
        |> SymbolTuple

type StatementVisitor(tokens) =
    inherit QSharpParserBaseVisitor<Statement>()

    let expressionVisitor = ExpressionVisitor tokens

    let symbolBindingVisitor = SymbolBindingVisitor tokens

    override _.DefaultResult = failwith "Unknown statement."

    override _.VisitReturnStatement context =
        Return
            { ReturnKeyword = context.``return`` |> toTerminal tokens
              Expression = expressionVisitor.Visit context.value
              Semicolon = context.semicolon |> toTerminal tokens }

    override _.VisitLetStatement context =
        Let
            { LetKeyword = context.``let`` |> toTerminal tokens
              Binding = symbolBindingVisitor.Visit context.binding
              Equals = context.equals |> toTerminal tokens
              Value = expressionVisitor.Visit context.value
              Semicolon = context.semicolon |> toTerminal tokens }
