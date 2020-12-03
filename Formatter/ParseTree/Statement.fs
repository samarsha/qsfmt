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
          OpenBrace = context.body.openBrace |> toTerminal tokens
          Statements =
              context.body._statements
              |> Seq.map visitor.Visit
              |> List.ofSeq
          CloseBrace = context.body.closeBrace |> toTerminal tokens }
        |> If

    override visitor.VisitElseStatement context =
        { ElseKeyword = context.``else`` |> toTerminal tokens
          OpenBrace = context.body.openBrace |> toTerminal tokens
          Statements =
              context.body._statements
              |> Seq.map visitor.Visit
              |> List.ofSeq
          CloseBrace = context.body.closeBrace |> toTerminal tokens }
        |> Else
