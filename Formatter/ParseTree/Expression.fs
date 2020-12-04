module internal QsFmt.Formatter.ParseTree.Expression

open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Parser

type ExpressionVisitor(tokens) =
    inherit QSharpParserBaseVisitor<Expression>()

    override _.DefaultResult = failwith "Unknown expression."

    override _.VisitMissingExpression context =
        context.Underscore().Symbol
        |> toTerminal tokens
        |> MissingExpression

    override _.VisitIdentifierExpression context =
        { Prefix = prefix tokens context.name.Start.TokenIndex
          Text = context.name.GetText() }
        |> Literal

    override _.VisitIntegerExpression context =
        context.value |> toTerminal tokens |> Literal

    override visitor.VisitTupleExpression context =
        let expressions = context._items |> Seq.map visitor.Visit

        let commas =
            context._commas |> Seq.map (toTerminal tokens)

        { OpenParen = context.openParen |> toTerminal tokens
          Items = tupleItems expressions commas
          CloseParen = context.closeParen |> toTerminal tokens }
        |> Tuple

    override visitor.VisitAddExpression context =
        { Left = visitor.Visit context.left
          Operator = context.operator |> toTerminal tokens
          Right = visitor.Visit context.right }
        |> BinaryOperator

    override visitor.VisitEqualsExpression context =
        { Left = visitor.Visit context.left
          Operator = context.operator |> toTerminal tokens
          Right = visitor.Visit context.right }
        |> BinaryOperator

    override visitor.VisitUpdateExpression context =
        { Record = visitor.Visit context.record
          With = context.``with`` |> toTerminal tokens
          Item = visitor.Visit context.item
          Arrow = context.arrow |> toTerminal tokens
          Value = visitor.Visit context.value }
        |> Update
