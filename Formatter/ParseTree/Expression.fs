module internal QsFmt.Formatter.ParseTree.Expression

open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Parser
open System.Collections.Generic

let private padZip (source1: _ seq, padding1) (source2: _ seq, padding2) =
    let enumerator1 = source1.GetEnumerator()
    let enumerator2 = source2.GetEnumerator()

    let next (enumerator: _ IEnumerator) =
        if enumerator.MoveNext() then Some enumerator.Current else None

    let nextPair _ =
        match next enumerator1, next enumerator2 with
        | None, None -> None
        | next1, next2 -> Some(next1 |> Option.defaultValue padding1, next2 |> Option.defaultValue padding2)

    Seq.initInfinite nextPair
    |> Seq.takeWhile Option.isSome
    |> Seq.choose id

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

        let items =
            padZip (expressions |> Seq.map Some, None) (commas |> Seq.map Some, None)
            |> Seq.map (fun (item, comma) -> { Item = item; Comma = comma })
            |> List.ofSeq

        Tuple
            { OpenParen = context.openParen |> toTerminal tokens
              Items = items
              CloseParen = context.closeParen |> toTerminal tokens }

    override visitor.VisitAddExpression context =
        BinaryOperator
            { Left = visitor.Visit context.left
              Operator = context.operator |> toTerminal tokens
              Right = visitor.Visit context.right }

    override visitor.VisitUpdateExpression context =
        Update
            { Record = visitor.Visit context.record
              With = context.``with`` |> toTerminal tokens
              Item = visitor.Visit context.item
              Arrow = context.arrow |> toTerminal tokens
              Value = visitor.Visit context.value }
