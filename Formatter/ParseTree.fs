﻿module internal QsFmt.Formatter.ParseTree

open Antlr4.Runtime
open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type
open QsFmt.Parser
open System.Collections.Generic
open System.Collections.Immutable

let private hiddenTokensToRight (tokens : IToken ImmutableArray) index =
    seq {
        for i in index + 1 .. tokens.Length - 1 do
            tokens.[i] }
    |> Seq.takeWhile (fun token -> token.Channel = QSharpLexer.Hidden)

let private trailingTrivia tokens index =
    hiddenTokensToRight tokens index
    |> Seq.map (fun token -> token.Text)
    |> String.concat ""

let private toNode tokens (context : ParserRuleContext) kind =
    { Kind = Some kind
      TrailingTrivia = trailingTrivia tokens context.Stop.TokenIndex }

let private toTerminal tokens (terminal : IToken) =
    { Kind = Terminal terminal.Text |> Some
      TrailingTrivia = trailingTrivia tokens terminal.TokenIndex }

let private padZip (source1 : _ seq, padding1) (source2 : _ seq, padding2) =
    let enumerator1 = source1.GetEnumerator ()
    let enumerator2 = source2.GetEnumerator ()
    let next (enumerator : _ IEnumerator) = if enumerator.MoveNext () then Some enumerator.Current else None
    let nextPair _ =
        match next enumerator1, next enumerator2 with
        | None, None -> None
        | next1, next2 -> Some (next1 |> Option.defaultValue padding1, next2 |> Option.defaultValue padding2)
    Seq.initInfinite nextPair |> Seq.takeWhile Option.isSome |> Seq.choose id

type private TypeVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Type Node> ()

    override _.DefaultResult = failwith "Unknown type."

    override _.VisitIntType context = Int |> toNode tokens context

    override _.VisitUserDefinedType context = context.name.GetText () |> UserDefinedType |> toNode tokens context

type private ExpressionVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Expression Node> ()

    override _.DefaultResult = failwith "Unknown expression."

    override _.VisitMissingExpression context = MissingExpression |> toNode tokens context

    override _.VisitIdentifierExpression context = context.name.GetText () |> Literal |> toNode tokens context

    override _.VisitIntegerExpression context = context.value.Text |> Literal |> toNode tokens context

    override visitor.VisitTupleExpression context =
        let expressions = context._items |> Seq.map visitor.Visit
        let commas = context._commas |> Seq.map (toTerminal tokens)
        let items =
            padZip (expressions, missingNode) (commas, missingNode)
            |> Seq.map (fun (item, comma) -> { Item = item; Comma = comma })
            |> List.ofSeq
        Tuple {
            OpenParen = context.openParen |> toTerminal tokens
            Items = items
            CloseParen = context.closeParen |> toTerminal tokens }
        |> toNode tokens context
        |> withoutTrivia

    override visitor.VisitAddExpression context =
        BinaryOperator {
            Left = visitor.Visit context.left
            Operator = context.operator |> toTerminal tokens
            Right = visitor.Visit context.right }
        |> toNode tokens context
        |> withoutTrivia

    override visitor.VisitUpdateExpression context =
        Update {
            Record = visitor.Visit context.record
            With = context.``with`` |> toTerminal tokens
            Item = visitor.Visit context.item
            Arrow = context.arrow |> toTerminal tokens
            Value = visitor.Visit context.value }
        |> toNode tokens context
        |> withoutTrivia

type private SymbolBindingVisitor (tokens) =
    inherit QSharpParserBaseVisitor<SymbolBinding Node> ()

    override _.DefaultResult = failwith "Unknown symbol binding."

    override _.VisitSymbolName context = context.name |> toTerminal tokens |> SymbolName |> toNode tokens context

    override visitor.VisitSymbolTuple context =
        context._bindings |> Seq.map visitor.Visit |> List.ofSeq |> SymbolTuple |> toNode tokens context

type private StatementVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Statement Node> ()

    let expressionVisitor = ExpressionVisitor tokens

    let symbolBindingVisitor = SymbolBindingVisitor tokens

    override _.DefaultResult = failwith "Unknown statement."

    override _.VisitReturnStatement context =
        Return {
            ReturnKeyword = context.``return`` |> toTerminal tokens
            Expression = expressionVisitor.Visit context.value
            Semicolon = context.semicolon |> toTerminal tokens }
        |> toNode tokens context
        |> withoutTrivia

    override _.VisitLetStatement context =
        Let {
            LetKeyword = context.``let`` |> toTerminal tokens
            Binding = symbolBindingVisitor.Visit context.binding
            Equals = context.equals |> toTerminal tokens
            Value = expressionVisitor.Visit context.value
            Semicolon = context.semicolon |> toTerminal tokens }
        |> toNode tokens context
        |> withoutTrivia

type private NamespaceElementVisitor (tokens) =
    inherit QSharpParserBaseVisitor<NamespaceElement Node> ()

    let typeVisitor = TypeVisitor tokens

    let statementVisitor = StatementVisitor tokens

    override _.DefaultResult = failwith "Unknown namespace element."

    override _.VisitCallableElement context =
        let scope = context.callable.body.scope () // TODO
        CallableDeclaration {
            CallableKeyword = context.callable.keyword |> toTerminal tokens
            Name = context.callable.name |> toTerminal tokens
            Colon = context.callable.colon |> toTerminal tokens
            ReturnType = typeVisitor.Visit context.callable.returnType
            OpenBrace = scope.openBrace |> toTerminal tokens
            Statements = scope._statements |> Seq.map statementVisitor.Visit |> List.ofSeq
            CloseBrace = scope.closeBrace |> toTerminal tokens }
        |> toNode tokens context
        |> withoutTrivia

let private toNamespaceToken tokens (context : QSharpParser.NamespaceContext) =
    let visitor = NamespaceElementVisitor tokens
    { NamespaceKeyword = context.keyword |> toTerminal tokens
      Name = context.name.GetText () |> Terminal |> toNode tokens context.name
      OpenBrace = context.openBrace |> toTerminal tokens
      Elements = context._elements |> Seq.map visitor.Visit |> List.ofSeq
      CloseBrace = context.closeBrace |> toTerminal tokens }
    |> toNode tokens context
    |> withoutTrivia

let toProgramToken tokens (context : QSharpParser.ProgramContext) =
    context.``namespace`` ()
    |> Array.toList
    |> List.map (toNamespaceToken tokens)
    |> Program
    |> toNode tokens context
    |> withoutTrivia
