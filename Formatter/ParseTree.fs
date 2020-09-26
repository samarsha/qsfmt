module internal QsFmt.Formatter.ParseTree

open Antlr4.Runtime
open QsFmt.Formatter.SyntaxTree
open QsFmt.Parser

let private trailingTrivia (tokens : BufferedTokenStream) (context : ParserRuleContext) =
    tokens.GetHiddenTokensToRight context.Stop.TokenIndex :> _ seq
    |> Option.ofObj
    |> Option.defaultValue Seq.empty
    |> Seq.map (fun token -> token.Text)
    |> String.concat ""

let private toNode tokens context node =
    { Node = node
      TrailingTrivia = trailingTrivia tokens context }

type private ExpressionVisitor () =
    inherit QSharpBaseVisitor<Expression Node> ()

    override _.DefaultResult = { Node = InvalidExpression; TrailingTrivia = "" }

let private expressionVisitor = ExpressionVisitor ()

type private SymbolTupleVisitor (tokens) =
    inherit QSharpBaseVisitor<SymbolTuple Node> ()

    override _.DefaultResult = { Node = InvalidSymbolTuple; TrailingTrivia = "" }

    override _.VisitSymbol context =
        context.Identifier().GetText() |> Symbol |> toNode tokens context

    override this.VisitSymbols context =
        context.symbolTuple ()
        |> Array.toList
        |> List.map this.Visit
        |> Symbols
        |> toNode tokens context

type private StatementVisitor (tokens) =
    inherit QSharpBaseVisitor<Statement Node> ()

    let symbolTupleVisitor = SymbolTupleVisitor tokens

    override _.DefaultResult = { Node = InvalidStatement; TrailingTrivia = "" }

    override _.VisitReturn context =
        context.expression ()
        |> expressionVisitor.Visit
        |> Return
        |> toNode tokens context

    override _.VisitLet context =
        let symbols = context.symbolTuple () |> symbolTupleVisitor.Visit
        let expression = context.expression () |> expressionVisitor.Visit
        Let (symbols, expression) |> toNode tokens context

type private NamespaceElementVisitor (tokens) =
    inherit QSharpBaseVisitor<NamespaceElement Node> ()

    let statementVisitor = StatementVisitor tokens

    override _.DefaultResult = { Node = InvalidNamespaceElement; TrailingTrivia = "" }

    override _.VisitCallableDeclaration context =
        context.callableDeclarationSuffix().callableBody().scope().statement() // TODO
        |> Array.toList
        |> List.map statementVisitor.Visit
        |> CallableDeclaration
        |> toNode tokens context

let private toNamespaceNode tokens (context : QSharpParser.NamespaceContext) =
    let visitor = NamespaceElementVisitor tokens
    context.namespaceElement ()
    |> Array.toList
    |> List.map visitor.Visit
    |> Namespace
    |> toNode tokens context

let toProgramNode tokens (context : QSharpParser.ProgramContext) =
    context.``namespace`` ()
    |> Array.toList
    |> List.map (toNamespaceNode tokens)
    |> Program
    |> toNode tokens context
