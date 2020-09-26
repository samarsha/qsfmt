module internal QsFmt.Formatter.ParseTree

open QsFmt.Formatter.SyntaxTree
open QsFmt.Parser

type private ExpressionVisitor () =
    inherit QSharpBaseVisitor<Expression Node> ()

    override _.DefaultResult = { Node = InvalidExpression; TrailingTrivia = None }

let private expressionVisitor = ExpressionVisitor ()

type private SymbolTupleVisitor () =
    inherit QSharpBaseVisitor<SymbolTuple Node> ()

    override _.DefaultResult = { Node = InvalidSymbolTuple; TrailingTrivia = None }

    override _.VisitSymbol context =
        { Node = context.Identifier().GetText() |> Symbol
          TrailingTrivia = None }

    override this.VisitSymbols context =
        { Node = context.symbolTuple () |> Array.toList |> List.map this.Visit |> Symbols
          TrailingTrivia = None }

let private symbolTupleVisitor = SymbolTupleVisitor ()

type private StatementVisitor () =
    inherit QSharpBaseVisitor<Statement Node> ()

    override _.DefaultResult = { Node = InvalidStatement; TrailingTrivia = None }

    override _.VisitReturn context =
        { Node = context.expression () |> expressionVisitor.Visit |> Return
          TrailingTrivia = None }

    override _.VisitLet context =
        let symbols = context.symbolTuple () |> symbolTupleVisitor.Visit
        let expression = context.expression () |> expressionVisitor.Visit
        { Node = Let (symbols, expression); TrailingTrivia = None }

let private statementVisitor = StatementVisitor ()

type private NamespaceElementVisitor () =
    inherit QSharpBaseVisitor<NamespaceElement Node> ()

    override _.DefaultResult = { Node = InvalidNamespaceElement; TrailingTrivia = None }

    override _.VisitCallableDeclaration context =
        // TODO
        let statements =
            context.callableDeclarationSuffix().callableBody().scope().statement()
            |> Array.toList
            |> List.map statementVisitor.Visit
        { Node = CallableDeclaration statements; TrailingTrivia = None }

let private namespaceElementVisitor = NamespaceElementVisitor ()

let private toNamespaceNode (context : QSharpParser.NamespaceContext) =
    let ns =
        context.namespaceElement ()
        |> Array.toList
        |> List.map namespaceElementVisitor.Visit
        |> Namespace
    { Node = ns; TrailingTrivia = None }

let toProgramNode (context : QSharpParser.ProgramContext) =
    let program =
        context.``namespace`` ()
        |> Array.toList
        |> List.map toNamespaceNode
        |> Program
    { Node = program; TrailingTrivia = None }
