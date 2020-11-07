module internal QsFmt.Formatter.ParseTree

open Antlr4.Runtime
open QsFmt.Formatter.SyntaxTree
open QsFmt.Parser
open System.Collections.Immutable

let private hiddenTokensToRight (tokens : IToken ImmutableArray) index =
    seq {
        for i in index + 1 .. tokens.Length - 1 do
            tokens.[i]
    }
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

let private flip f x y = f y x

type private TypeVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Type Node> ()

    override _.DefaultResult = failwith "Unknown type."

    override _.VisitIntType context = Int |> toNode tokens context

    override _.VisitUserDefinedType context = context.name.GetText () |> TypeName |> toNode tokens context

type private ExpressionVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Expression Node> ()

    override _.DefaultResult = failwith "Unknown expression."

    override _.VisitMissingExpression context = MissingExpression |> toNode tokens context

    override _.VisitIdentifierExpression context = context.name.GetText () |> Literal |> toNode tokens context

    override _.VisitIntegerExpression context = context.value.Text |> Literal |> toNode tokens context

    override this.VisitTupleExpression context =
        { OpenParen = context.openParen |> toTerminal tokens
          Items = context._items |> Seq.map this.Visit |> List.ofSeq
          CloseParen = context.closeParen |> toTerminal tokens }
        |> Tuple
        |> toNode tokens context
        |> withoutTrailingTrivia

    override this.VisitAddExpression context =
        { Left = this.Visit context.left
          Operator = context.operator |> toTerminal tokens
          Right = this.Visit context.right }
        |> BinaryOperator
        |> toNode tokens context
        |> withoutTrailingTrivia

    override this.VisitUpdateExpression context =
        { Record = this.Visit context.record
          With = context.``with`` |> toTerminal tokens
          Item = this.Visit context.item
          Arrow = context.arrow |> toTerminal tokens
          Value = this.Visit context.value }
        |> Update
        |> toNode tokens context
        |> withoutTrailingTrivia

type private SymbolBindingVisitor (tokens) =
    inherit QSharpParserBaseVisitor<SymbolBinding Node> ()

    override _.DefaultResult = failwith "Unknown symbol binding."

    override _.VisitSymbolName context = context.name |> toTerminal tokens |> SymbolName |> toNode tokens context

    override this.VisitSymbolTuple context =
        context._bindings |> Seq.map this.Visit |> List.ofSeq |> SymbolTuple |> toNode tokens context

type private StatementVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Statement Node> ()

    let expressionVisitor = ExpressionVisitor tokens

    let symbolBindingVisitor = SymbolBindingVisitor tokens

    override _.DefaultResult = failwith "Unknown statement."

    override _.VisitReturnStatement context =
        { ReturnKeyword = context.``return`` |> toTerminal tokens
          Expression = expressionVisitor.Visit context.value
          Semicolon = context.semicolon |> toTerminal tokens }
        |> Return
        |> toNode tokens context
        |> withoutTrailingTrivia

    override _.VisitLetStatement context =
        { LetKeyword = context.``let`` |> toTerminal tokens
          Binding = symbolBindingVisitor.Visit context.binding
          Equals = context.equals |> toTerminal tokens
          Value = expressionVisitor.Visit context.value
          Semicolon = context.semicolon |> toTerminal tokens }
        |> Let
        |> toNode tokens context
        |> withoutTrailingTrivia

type private NamespaceElementVisitor (tokens) =
    inherit QSharpParserBaseVisitor<NamespaceElement Node> ()

    let typeVisitor = TypeVisitor tokens

    let statementVisitor = StatementVisitor tokens

    override _.DefaultResult = failwith "Unknown namespace element."

    override _.VisitCallableElement context =
        let scope = context.callable.body.scope () // TODO
        { CallableKeyword = context.callable.keyword |> toTerminal tokens
          Name = context.callable.name |> toTerminal tokens
          Colon = context.callable.colon |> toTerminal tokens
          ReturnType = typeVisitor.Visit context.callable.returnType
          OpenBrace = scope.openBrace |> toTerminal tokens
          Statements = scope._statements |> Seq.map statementVisitor.Visit |> List.ofSeq
          CloseBrace = scope.closeBrace |> toTerminal tokens }
        |> CallableDeclaration
        |> toNode tokens context
        |> withoutTrailingTrivia

let private toNamespaceToken tokens (context : QSharpParser.NamespaceContext) =
    let visitor = NamespaceElementVisitor tokens
    { NamespaceKeyword = context.keyword |> toTerminal tokens
      Name = context.name.GetText () |> Terminal |> toNode tokens context.name
      OpenBrace = context.openBrace |> toTerminal tokens
      Elements = context._elements |> Seq.map visitor.Visit |> List.ofSeq
      CloseBrace = context.closeBrace |> toTerminal tokens }
    |> toNode tokens context
    |> withoutTrailingTrivia

let toProgramToken tokens (context : QSharpParser.ProgramContext) =
    context.``namespace`` ()
    |> Array.toList
    |> List.map (toNamespaceToken tokens)
    |> Program
    |> toNode tokens context
    |> withoutTrailingTrivia
