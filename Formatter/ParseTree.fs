module internal QsFmt.Formatter.ParseTree

open Antlr4.Runtime
open Antlr4.Runtime.Tree
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

let private toTerminal tokens (terminal : ITerminalNode) =
    { Kind = terminal.GetText () |> Terminal |> Some
      TrailingTrivia = trailingTrivia tokens terminal.Symbol.TokenIndex }

let private findTerminal tokens (context : ParserRuleContext) predicate =
    context.children
    |> Seq.choose (function
        | :? ITerminalNode as terminal -> Some terminal
        | _ -> None)
    |> Seq.tryFind (fun terminal -> terminal.GetText () |> predicate)
    |> Option.map (toTerminal tokens)
    |> Option.defaultValue missingNode

let private flip f x y = f y x

type private TypeVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Type Node> ()

    override _.DefaultResult = missingNode

    override _.VisitUserDefinedType context =
        context.qualifiedName().GetText () |> TypeName |> toNode tokens context

type private ExpressionVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Expression Node> ()

    override _.DefaultResult = missingNode

    override _.VisitMissingExpression context =
        MissingExpression |> toNode tokens context

    override _.VisitIdentifierExpression context =
        context.qualifiedName().GetText () |> Literal |> toNode tokens context

    override _.VisitIntegerExpression context =
        context.IntegerLiteral().GetText () |> Literal |> toNode tokens context

    override this.VisitTupleExpression context =
        { OpenParen = (=) "(" |> findTerminal tokens context
          Items = context.expression () |> Array.toList |> List.map this.Visit
          CloseParen = (=) ")" |> findTerminal tokens context }
        |> Tuple
        |> toNode tokens context
        |> withoutTrailingTrivia

    override this.VisitAddExpression context =
        { Left = context.expression 0 |> this.Visit
          Operator = [ "+"; "-" ] |> flip List.contains |> findTerminal tokens context
          Right = context.expression 1 |> this.Visit }
        |> BinaryOperator
        |> toNode tokens context
        |> withoutTrailingTrivia

type private SymbolBindingVisitor (tokens) =
    inherit QSharpParserBaseVisitor<SymbolBinding Node> ()

    override _.DefaultResult = missingNode

    override _.VisitSymbolName context =
        context.Identifier () |> toTerminal tokens |> SymbolName |> toNode tokens context

    override this.VisitSymbolTuple context =
        context.symbolBinding ()
        |> Array.toList
        |> List.map this.Visit
        |> SymbolTuple
        |> toNode tokens context

type private StatementVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Statement Node> ()

    let expressionVisitor = ExpressionVisitor tokens

    let symbolBindingVisitor = SymbolBindingVisitor tokens

    override _.DefaultResult = missingNode

    override _.VisitReturnStatement context =
        { ReturnKeyword = (=) "return" |> findTerminal tokens context
          Expression = context.expression () |> expressionVisitor.Visit
          Semicolon = (=) ";" |> findTerminal tokens context }
        |> Return
        |> toNode tokens context
        |> withoutTrailingTrivia

    override _.VisitLetStatement context =
        { LetKeyword = (=) "let" |> findTerminal tokens context
          Binding = context.symbolBinding () |> symbolBindingVisitor.Visit
          Equals = (=) "=" |> findTerminal tokens context
          Expression = context.expression () |> expressionVisitor.Visit
          Semicolon = (=) ";" |> findTerminal tokens context }
        |> Let
        |> toNode tokens context
        |> withoutTrailingTrivia

type private NamespaceElementVisitor (tokens) =
    inherit QSharpParserBaseVisitor<NamespaceElement Node> ()

    let typeVisitor = TypeVisitor tokens

    let statementVisitor = StatementVisitor tokens

    override _.DefaultResult = missingNode

    override _.VisitCallableDeclaration context =
        let scope = context.callableBody().scope () // TODO
        { CallableKeyword = [ "function"; "operation" ] |> flip List.contains |> findTerminal tokens context
          Name = context.Identifier () |> toTerminal tokens
          Colon = (=) ":" |> findTerminal tokens context
          ReturnType = context.``type`` () |> typeVisitor.Visit
          OpenBrace = (=) "{" |> findTerminal tokens scope
          Statements = scope.statement() |> Array.toList |> List.map statementVisitor.Visit
          CloseBrace = (=) "}" |> findTerminal tokens scope }
        |> CallableDeclaration
        |> toNode tokens context
        |> withoutTrailingTrivia

let private toNamespaceToken tokens (context : QSharpParser.NamespaceContext) =
    let visitor = NamespaceElementVisitor tokens
    let name = context.qualifiedName ()
    { NamespaceKeyword = (=) "namespace" |> findTerminal tokens context
      Name = name.GetText () |> Terminal |> toNode tokens name
      OpenBrace = (=) "{" |> findTerminal tokens context
      Elements = context.namespaceElement () |> Array.toList |> List.map visitor.Visit
      CloseBrace = (=) "}" |> findTerminal tokens context }
    |> toNode tokens context
    |> withoutTrailingTrivia

let toProgramToken tokens (context : QSharpParser.ProgramContext) =
    context.``namespace`` ()
    |> Array.toList
    |> List.map (toNamespaceToken tokens)
    |> Program
    |> toNode tokens context
    |> withoutTrailingTrivia
