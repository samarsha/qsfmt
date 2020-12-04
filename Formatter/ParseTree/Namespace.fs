module internal QsFmt.Formatter.ParseTree.Namespace

open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.ParseTree.Statement
open QsFmt.Formatter.ParseTree.Type
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type
open QsFmt.Parser

type private ParameterVisitor(tokens) =
    inherit QSharpParserBaseVisitor<SymbolBinding>()

    let typeVisitor = TypeVisitor tokens

    override _.DefaultResult = failwith "Unknown symbol binding."

    override visitor.VisitNamedParameter context =
        context.namedItem () |> visitor.VisitNamedItem

    override _.VisitNamedItem context =
        { Name = context.name |> toTerminal tokens
          Type =
              { Colon = context.colon |> toTerminal tokens
                Type = context.itemType |> typeVisitor.Visit }
              |> Some }
        |> SymbolDeclaration

    override visitor.VisitParameterTuple context =
        let parameters =
            context._parameters |> Seq.map visitor.Visit

        let commas =
            context._commas |> Seq.map (toTerminal tokens)

        { OpenParen = context.openParen |> toTerminal tokens
          Items = tupleItems parameters commas
          CloseParen = context.closeParen |> toTerminal tokens }
        |> SymbolTuple

type private NamespaceItemVisitor(tokens) =
    inherit QSharpParserBaseVisitor<NamespaceItem>()

    let parameterVisitor = ParameterVisitor tokens
    let typeVisitor = TypeVisitor tokens
    let statementVisitor = StatementVisitor tokens

    override _.DefaultResult = failwith "Unknown namespace element."

    override _.VisitCallableElement context =
        let scope = context.callable.body.scope () // TODO

        { CallableKeyword = context.callable.keyword |> toTerminal tokens
          Name = context.callable.name |> toTerminal tokens
          Parameters = parameterVisitor.Visit context.callable.tuple
          ReturnType =
              { Colon = context.callable.colon |> toTerminal tokens
                Type = typeVisitor.Visit context.callable.returnType }
          Block =
              { OpenBrace = scope.openBrace |> toTerminal tokens
                Items =
                    scope._statements
                    |> Seq.map statementVisitor.Visit
                    |> List.ofSeq
                CloseBrace = scope.closeBrace |> toTerminal tokens } }
        |> CallableDeclaration

let private toNamespace tokens (context: QSharpParser.NamespaceContext) =
    let visitor = NamespaceItemVisitor tokens

    { NamespaceKeyword = context.keyword |> toTerminal tokens
      Name =
          { Prefix = prefix tokens context.name.Start.TokenIndex
            Text = context.name.GetText() }
      Block =
          { OpenBrace = context.openBrace |> toTerminal tokens
            Items =
                context._elements
                |> Seq.map visitor.Visit
                |> List.ofSeq
            CloseBrace = context.closeBrace |> toTerminal tokens } }

let toProgram tokens (context: QSharpParser.ProgramContext) =
    let namespaces =
        context.``namespace`` ()
        |> Array.toList
        |> List.map (toNamespace tokens)

    let eof =
        { (context.eof |> toTerminal tokens) with
              Text = "" }

    { Namespaces = namespaces; Eof = eof }
