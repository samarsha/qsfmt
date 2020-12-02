module internal QsFmt.Formatter.ParseTree.Type

open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Type
open QsFmt.Parser

type TypeVisitor(tokens) =
    inherit QSharpParserBaseVisitor<Type>()

    override _.DefaultResult = failwith "Unknown type."

    override _.VisitIntType context =
        context.Int().Symbol
        |> toTerminal tokens
        |> BuiltInType

    override _.VisitUserDefinedType context =
        { Prefix = prefix tokens context.name.Start.TokenIndex
          Text = context.name.GetText() }
        |> UserDefinedType
