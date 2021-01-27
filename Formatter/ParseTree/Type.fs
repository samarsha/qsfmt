namespace QsFmt.Formatter.ParseTree

open QsFmt.Formatter.SyntaxTree
open QsFmt.Parser

type internal TypeVisitor(tokens) =
    inherit QSharpParserBaseVisitor<Type>()

    override _.DefaultResult = failwith "Unknown type."

    override _.VisitChildren node =
        Node.toUnknown tokens node |> UnknownType

    override _.VisitIntType context =
        context.Int().Symbol
        |> Node.toTerminal tokens
        |> BuiltInType

    override _.VisitUserDefinedType context =
        { Prefix = Node.prefix tokens context.name.Start.TokenIndex
          Text = context.name.GetText() }
        |> UserDefinedType
