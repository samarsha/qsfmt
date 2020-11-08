module internal QsFmt.Formatter.ParseTree.Type

open QsFmt.Formatter.ParseTree.Node
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Type
open QsFmt.Parser

type TypeVisitor (tokens) =
    inherit QSharpParserBaseVisitor<Type Node> ()

    override _.DefaultResult = failwith "Unknown type."

    override _.VisitIntType context = Int |> toNode tokens context

    override _.VisitUserDefinedType context = context.name.GetText () |> UserDefinedType |> toNode tokens context
