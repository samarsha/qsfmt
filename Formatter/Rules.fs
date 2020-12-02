module internal QsFmt.Formatter.Rules

open System.Text.RegularExpressions

#nowarn "40"

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement

let private mapNode mapPrefix mapKind =
    Node.map
    <| fun node ->
        { node with
              Prefix = mapPrefix node.Prefix
              Kind = mapKind node.Kind }

let rec private mapSequenceItem mapComma mapItem item =
    { item with
          Item = mapItem item.Item
          Comma = mapComma item.Comma }

let rec private mapExpressionPrefix f =
    function
    | MissingExpression terminal -> terminal |> mapNode f id |> MissingExpression
    | Literal terminal -> terminal |> mapNode f id |> Literal
    | Tuple tuple ->
        Tuple
            { OpenParen = mapNode f id tuple.OpenParen
              Items =
                  tuple.Items
                  |> List.map (mapSequenceItem (mapNode f id) (mapExpressionPrefix f |> Option.map))
              CloseParen = mapNode f id tuple.CloseParen }
    | BinaryOperator operator ->
        BinaryOperator
            { Left = mapExpressionPrefix f operator.Left
              Operator = mapNode f id operator.Operator
              Right = mapExpressionPrefix f operator.Right }
    | Update update ->
        Update
            { Record = mapExpressionPrefix f update.Record
              With = mapNode f id update.With
              Item = mapExpressionPrefix f update.Item
              Arrow = mapNode f id update.Arrow
              Value = mapExpressionPrefix f update.Value }

let rec private mapSymbolTuplePrefix f =
    function
    | SymbolName symbol -> mapNode f id symbol |> SymbolName
    | SymbolTuple tuples ->
        tuples
        |> List.map (mapSymbolTuplePrefix f)
        |> SymbolTuple

let private mapStatementPrefix f =
    function
    | Return returnStmt ->
        Return
            { ReturnKeyword = mapNode f id returnStmt.ReturnKeyword
              Expression = mapExpressionPrefix f returnStmt.Expression
              Semicolon = mapNode f id returnStmt.Semicolon }
    | Let letStmt ->
        Let
            { LetKeyword = mapNode f id letStmt.LetKeyword
              Binding = mapSymbolTuplePrefix f letStmt.Binding
              Equals = mapNode f id letStmt.Equals
              Value = mapExpressionPrefix f letStmt.Value
              Semicolon = mapNode f id letStmt.Semicolon }

let private mapNamespaceElementPrefix f =
    function
    | CallableDeclaration callable ->
        CallableDeclaration
            { CallableKeyword = mapNode f id callable.CallableKeyword
              Name = mapNode f id callable.Name
              Colon = mapNode f id callable.Colon
              ReturnType = callable.ReturnType // TODO
              OpenBrace = mapNode f id callable.OpenBrace
              Statements =
                  callable.Statements
                  |> List.map (mapStatementPrefix f)
              CloseBrace = mapNode f id callable.CloseBrace }

let private mapNamespacePrefix f ns =
    { NamespaceKeyword = mapNode f id ns.NamespaceKeyword
      Name = mapNode f id ns.Name
      OpenBrace = mapNode f id ns.OpenBrace
      Elements =
          ns.Elements
          |> List.map (mapNamespaceElementPrefix f)
      CloseBrace = mapNode f id ns.CloseBrace }

let private mapProgramPrefix f program =
    let namespaces =
        program.Namespaces
        |> List.map (mapNamespacePrefix f)

    { Namespaces = namespaces
      Eof = mapNode f id program.Eof }

let collapseSpaces =
    mapProgramPrefix
    <| fun prefix ->
        let sameLine, nextLines =
            if prefix.Contains '\n'
            then prefix.Substring(0, prefix.IndexOf '\n'), prefix.IndexOf '\n' |> prefix.Substring
            else prefix, ""

        Regex.Replace(sameLine, " +", " ") + nextLines

let singleSpaceAfterLetBinding program =
    let mapStatement =
        function
        | Let letStmt ->
            let equals =
                letStmt.Equals
                |> Node.map (fun node -> { node with Prefix = " " })

            Let { letStmt with Equals = equals }
        | statement -> statement

    let mapNamespaceElement =
        function
        | CallableDeclaration callable ->
            CallableDeclaration
                { callable with
                      Statements = callable.Statements |> List.map mapStatement }

    let mapNamespace ns =
        { ns with
              Elements = ns.Elements |> List.map mapNamespaceElement }

    { program with
          Namespaces = program.Namespaces |> List.map mapNamespace }
