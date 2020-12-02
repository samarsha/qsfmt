module internal QsFmt.Formatter.Rules

#nowarn "40"

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement

let private mapTerminal mapPrefix mapText node =
    { node with
          Prefix = mapPrefix node.Prefix
          Text = mapText node.Text }

let private mapSequenceItem mapComma mapItem item =
    { item with
          Item = item.Item |> Option.map mapItem
          Comma = item.Comma |> Option.map mapComma }

let rec private mapExpressionPrefix f =
    function
    | MissingExpression terminal -> terminal |> mapTerminal f id |> MissingExpression
    | Literal terminal -> terminal |> mapTerminal f id |> Literal
    | Tuple tuple ->
        Tuple
            { OpenParen = mapTerminal f id tuple.OpenParen
              Items =
                  tuple.Items
                  |> List.map (mapSequenceItem (mapTerminal f id) (mapExpressionPrefix f))
              CloseParen = mapTerminal f id tuple.CloseParen }
    | BinaryOperator operator ->
        BinaryOperator
            { Left = mapExpressionPrefix f operator.Left
              Operator = mapTerminal f id operator.Operator
              Right = mapExpressionPrefix f operator.Right }
    | Update update ->
        Update
            { Record = mapExpressionPrefix f update.Record
              With = mapTerminal f id update.With
              Item = mapExpressionPrefix f update.Item
              Arrow = mapTerminal f id update.Arrow
              Value = mapExpressionPrefix f update.Value }

let rec private mapSymbolTuplePrefix f =
    function
    | SymbolName symbol -> mapTerminal f id symbol |> SymbolName
    | SymbolTuple tuples ->
        tuples
        |> List.map (mapSymbolTuplePrefix f)
        |> SymbolTuple

let private mapStatementPrefix f =
    function
    | Return returnStmt ->
        Return
            { ReturnKeyword = mapTerminal f id returnStmt.ReturnKeyword
              Expression = mapExpressionPrefix f returnStmt.Expression
              Semicolon = mapTerminal f id returnStmt.Semicolon }
    | Let letStmt ->
        Let
            { LetKeyword = mapTerminal f id letStmt.LetKeyword
              Binding = mapSymbolTuplePrefix f letStmt.Binding
              Equals = mapTerminal f id letStmt.Equals
              Value = mapExpressionPrefix f letStmt.Value
              Semicolon = mapTerminal f id letStmt.Semicolon }

let private mapNamespaceElementPrefix f =
    function
    | CallableDeclaration callable ->
        CallableDeclaration
            { CallableKeyword = mapTerminal f id callable.CallableKeyword
              Name = mapTerminal f id callable.Name
              Colon = mapTerminal f id callable.Colon
              ReturnType = callable.ReturnType // TODO
              OpenBrace = mapTerminal f id callable.OpenBrace
              Statements =
                  callable.Statements
                  |> List.map (mapStatementPrefix f)
              CloseBrace = mapTerminal f id callable.CloseBrace }

let private mapNamespacePrefix f ns =
    { NamespaceKeyword = mapTerminal f id ns.NamespaceKeyword
      Name = mapTerminal f id ns.Name
      OpenBrace = mapTerminal f id ns.OpenBrace
      Elements =
          ns.Elements
          |> List.map (mapNamespaceElementPrefix f)
      CloseBrace = mapTerminal f id ns.CloseBrace }

let private mapProgramPrefix f program =
    let namespaces =
        program.Namespaces
        |> List.map (mapNamespacePrefix f)

    { Namespaces = namespaces
      Eof = mapTerminal f id program.Eof }

let rec private mapWithPrevious previous f =
    function
    | [] -> []
    | x :: xs -> f previous x :: mapWithPrevious (Some x) f xs

let collapseSpaces =
    fun previous trivia ->
        match previous, trivia with
        | Some NewLine, Whitespace _ -> trivia
        | _, Whitespace _ -> Trivia.collapseSpaces trivia
        | _ -> trivia
    |> mapWithPrevious None
    |> mapProgramPrefix

let singleSpaceAfterLetBinding program =
    let mapStatement =
        function
        | Let letStmt ->
            let equals =
                { letStmt.Equals with
                      Prefix = [ Trivia.spaces 1 ] }

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
