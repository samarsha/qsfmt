module internal QsFmt.Formatter.Rules

open System.Text.RegularExpressions

#nowarn "40"

open QsFmt.Formatter.SyntaxTree

let private mapNode mapTrivia mapKind node =
    { node with
          Kind = node.Kind |> Option.map mapKind
          TrailingTrivia = mapTrivia node.TrailingTrivia }

let rec private mapExpressionTrivia f = mapNode f <| function
    | Literal text -> Literal text
    | Tuple tuple ->
        { OpenParen = mapNode f id tuple.OpenParen
          Items = tuple.Items |> List.map (mapExpressionTrivia f)
          CloseParen = mapNode f id tuple.CloseParen }
        |> Tuple
    | BinaryOperator operator ->
        { Left = mapExpressionTrivia f operator.Left
          Operator = mapNode f id operator.Operator
          Right = mapExpressionTrivia f operator.Right }
        |> BinaryOperator

let rec private mapSymbolTupleTrivia f = mapNode f <| function
    | Symbol symbol -> mapNode f id symbol |> Symbol
    | Symbols tuples -> tuples |> List.map (mapSymbolTupleTrivia f) |> Symbols

let private mapStatementTrivia f = mapNode f <| function
    | Return returnStmt ->
        { ReturnKeyword = mapNode f id returnStmt.ReturnKeyword
          Expression = mapExpressionTrivia f returnStmt.Expression
          Semicolon = mapNode f id returnStmt.Semicolon }
        |> Return
    | Let letStmt ->
        { LetKeyword = mapNode f id letStmt.LetKeyword
          SymbolTuple = mapSymbolTupleTrivia f letStmt.SymbolTuple
          Equals = mapNode f id letStmt.Equals
          Expression = mapExpressionTrivia f letStmt.Expression
          Semicolon = mapNode f id letStmt.Semicolon }
        |> Let

let private mapNamespaceElementTrivia f = mapNode f <| function
    | CallableDeclaration callable ->
        { CallableKeyword = mapNode f id callable.CallableKeyword
          Name = mapNode f id callable.Name
          Colon = mapNode f id callable.Colon
          ReturnType = mapNode f id callable.ReturnType
          OpenBrace = mapNode f id callable.OpenBrace
          Statements = callable.Statements |> List.map (mapStatementTrivia f)
          CloseBrace = mapNode f id callable.CloseBrace }
        |> CallableDeclaration

let private mapNamespaceTrivia f = mapNode f <| fun ns ->
    { NamespaceKeyword = mapNode f id ns.NamespaceKeyword
      Name = mapNode f id ns.Name
      OpenBrace = mapNode f id ns.OpenBrace
      Elements = ns.Elements |> List.map (mapNamespaceElementTrivia f)
      CloseBrace = mapNode f id ns.CloseBrace }

let private mapProgramTrivia f = mapNode f <| fun (Program namespaces) ->
    namespaces |> List.map (mapNamespaceTrivia f) |> Program

let collapseSpaces = mapProgramTrivia <| fun trivia ->
    let sameLine, nextLines =
        if trivia.Contains '\n'
        then
            trivia.Substring (0, trivia.IndexOf '\n'),
            trivia.IndexOf '\n' |> trivia.Substring
        else trivia, ""
    Regex.Replace (sameLine, " +", " ") + nextLines

let singleSpaceAfterLetBinding =
    let mapStatement = mapNode id <| function
        | Let letStmt ->
            let symbolTuple = { letStmt.SymbolTuple with TrailingTrivia = " " }
            Let { letStmt with SymbolTuple = symbolTuple }
        | statement -> statement

    let mapNamespaceElement = mapNode id <| function
        | CallableDeclaration callable ->
            { callable with Statements = callable.Statements |> List.map mapStatement }
            |> CallableDeclaration

    let mapNamespace = mapNode id <| fun ns ->
        { ns with Elements = ns.Elements |> List.map mapNamespaceElement }

    mapNode id <| fun (Program namespaces) ->
        namespaces |> List.map mapNamespace |> Program
