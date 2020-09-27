module internal QsFmt.Formatter.Rules

open System.Text.RegularExpressions

#nowarn "40"

open QsFmt.Formatter.SyntaxTree

let private mapTokenTrivia mapTrivia mapNode = function
    | Node node ->
        { node with
              Node = mapNode node.Node
              TrailingTrivia = mapTrivia node.TrailingTrivia }
        |> Node
    | Missing -> Missing

let rec private mapExpressionTrivia f = mapTokenTrivia f <| function
    | Literal text -> Literal text
    | Tuple tuple ->
        { OpenParen = mapTokenTrivia f id tuple.OpenParen
          Items = tuple.Items |> List.map (mapExpressionTrivia f)
          CloseParen = mapTokenTrivia f id tuple.CloseParen }
        |> Tuple
    | BinaryOperator operator ->
        { Left = mapExpressionTrivia f operator.Left
          Operator = mapTokenTrivia f id operator.Operator
          Right = mapExpressionTrivia f operator.Right }
        |> BinaryOperator

let rec private mapSymbolTupleTrivia f = mapTokenTrivia f <| function
    | Symbol symbol -> mapTokenTrivia f id symbol |> Symbol
    | Symbols tuples -> tuples |> List.map (mapSymbolTupleTrivia f) |> Symbols

let private mapStatementTrivia f = mapTokenTrivia f <| function
    | Return returnStmt ->
        { ReturnKeyword = mapTokenTrivia f id returnStmt.ReturnKeyword
          Expression = mapExpressionTrivia f returnStmt.Expression
          Semicolon = mapTokenTrivia f id returnStmt.Semicolon }
        |> Return
    | Let letStmt ->
        { LetKeyword = mapTokenTrivia f id letStmt.LetKeyword
          SymbolTuple = mapSymbolTupleTrivia f letStmt.SymbolTuple
          Equals = mapTokenTrivia f id letStmt.Equals
          Expression = mapExpressionTrivia f letStmt.Expression
          Semicolon = mapTokenTrivia f id letStmt.Semicolon }
        |> Let

let private mapNamespaceElementTrivia f = mapTokenTrivia f <| function
    | CallableDeclaration callable ->
        { CallableKeyword = mapTokenTrivia f id callable.CallableKeyword
          Name = mapTokenTrivia f id callable.Name
          Colon = mapTokenTrivia f id callable.Colon
          ReturnType = mapTokenTrivia f id callable.ReturnType
          OpenBrace = mapTokenTrivia f id callable.OpenBrace
          Statements = callable.Statements |> List.map (mapStatementTrivia f)
          CloseBrace = mapTokenTrivia f id callable.CloseBrace }
        |> CallableDeclaration

let private mapNamespaceTrivia f = mapTokenTrivia f <| fun ns ->
    { NamespaceKeyword = mapTokenTrivia f id ns.NamespaceKeyword
      Name = mapTokenTrivia f id ns.Name
      OpenBrace = mapTokenTrivia f id ns.OpenBrace
      Elements = ns.Elements |> List.map (mapNamespaceElementTrivia f)
      CloseBrace = mapTokenTrivia f id ns.CloseBrace }

let private mapProgramTrivia f = mapTokenTrivia f <| fun (Program namespaces) ->
    namespaces |> List.map (mapNamespaceTrivia f) |> Program

let collapseSpaces = mapProgramTrivia <| fun trivia ->
    let sameLine, nextLines =
        if trivia.Contains '\n'
        then
            trivia.Substring (0, trivia.IndexOf '\n'),
            trivia.IndexOf '\n' |> trivia.Substring
        else trivia, ""
    Regex.Replace (sameLine, " +", " ") + nextLines
