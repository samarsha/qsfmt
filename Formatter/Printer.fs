module internal QsFmt.Formatter.Printer

#nowarn "40"

open QsFmt.Formatter.SyntaxTree

let private printToken printNode node =
    (node.Kind |> Option.map printNode |> Option.defaultValue "")
    + node.TrailingTrivia

let private printTerminal = printToken <| fun (Terminal text) -> text

let private printType = printToken <| function
    | Int -> "Int"
    | TypeName name -> name

let rec private printExpression = printToken <| function
    | MissingExpression -> "_"
    | Literal text -> text
    | Tuple tuple ->
        let items = tuple.Items |> List.map printExpression |> String.concat ""
        printTerminal tuple.OpenParen + items + printTerminal tuple.CloseParen
    | BinaryOperator operator ->
        printExpression operator.Left
        + printTerminal operator.Operator
        + printExpression operator.Right
    | Update update ->
        printExpression update.Base
        + printTerminal update.With
        + printExpression update.Item
        + printTerminal update.Arrow
        + printExpression update.Value

let rec private printSymbolTuple = printToken <| function
    | SymbolName symbol -> printTerminal symbol
    | SymbolTuple tuples -> tuples |> List.map printSymbolTuple |> String.concat ""

let private printStatement = printToken <| function
    | Return returnStmt ->
        printTerminal returnStmt.ReturnKeyword
        + printExpression returnStmt.Expression
        + printTerminal returnStmt.Semicolon
    | Let letStmt ->
        printTerminal letStmt.LetKeyword
        + printSymbolTuple letStmt.Binding
        + printTerminal letStmt.Equals
        + printExpression letStmt.Expression
        + printTerminal letStmt.Semicolon

let private printNamespaceElement = printToken <| function
    | CallableDeclaration callable ->
        let statements = callable.Statements |> List.map printStatement |> String.concat ""
        printTerminal callable.CallableKeyword
        + printTerminal callable.Name
        + "() "
        + printTerminal callable.Colon
        + printType callable.ReturnType
        + printTerminal callable.OpenBrace
        + statements
        + printTerminal callable.CloseBrace

let private printNamespace = printToken <| fun ns ->
    let elements = ns.Elements |> List.map printNamespaceElement |> String.concat ""
    printTerminal ns.NamespaceKeyword
    + printTerminal ns.Name
    + printTerminal ns.OpenBrace
    + elements
    + printTerminal ns.CloseBrace

let printProgram = printToken <| fun (Program namespaces) ->
    namespaces |> List.map printNamespace |> String.concat ""
