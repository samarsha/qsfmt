module internal QsFmt.Formatter.Printer

#nowarn "40"

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

let private printToken printNode node =
    (node.Kind |> Option.map printNode |> Option.defaultValue "")
    + node.TrailingTrivia

let private printTerminal = printToken <| fun (Terminal text) -> text

let private printSequenceItem printItem (item : _ SequenceItem) =
    printItem item.Item + printTerminal item.Comma

let private printSequenceItems printItem = List.map (printSequenceItem printItem) >> String.concat ""

let rec private printCharacteristic = printToken <| function
    | Adjoint -> "Adj"
    | Controlled -> "Ctl"
    | CharacteristicGroup group ->
        printTerminal group.OpenParen
        + printCharacteristic group.Characteristic
        + printTerminal group.CloseParen
    | CharacteristicBinaryOperator operator ->
        printCharacteristic operator.Left
        + printTerminal operator.Operator
        + printCharacteristic operator.Right

let private printCharacteristics = printToken <| fun characteristics ->
    printTerminal characteristics.IsKeyword
    + printCharacteristic characteristics.Characteristic

let rec private printType = printToken <| function
    | MissingType -> "_"
    | TypeParameter name -> name
    | BigInt -> "BigInt"
    | Bool -> "Bool"
    | Double -> "Double"
    | Int -> "Int"
    | Pauli -> "Pauli"
    | Qubit -> "Qubit"
    | Range -> "Range"
    | Result -> "Result"
    | String -> "String"
    | Unit -> "Unit"
    | UserDefinedType name -> name
    | TupleType tuple ->
        printTerminal tuple.OpenParen
        + printSequenceItems printType tuple.Items
        + printTerminal tuple.CloseParen
    | ArrayType array ->
        printType array.BaseType
        + printTerminal array.OpenBracket
        + printTerminal array.CloseBracket
    | CallableType callable ->
        printTerminal callable.OpenParen
        + printTerminal callable.InnerOpenParen
        + printType callable.FromType
        + printTerminal callable.Arrow
        + printType callable.ToType
        + printTerminal callable.InnerCloseParen
        + (callable.Characteristics |> Option.map printCharacteristics |> Option.defaultValue "")
        + printTerminal callable.CloseParen

let rec private printExpression = printToken <| function
    | MissingExpression -> "_"
    | Literal text -> text
    | Tuple tuple ->
        printTerminal tuple.OpenParen
        + printSequenceItems printExpression tuple.Items
        + printTerminal tuple.CloseParen
    | BinaryOperator operator ->
        printExpression operator.Left
        + printTerminal operator.Operator
        + printExpression operator.Right
    | Update update ->
        printExpression update.Record
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
        + printExpression letStmt.Value
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
