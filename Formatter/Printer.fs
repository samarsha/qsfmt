module internal QsFmt.Formatter.Printer

#nowarn "40"

open System

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Namespace
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

let private printTrivia =
    function
    | Whitespace ws -> Whitespace.toString ws
    | NewLine -> Environment.NewLine
    | Comment comment -> Comment.toString comment

let private printPrefix = List.map printTrivia >> String.concat ""

let private printTerminal terminal =
    printPrefix terminal.Prefix + terminal.Text

let private printSequenceItem printItem (item: _ SequenceItem) =
    (item.Item
     |> Option.map printItem
     |> Option.defaultValue "")
    + (item.Comma
       |> Option.map printTerminal
       |> Option.defaultValue "")

let private printList printItem = List.map printItem >> String.concat ""

let rec private printCharacteristic =
    function
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

let private printCharacteristics characteristics =
    printTerminal characteristics.IsKeyword
    + printCharacteristic characteristics.Characteristic

let rec private printType =
    function
    | MissingType terminal
    | TypeParameter terminal
    | BuiltInType terminal
    | UserDefinedType terminal -> printTerminal terminal
    | TupleType tuple ->
        printTerminal tuple.OpenParen
        + (tuple.Items
           |> printList (printSequenceItem printType))
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
        + (callable.Characteristics
           |> Option.map printCharacteristics
           |> Option.defaultValue "")
        + printTerminal callable.CloseParen

let rec private printExpression =
    function
    | MissingExpression terminal
    | Literal terminal -> printTerminal terminal
    | Tuple tuple ->
        printTerminal tuple.OpenParen
        + (tuple.Items
           |> printList (printSequenceItem printExpression))
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

let rec private printSymbolTuple =
    function
    | SymbolName symbol -> printTerminal symbol
    | SymbolTuple tuples -> printList printSymbolTuple tuples

let rec private printStatement =
    function
    | Return returns ->
        printTerminal returns.ReturnKeyword
        + printExpression returns.Expression
        + printTerminal returns.Semicolon
    | Let lets ->
        printTerminal lets.LetKeyword
        + printSymbolTuple lets.Binding
        + printTerminal lets.Equals
        + printExpression lets.Value
        + printTerminal lets.Semicolon
    | If ifs ->
        printTerminal ifs.IfKeyword
        + printTerminal ifs.OpenParen
        + printExpression ifs.Condition
        + printTerminal ifs.CloseParen
        + printTerminal ifs.OpenBrace
        + printList printStatement ifs.Statements
        + printTerminal ifs.CloseBrace
    | Else elses ->
        printTerminal elses.ElseKeyword
        + printTerminal elses.OpenBrace
        + printList printStatement elses.Statements
        + printTerminal elses.CloseBrace

let private printNamespaceElement =
    function
    | CallableDeclaration callable ->
        let statements =
            callable.Statements
            |> List.map printStatement
            |> String.concat ""

        printTerminal callable.CallableKeyword
        + printTerminal callable.Name
        + "()"
        + printTerminal callable.Colon
        + printType callable.ReturnType
        + printTerminal callable.OpenBrace
        + statements
        + printTerminal callable.CloseBrace

let private printNamespace ns =
    let elements =
        ns.Elements
        |> List.map printNamespaceElement
        |> String.concat ""

    printTerminal ns.NamespaceKeyword
    + printTerminal ns.Name
    + printTerminal ns.OpenBrace
    + elements
    + printTerminal ns.CloseBrace

let printProgram program =
    let namespaces =
        program.Namespaces
        |> List.map printNamespace
        |> String.concat ""

    namespaces + printPrefix program.Eof.Prefix
