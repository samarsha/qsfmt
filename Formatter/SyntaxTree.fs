module internal QsFmt.Formatter.SyntaxTree

type 'a Node =
    { Node : 'a
      TrailingTrivia : string option }

type Expression =
    | InvalidExpression

type SymbolTuple =
    | Symbol of string
    | Symbols of SymbolTuple Node list
    | InvalidSymbolTuple

type Statement =
    | Return of Expression Node
    | Let of SymbolTuple Node * Expression Node
    | InvalidStatement

type NamespaceElement =
    | OpenDirective
    | TypeDeclaration
    | CallableDeclaration of Statement Node list
    | InvalidNamespaceElement

type Namespace = Namespace of NamespaceElement Node list

type Program = Program of Namespace Node list
