module internal QsFmt.Formatter.SyntaxTree

type 'a Node =
    { Node : 'a
      TrailingTrivia : string }

type 'a Token =
    | Missing
    | Node of 'a Node

type Terminal = Terminal of string

type Expression =
    | InvalidExpression

type SymbolTuple =
    | Symbol of string
    | Symbols of SymbolTuple Token list

type Return =
    { Expression : Expression Token
      Semicolon : Terminal Token }

type Let =
    { SymbolTuple : SymbolTuple Token
      Equals : Terminal Token
      Expression : Expression Token
      Semicolon : Terminal Token }

type Statement =
    | Return of Return
    | Let of Let

type NamespaceElement =
    | OpenDirective
    | TypeDeclaration
    | CallableDeclaration of Statement Token list

type Namespace = Namespace of NamespaceElement Token list

type Program = Program of Namespace Token list
