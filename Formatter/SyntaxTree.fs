module internal QsFmt.Formatter.SyntaxTree

type 'a Node =
    { Node : 'a
      TrailingTrivia : string }

type 'a Token =
    | Node of 'a Node
    | Missing

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

type CallableDeclaration =
    { OpenBrace : Terminal Token
      Statements : Statement Token list
      CloseBrace : Terminal Token }

type NamespaceElement =
    | CallableDeclaration of CallableDeclaration

type Namespace =
    { OpenBrace : Terminal Token
      Elements : NamespaceElement Token list
      CloseBrace : Terminal Token }

type Program = Program of Namespace Token list
