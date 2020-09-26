module internal QsFmt.Formatter.SyntaxTree

type 'a Node =
    { Node : 'a
      TrailingTrivia : string }

type 'a Token =
    | Node of 'a Node
    | Missing

type Terminal = Terminal of string

type Tuple =
    { OpenParen : Terminal Token
      Items : Expression Token list
      CloseParen : Terminal Token }

and BinaryOperator =
    { Left : Expression Token
      Operator : Terminal Token
      Right : Expression Token }

and Expression =
    | Literal of string
    | Tuple of Tuple
    | BinaryOperator of BinaryOperator

type SymbolTuple =
    | Symbol of Terminal Token
    | Symbols of SymbolTuple Token list

type Return =
    { ReturnKeyword : Terminal Token
      Expression : Expression Token
      Semicolon : Terminal Token }

type Let =
    { LetKeyword : Terminal Token
      SymbolTuple : SymbolTuple Token
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
    { NamespaceKeyword : Terminal Token
      Name : Terminal Token
      OpenBrace : Terminal Token
      Elements : NamespaceElement Token list
      CloseBrace : Terminal Token }

type Program = Program of Namespace Token list
