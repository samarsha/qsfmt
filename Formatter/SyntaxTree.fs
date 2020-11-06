module internal QsFmt.Formatter.SyntaxTree

type 'a Node =
    { Kind : 'a option
      TrailingTrivia : string }

type Terminal = Terminal of string

type Type =
    | TypeName of string

type Tuple =
    { OpenParen : Terminal Node
      Items : Expression Node list
      CloseParen : Terminal Node }

and BinaryOperator =
    { Left : Expression Node
      Operator : Terminal Node
      Right : Expression Node }

and Expression =
    | MissingExpression
    | Literal of string
    | Tuple of Tuple
    | BinaryOperator of BinaryOperator

type SymbolBinding =
    | SymbolName of Terminal Node
    | SymbolTuple of SymbolBinding Node list

type Return =
    { ReturnKeyword : Terminal Node
      Expression : Expression Node
      Semicolon : Terminal Node }

type Let =
    { LetKeyword : Terminal Node
      Binding : SymbolBinding Node
      Equals : Terminal Node
      Expression : Expression Node
      Semicolon : Terminal Node }

type Statement =
    | Return of Return
    | Let of Let

type CallableDeclaration =
    { CallableKeyword : Terminal Node
      Name : Terminal Node
      Colon : Terminal Node
      ReturnType : Type Node
      OpenBrace : Terminal Node
      Statements : Statement Node list
      CloseBrace : Terminal Node }

type NamespaceElement =
    | CallableDeclaration of CallableDeclaration

type Namespace =
    { NamespaceKeyword : Terminal Node
      Name : Terminal Node
      OpenBrace : Terminal Node
      Elements : NamespaceElement Node list
      CloseBrace : Terminal Node }

type Program = Program of Namespace Node list

let missingNode = { Kind = None; TrailingTrivia = "" }

let withoutTrailingTrivia node = { node with TrailingTrivia = "" }
