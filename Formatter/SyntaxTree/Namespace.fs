module internal QsFmt.Formatter.SyntaxTree.Namespace

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

type CallableDeclaration =
    { CallableKeyword: Terminal Node
      Name: Terminal Node
      Colon: Terminal Node
      ReturnType: Type
      OpenBrace: Terminal Node
      Statements: Statement list
      CloseBrace: Terminal Node }

type NamespaceElement = CallableDeclaration of CallableDeclaration

type Namespace =
    { NamespaceKeyword: Terminal Node
      Name: Terminal Node
      OpenBrace: Terminal Node
      Elements: NamespaceElement list
      CloseBrace: Terminal Node }

type Program =
    { Namespaces: Namespace list
      Eof: Terminal Node }
