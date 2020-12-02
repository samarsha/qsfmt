module internal QsFmt.Formatter.SyntaxTree.Namespace

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

type CallableDeclaration =
    { CallableKeyword: Terminal
      Name: Terminal
      Colon: Terminal
      ReturnType: Type
      OpenBrace: Terminal
      Statements: Statement list
      CloseBrace: Terminal }

type NamespaceElement = CallableDeclaration of CallableDeclaration

type Namespace =
    { NamespaceKeyword: Terminal
      Name: Terminal
      OpenBrace: Terminal
      Elements: NamespaceElement list
      CloseBrace: Terminal }

type Program =
    { Namespaces: Namespace list
      Eof: Terminal }
