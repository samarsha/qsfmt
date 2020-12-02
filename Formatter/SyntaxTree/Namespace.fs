module internal QsFmt.Formatter.SyntaxTree.Namespace

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

type CallableDeclaration =
    { CallableKeyword: Terminal Node
      Name: Terminal Node
      Colon: Terminal Node
      ReturnType: Type Node
      OpenBrace: Terminal Node
      Statements: Statement Node list
      CloseBrace: Terminal Node }

type NamespaceElement = CallableDeclaration of CallableDeclaration

type Namespace =
    { NamespaceKeyword: Terminal Node
      Name: Terminal Node
      OpenBrace: Terminal Node
      Elements: NamespaceElement Node list
      CloseBrace: Terminal Node }

type Program =
    { Namespaces: Namespace Node list
      Eof: Terminal Node }
