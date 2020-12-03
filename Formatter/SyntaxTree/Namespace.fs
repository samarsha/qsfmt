module internal QsFmt.Formatter.SyntaxTree.Namespace

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

type CallableDeclaration =
    { CallableKeyword: Terminal
      Name: Terminal
      Colon: Terminal
      ReturnType: Type
      Block: Statement Block }

type NamespaceElement = CallableDeclaration of CallableDeclaration

type Namespace =
    { NamespaceKeyword: Terminal
      Name: Terminal
      Block: NamespaceElement Block }

type Program =
    { Namespaces: Namespace list
      Eof: Terminal }
