module internal QsFmt.Formatter.SyntaxTree.Namespace

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Statement
open QsFmt.Formatter.SyntaxTree.Type

type CallableDeclaration =
    { CallableKeyword: Terminal
      Name: Terminal
      Parameters: SymbolBinding
      ReturnType: TypeAnnotation
      Block: Statement Block }

type NamespaceItem = CallableDeclaration of CallableDeclaration

type Namespace =
    { NamespaceKeyword: Terminal
      Name: Terminal
      Block: NamespaceItem Block }

type Program =
    { Namespaces: Namespace list
      Eof: Terminal }
