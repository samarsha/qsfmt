module internal QsFmt.Formatter.SyntaxTree.Statement

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Node

type SymbolBinding =
    | SymbolName of Terminal Node
    | SymbolTuple of SymbolBinding list

type Return =
    { ReturnKeyword: Terminal Node
      Expression: Expression
      Semicolon: Terminal Node }

type Let =
    { LetKeyword: Terminal Node
      Binding: SymbolBinding
      Equals: Terminal Node
      Value: Expression
      Semicolon: Terminal Node }

type Statement =
    | Return of Return
    | Let of Let
