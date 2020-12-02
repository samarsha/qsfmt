module internal QsFmt.Formatter.SyntaxTree.Statement

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Node

type SymbolBinding =
    | SymbolName of Terminal
    | SymbolTuple of SymbolBinding list

type Return =
    { ReturnKeyword: Terminal
      Expression: Expression
      Semicolon: Terminal }

type Let =
    { LetKeyword: Terminal
      Binding: SymbolBinding
      Equals: Terminal
      Value: Expression
      Semicolon: Terminal }

type Statement =
    | Return of Return
    | Let of Let
