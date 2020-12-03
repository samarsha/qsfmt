namespace QsFmt.Formatter.SyntaxTree.Statement

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Node

type internal SymbolBinding =
    | SymbolName of Terminal
    | SymbolTuple of SymbolBinding list

type internal Let =
    { LetKeyword: Terminal
      Binding: SymbolBinding
      Equals: Terminal
      Value: Expression
      Semicolon: Terminal }

type internal Return =
    { ReturnKeyword: Terminal
      Expression: Expression
      Semicolon: Terminal }

type internal Statement =
    | Let of Let
    | Return of Return

module internal Statement =
    let mapPrefix mapper =
        function
        | Let lets ->
            { lets with
                  LetKeyword = lets.LetKeyword |> Terminal.mapPrefix mapper }
            |> Let
        | Return returns ->
            { returns with
                  ReturnKeyword = returns.ReturnKeyword |> Terminal.mapPrefix mapper }
            |> Return
