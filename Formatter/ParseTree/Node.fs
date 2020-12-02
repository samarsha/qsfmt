module internal QsFmt.Formatter.ParseTree.Node

open Antlr4.Runtime
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Parser
open System.Collections.Immutable

let private hiddenTokensBefore (tokens: IToken ImmutableArray) index =
    seq {
        for i = index - 1 downto 0 do
            tokens.[i]
    }
    |> Seq.takeWhile (fun token -> token.Channel = QSharpLexer.Hidden)
    |> Seq.rev

let prefix tokens index =
    hiddenTokensBefore tokens index
    |> Seq.map (fun token -> token.Text)
    |> String.concat ""
    |> Trivia.ofString

let toTerminal tokens (terminal: IToken) =
    { Prefix = prefix tokens terminal.TokenIndex
      Text = terminal.Text }
