namespace QsFmt.Formatter.SyntaxTree.Node

open System.Text.RegularExpressions

type internal Whitespace = private Whitespace of string

module internal Whitespace =
    let toString (Whitespace ws) = ws

type internal Comment = private Comment of string

module internal Comment =
    let toString (Comment comment) = comment

type internal Trivia =
    | Whitespace of Whitespace
    | NewLine
    | Comment of Comment

module internal Trivia =
    let spaces count =
        String.replicate count " "
        |> Whitespace.Whitespace
        |> Whitespace

    let collapseSpaces =
        let replace str = Regex.Replace(str, "\s+", " ")

        function
        | Whitespace (Whitespace.Whitespace ws) -> Whitespace(replace ws |> Whitespace.Whitespace)
        | NewLine -> NewLine
        | Comment comment -> Comment comment

    let private (|Prefix|_|) (pattern: string) (input: string) =
        let result = Regex.Match(input, "^" + pattern)

        if result.Success
        then Some(result.Value, input.[result.Length..])
        else None

    let rec ofString =
        function
        | "" -> []
        | Prefix "\r\n" (_, rest)
        | Prefix "\r" (_, rest)
        | Prefix "\n" (_, rest) -> NewLine :: ofString rest
        | Prefix "\s+" (result, rest) ->
            Whitespace(Whitespace.Whitespace result)
            :: ofString rest
        | Prefix "//[^\r\n]*" (result, rest) -> Comment(Comment.Comment result) :: ofString rest
        | _ -> failwith "String contains invalid trivia."

type internal Terminal = { Prefix: Trivia list; Text: string }

module internal Terminal =
    let mapPrefix mapper terminal =
        { terminal with
              Prefix = mapper terminal.Prefix }

type internal 'a SequenceItem =
    { Item: 'a option
      Comma: Terminal option }

type internal 'a Block =
    { OpenBrace: Terminal
      Items: 'a list
      CloseBrace: Terminal }
