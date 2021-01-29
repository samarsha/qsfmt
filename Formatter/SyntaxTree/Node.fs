﻿namespace QsFmt.Formatter.SyntaxTree

open System.Text.RegularExpressions

/// A contiguous region of whitespace.
type internal Whitespace = private Whitespace of string

module internal Whitespace =
    /// Renders this whitespace as a string.
    let toString (Whitespace ws) = ws

/// A comment.
type internal Comment = private Comment of string

module internal Comment =
    /// Renders this comment as a string, including the prefix.
    let toString (Comment comment) = comment

/// A trivia node is any piece of the source code that isn't relevant to the grammar.
type internal Trivia =
    /// A contiguous region of whitespace.
    | Whitespace of Whitespace

    /// A new line character.
    | NewLine

    /// A comment.
    | Comment of Comment

module internal Trivia =
    /// <summary>
    /// A trivia node containing <paramref name="count"/> number of space characters.
    /// </summary>
    let spaces count =
        String.replicate count " "
        |> Whitespace.Whitespace
        |> Whitespace

    /// Replaces each occurrence of more than one whitespace character in a row with a single space.
    let collapseSpaces =
        let replace str = Regex.Replace(str, "\s+", " ")

        function
        | Whitespace (Whitespace.Whitespace ws) -> Whitespace(replace ws |> Whitespace.Whitespace)
        | NewLine -> NewLine
        | Comment comment -> Comment comment

    /// <summary>
    /// Matches if the <paramref name="pattern"/> regex matches the start of the <paramref name="input"/> string. Yields
    /// the value of the match and the remaining string.
    /// </summary>
    let private (|Prefix|_|) (pattern: string) (input: string) =
        let result = Regex.Match(input, "^" + pattern)

        if result.Success
        then Some(result.Value, input.[result.Length..])
        else None

    /// <summary>
    /// Converts a string into a list of trivia nodes.
    /// </summary>
    /// <exception cref="System.Exception">The string contains invalid trivia.</exception>
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
        | _ ->
            // TODO: Use option.
            failwith "String contains invalid trivia."

/// A terminal symbol has no child nodes and represents a token in the source code.
type internal Terminal =
    { /// The trivia preceding the terminal.
      Prefix: Trivia list

      /// The text content of the terminal.
      Text: string }

module internal Terminal =
    /// <summary>
    /// Maps <paramref name="terminal"/> by applying <paramref name="mapper"/> to its trivia prefix.
    /// </summary>
    let mapPrefix mapper terminal =
        { terminal with
              Prefix = mapper terminal.Prefix }

/// An item in a comma-separated sequence.
type internal 'a SequenceItem =
    { /// The item.
      Item: 'a option

      /// The comma following the item.
      Comma: Terminal option }

/// A tuple.
type internal 'a Tuple =
    { /// The opening parenthesis.
      OpenParen: Terminal

      /// The items in the tuple.
      Items: 'a SequenceItem list

      /// The closing parenthesis.
      CloseParen: Terminal }

/// A block.
type internal 'a Block =
    { /// The opening brace.
      OpenBrace: Terminal

      /// The items in the block.
      Items: 'a list

      /// The closing brace.
      CloseBrace: Terminal }
