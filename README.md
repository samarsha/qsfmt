# QsFmt

QsFmt is a source code formatter for Q#.

**Disclaimer:** QsFmt is very early in development and only supports a few basic formatting rules right now.

## Doesn't Q# already have a formatter?

The Q# compiler has a built-in formatter that you can use by running `qsc format`.
It formats code by converting it into an abstract syntax tree (AST) and back into a standard source code format.
The problem is that the AST it uses does not preserve any information about whitespace in the original code, so all formatting choices are lost (such as choosing to insert a line break in an expression).

QsFmt, in contrast, remembers everything about the original code.
It can convert a Q# program into an AST and back to source code, matching the original code character-for-character.
Formatting rules are applied on top of this, so incorrectly formatted code can be fixed without making additional unwanted changes.

For example, here is some badly formatted Q# code:

```qsharp
namespace     Foo {
    function Bar() : Int {
        let x= // Newlines are preserved.
            (7 -   1) // Comments too.
            + 4;
        return  x;
    }
}
```

Here is what happens after running QsFmt:

```qsharp
namespace Foo {
    function Bar() : Int {
        let x = // Newlines are preserved.
            (7 - 1) // Comments too.
            + 4;
        return x;
    }
}
```

And for comparison, here is what happens after running `qsc format`:

```qsharp
namespace Foo {
    
    function Bar () : Int {
        
        // Newlines are preserved.
        // Comments too.
        let x = (7 - 1) + 4;
        
        // Comments too.
        return x;
    }
    
}
```

## Design

QsFmt uses a [concrete syntax tree](https://en.wikipedia.org/wiki/Parse_tree), which is lossless: a Q# program parsed into a CST can be converted back into a string without any loss of information.
QsFmt's syntax tree is modeled on the Q# compiler's abstract syntax tree, but with additional information on every node for tokens like semicolons and curly braces that are unnecessary in an AST.
Whitespace and comment tokens, known as *trivia tokens*, are attached as a prefix to a non-trivia token.
For example, in the expression `x + y`, `x` has prefix `""`, `+` has prefix `" "`, and `y` has prefix `" "`.

QsFmt uses [ANTLR](https://www.antlr.org/) to parse Q# programs.
It uses a grammar based on the [grammar in the Q# language specification](https://github.com/microsoft/qsharp-language/tree/main/Specifications/Language/5_Grammar).
ANTLR's parse tree is then converted into QsFmt's concrete syntax tree.

Formatting rules are a mapping from one CST to another CST.
Then the formatting pipeline is:

1. Parse a Q# program into a CST.
2. Apply formatting rules to the CST in order.
3. Unparse the CST into a Q# program.

This allows formatting transformations (indentation, brace position, etc.) to be written separately, and in many cases may be independent from each other.
(However, there may be dependencies where one transformation must run before another.)
This will hopefully make the formatting transformations more modular and simpler to write.
