# QsFmt

QsFmt is a source code formatter for Q#.
It's only an early prototype right now.

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
