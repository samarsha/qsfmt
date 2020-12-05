﻿module QsFmt.Formatter.Tests.Formatting

[<Example>]
let ``Indents function`` =
    """namespace Foo {
function Bar() : Int {
let x = 5;
return x;
}
}""",

    """namespace Foo {
    function Bar() : Int {
        let x = 5;
        return x;
    }
}"""

[<Example>]
let ``Indents if-else statement`` =
    """namespace Foo {
function Bar() : Int {
let x = 5;
if (x == 5) {
return x;
} else {
return x + 1;
}
}
}""",

    """namespace Foo {
    function Bar() : Int {
        let x = 5;
        if (x == 5) {
            return x;
        } else {
            return x + 1;
        }
    }
}"""

[<Example>]
let ``Removes extraneous spaces`` =
    """namespace     Foo {
    function Bar() : Int {
        let x= // Newlines are preserved.
            (7 -   1) // Comments too.
            + 4;
        return  x w/ Foo <- (7, y);
    }
}""",

    """namespace Foo {
    function Bar() : Int {
        let x = // Newlines are preserved.
            (7 - 1) // Comments too.
            + 4;
        return x w/ Foo <- (7, y);
    }
}"""