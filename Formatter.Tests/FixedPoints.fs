module QsFmt.Formatter.Tests.AlreadyFormatted

open QsFmt.Formatter.Tests

[<FixedPoint>]
let ``Preserves namespace comments`` = """/// The Foo namespace.
namespace Foo {}

/// The Bar namespace.
namespace Bar {}

// End of file."""

[<FixedPoint>]
let ``Preserves function with one parameter`` = """namespace Foo {
    function Bar(x : Int) : Int {
        return x;
    }
}"""

[<FixedPoint>]
let ``Preserves function with two parameters`` = """namespace Foo {
    function Bar(x : Int, y : Int) : Int {
        return x + y;
    }
}"""
