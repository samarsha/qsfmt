module QsFmt.Formatter.Tests.FixedPoints

open QsFmt.Formatter.Tests

[<FixedPoint>]
let ``Namespace comments`` = """/// The Foo namespace.
namespace Foo {}

/// The Bar namespace.
namespace Bar {}

// End of file."""

[<FixedPoint>]
let ``Function with one parameter`` = """namespace Foo {
    function Bar(x : Int) : Int {
        return x;
    }
}"""

[<FixedPoint>]
let ``Function with two parameters`` = """namespace Foo {
    function Bar(x : Int, y : Int) : Int {
        return x + y;
    }
}"""

[<FixedPoint(Skip = "Not supported.")>]
let ``Entry point and using statement`` = """namespace Microsoft.Quantum.Foo {
    @EntryPoint()
    operation RunProgram (nQubits : Int) : Unit {
        using (register = Qubit[nQubits]) {
            H(register[0]);
        }
    }
}"""

[<FixedPoint>]
let ``Open directives and operation`` = """namespace Foo {
    open Bar;
    open Baz;

    operation Spam () : Unit {}
}"""

[<FixedPoint(Skip = "Not supported.")>]
let ``Open directive and entry point`` = """namespace Foo {
    open Test;

    @EntryPoint()
    operation Bar () : Unit {}
}"""

[<FixedPoint(Skip = "Not supported.")>]
let ``Operation with comments`` = """namespace Foo {
    open Test;

    @EntryPoint()
    operation Bar () : Unit {
        // This is a comment
        Message("Bar");
        // Lorem ipsum
        // Dolor sit amet
    }
}"""
