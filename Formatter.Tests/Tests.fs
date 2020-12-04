module QsFmt.Formatter.Tests

open Xunit

let private badExamples =
    [ "\
namespace Foo {
function Bar() : Int {
let x = 5;
return x;
}
}",
      "\
namespace Foo {
    function Bar() : Int {
        let x = 5;
        return x;
    }
}"

      "\
namespace Foo {
function Bar() : Int {
let x = 5;
if (x == 5) {
return x;
} else {
return x + 1;
}
}
}",
      "\
namespace Foo {
    function Bar() : Int {
        let x = 5;
        if (x == 5) {
            return x;
        } else {
            return x + 1;
        }
    }
}"

      "\
namespace     Foo {
    function Bar() : Int {
        let x= // Newlines are preserved.
            (7 -   1) // Comments too.
            + 4;
        return  x w/ Foo <- (7, y);
    }
}",
      "\
namespace Foo {
    function Bar() : Int {
        let x = // Newlines are preserved.
            (7 - 1) // Comments too.
            + 4;
        return x w/ Foo <- (7, y);
    }
}" ]

type private BadExampleData() as data =
    inherit TheoryData<string, string>()
    do badExamples |> List.iter data.Add

[<Theory>]
[<ClassData(typeof<BadExampleData>)>]
let ``Unformatted code is formatted correctly`` input output =
    Assert.Equal(output, Formatter.format input)

let private goodExamples =
    (badExamples |> List.map snd)
    @ [ "\
/// The Foo namespace.
namespace Foo {}

/// The Bar namespace.
namespace Bar {}

// End of file."

        "\
namespace Foo {
    function Bar(x : Int) : Int {
        return x;
    }
}"

        "\
namespace Foo {
    function Bar(x : Int, y : Int) : Int {
        return x + y;
    }
}" ]

type private GoodExampleData() as data =
    inherit TheoryData<string>()
    do goodExamples |> List.iter data.Add

[<Theory>]
[<ClassData(typeof<GoodExampleData>)>]
let ``Formatted code is unchanged`` input =
    Assert.Equal(input, Formatter.format input)
