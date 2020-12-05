namespace QsFmt.Formatter.Tests

open QsFmt.Formatter
open System
open System.Reflection
open Xunit

type Example =
    { Name: string
      Before: string
      After: string }

    override example.ToString() = example.Name

type FixedPoint =
    { Name: string
      Source: string }

    override fixedPoint.ToString() = fixedPoint.Name

module internal Example =
    let toFixedPoint (example: Example) =
        { Name = example.Name
          Source = example.After }

type internal ExampleAttribute() =
    inherit Attribute()

type internal FixedPointAttribute() =
    inherit Attribute()

module Discoverer =
    let private properties (attribute: Type) =
        Assembly.GetCallingAssembly().GetTypes()
        |> Seq.collect (fun typ -> typ.GetProperties())
        |> Seq.filter (fun property ->
            property.GetCustomAttributes attribute
            |> Seq.isEmpty
            |> not)

    let private examples =
        properties typeof<ExampleAttribute>
        |> Seq.choose (fun property ->
            match property.GetValue null with
            | :? (string * string) as example ->
                { Name = property.Name
                  Before = fst example
                  After = snd example }
                |> Some
            | _ -> None)

    let private fixedPoints =
        properties typeof<FixedPointAttribute>
        |> Seq.choose (fun property ->
            match property.GetValue null with
            | :? string as source ->
                { Name = property.Name
                  Source = source }
                |> Some
            | _ -> None)

    type private ExampleData() as data =
        inherit TheoryData<Example>()

        do examples |> Seq.iter data.Add

    type private FixedPointData() as data =
        inherit TheoryData<FixedPoint>()

        do
            examples
            |> Seq.map Example.toFixedPoint
            |> Seq.append fixedPoints
            |> Seq.iter data.Add

    [<Theory>]
    [<ClassData(typeof<ExampleData>)>]
    let ``Code is formatted correctly`` example =
        Assert.Equal(example.After, Formatter.format example.Before)

    [<Theory>]
    [<ClassData(typeof<FixedPointData>)>]
    let ``Formatted code is unchanged`` fixedPoint =
        Assert.Equal(fixedPoint.Source, Formatter.format fixedPoint.Source)
