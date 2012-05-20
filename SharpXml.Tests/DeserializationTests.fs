namespace SharpXml.Tests

module DeserializationTests =

    open System
    open System.Diagnostics
    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers
    open SharpXml.Tests.Types

    let deserialize<'a> input =
        XmlSerializer.DeserializeFromString<'a>(input)

    [<Test>]
    let deserialize01() =
        let out = deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>"
        out.V1 |> should equal 42
        out.V2 |> should equal "bar"

    [<Test>]
    let deserialize02() =
        let out = deserialize<TestClass3> "<testClass><v1><item>foo</item><item>bar</item></v1><v2>42</v2></testClass>"
        out.V1.Length |> should equal 2
        out.V1.[0] |> should equal "bar"
        out.V1.[1] |> should equal "foo"
        out.V2 |> should equal 42

    [<Test>]
    let time01() =
        time (fun () -> deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>" |> ignore) 1000
        time (fun () -> deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>" |> ignore) 10000