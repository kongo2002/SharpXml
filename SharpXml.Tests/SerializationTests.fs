namespace SharpXml.Tests

module SerializationTests =

    open System
    open System.Collections.Generic
    open System.IO
    open NUnit.Framework

    open SharpXml
    open SharpXml.ValueTypeSerializer
    open SharpXml.Tests.TestHelpers
    open SharpXml.Tests.Types

    [<Test>]
    let serializeDateTime01() =
        let writer = new StringWriter()
        let curr = DateTime.Now
        let date = curr.Date
        Serializer<DateTime>.WriteTag(writer, "date", date)
        writer.ToString() |> should equal (sprintf "<date>%s</date>" (date.ToString("yyyy-MM-dd")))

    [<Test>]
    let serializeFloat() =
        let writer = new StringWriter()
        let value = 2.528
        Serializer.WriteTag(writer, "float", value)
        writer.ToString() |> should equal (sprintf "<float>%.3f</float>" value)

    [<Test>]
    let serializeClass01() =
        let writer = new StringWriter()
        let cls = TestClass(800, "foo bar")
        let func = Serializer<TestClass>.GetWriterFunc()
        func writer cls
        writer.ToString() |> should equal "<v1>800</v1><v2>foo bar</v2>"

    [<Test>]
    let serializeDict01() =
        let writer = new StringWriter()
        let dict = Dictionary<string, int>()
        dict.Add("foo", 42)
        dict.Add("bar", 200)
        let func = Serializer<Dictionary<string,int>>.GetWriterFunc()
        func writer dict
        writer.ToString() |> should equal "<key>foo</key><value>42</value><key>bar</key><value>200</value>"

    [<Test>]
    let serializeDict02() =
        let writer = new StringWriter()
        let dict = Dictionary<string, int>()
        dict.Add("foo", 42)
        dict.Add("bar", 200)
        let func = Serializer<obj>.GetWriterFunc()
        func writer dict
        writer.ToString() |> should equal "<key>foo</key><value>42</value><key>bar</key><value>200</value>"