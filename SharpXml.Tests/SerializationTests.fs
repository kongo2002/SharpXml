namespace SharpXml.Tests

module SerializationTests =

    open System
    open System.Collections.Generic
    open NUnit.Framework

    open SharpXml
    open SharpXml.ValueTypeSerializer
    open SharpXml.Tests.TestHelpers
    open SharpXml.Tests.Types

    let serialize<'a> (element : 'a) =
        XmlSerializer.SerializeToString<'a>(element)

    [<Test>]
    let serializeDateTime01() =
        let curr = DateTime.Now
        let date = curr.Date
        serialize date |> should equal (sprintf "<dateTime>%s</dateTime>" (date.ToString("yyyy-MM-dd")))

    [<Test>]
    let serializeFloat() =
        let value = 2.528
        serialize value |> should equal (sprintf "<double>%.3f</double>" value)

    [<Test>]
    let serializeClass01() =
        let cls = TestClass(800, "foo bar")
        serialize cls |> should equal "<testClass><v1>800</v1><v2>foo bar</v2></testClass>"

    [<Test>]
    let serializeClass02() =
        let cls = TestClass(800, "foo bar")
        serialize cls |> should equal "<testClass><v1>800</v1><v2>foo bar</v2></testClass>"

    [<Test>]
    let serializeDict01() =
        let dict = Dictionary<string, int>()
        dict.Add("foo", 42)
        dict.Add("bar", 200)
        serialize dict |> should equal "<dictionary><key>foo</key><value>42</value><key>bar</key><value>200</value></dictionary>"

    [<Test>]
    let serializeDict02() =
        let dict = Dictionary<int, string>()
        dict.Add(42, "foo")
        dict.Add(200, "bar")
        serialize dict |> should equal "<dictionary><key>42</key><value>foo</value><key>200</key><value>bar</value></dictionary>"

    [<Test>]
    let serializeArray01() =
        let array = [| 35; 200; 42 |]
        serialize array |> should equal "<array><item>35</item><item>200</item><item>42</item></array>"

    [<Test>]
    let serializeNestedClass01() =
        let cls = NestedClass(V1 = "foobar", V2 = SimpleClass(V1 = "bar foo", V2 = 200))
        serialize cls |> should equal "<nestedClass><v1>foobar</v1><v2><v1>bar foo</v1><v2>200</v2></v2></nestedClass>"

    [<Test>]
    let serializeNestedClass02() =
        let cls = NestedClass2(V1 = "foobar", V2 = NestedClass2(V1 = "barfoo"))
        serialize cls |> should equal "<nestedClass2><v1>foobar</v1><v2><v1>barfoo</v1></v2></nestedClass2>"

    [<Test>]
    let serializeNestedClass03() =
        let cls = NestedClass2(V1 = "foobar", V2 = NestedClass2(V1 = "barfoo", V2 = NestedClass2(V1 = "ham eggs")))
        serialize cls |> should equal "<nestedClass2><v1>foobar</v1><v2><v1>barfoo</v1><v2><v1>ham eggs</v1></v2></v2></nestedClass2>"

    [<Test>]
    let serializeNestedClass04() =
        let cls = NestedClass2(V1 = "foobar", V2 = NestedClass2(V1 = "barfoo", V2 = NestedClass2()))
        serialize cls |> should equal "<nestedClass2><v1>foobar</v1><v2><v1>barfoo</v1><v2></v2></v2></nestedClass2>"

    [<Test>]
    let serializeRecord01() =
        let record = { Value = 99; Name = "ham & eggs" }
        serialize record |> should equal "<testRecord><value>99</value><name>ham & eggs</name></testRecord>"

    [<Test>]
    let serializeTuple01() =
        let tuple = 406, "foo bar test"
        serialize tuple |> should equal "<tuple><item1>406</item1><item2>foo bar test</item2></tuple>"

    [<Test>]
    let serializeSpecialChars01() =
        let special = "foo\r\nbar"
        let cls = TestClass(305, special)
        serialize cls |> should equal "<testClass><v1>305</v1><v2>foo\r\nbar</v2></testClass>"

    [<Test>]
    let serializeSpecialChars02() =
        let special = "</v2>"
        let cls = TestClass(210, special)
        serialize cls |> should equal "<testClass><v1>210</v1><v2>&lt;/v2&gt;</v2></testClass>"

    [<Test>]
    let serializeNonPrintable01() =
        let chars = string [ for i in 10 .. 30 -> char i ]
        let cls = TestClass(999, chars)
        serialize cls |> should equal (sprintf "<testClass><v1>999</v1><v2>%s</v2></testClass>" chars)
