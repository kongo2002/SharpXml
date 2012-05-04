namespace SharpXml.Tests

module SerializationTests =

    open System
    open System.IO
    open NUnit.Framework

    open SharpXml
    open SharpXml.ValueTypeSerializer
    open SharpXml.Tests.Types

    [<Test>]
    let serializeDateTime01() =
        let writer = new StringWriter()
        let curr = DateTime.Now
        let date = curr.Date
        Serializer<DateTime>.WriteTag(writer, "date", date)
        Assert.AreEqual(sprintf "<date>%s</date>" (date.ToString("yyyy-MM-dd")), writer.ToString())

    [<Test>]
    let serializeFloat() =
        let writer = new StringWriter()
        let value = 2.528
        Serializer.WriteTag(writer, "float", value)
        Assert.AreEqual(sprintf "<float>%.3f</float>" value, writer.ToString())

    [<Test>]
    let serializeClass01() =
        let writer = new StringWriter()
        let cls = TestClass(800, "foo bar")
        let func = Serializer<TestClass>.GetWriterFunc()
        func writer cls
        Assert.AreEqual("<v1>800</v1><v2>foo bar</v2>", writer.ToString())