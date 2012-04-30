module SerializationTests

open System
open System.IO
open NUnit.Framework

open SharpXml.Serializer


[<Test>]
let serializeDateTime01() =
    let writer = new StringWriter()
    let curr = DateTime.Now
    let date = curr.Date
    writeDateTime writer "date" date
    Assert.AreEqual(sprintf "<date>%s</date>" (date.ToString("yyyy-MM-dd")), writer.ToString())

[<Test>]
let serializeFloat() =
    let writer = new StringWriter()
    let value = 2.52
    writeFloat2f writer "float" value
    Assert.AreEqual(sprintf "<float>%.2f</float>" value, writer.ToString())