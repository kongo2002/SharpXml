module SerializationTests

open System
open System.IO
open NUnit.Framework

open SharpXml.Serializer
open SharpXml.ValueTypeSerializer

[<Test>]
let serializeDateTime01() =
    let writer = new StringWriter()
    let curr = DateTime.Now
    let date = curr.Date
    writeTag writer "date" date writeDateTime
    Assert.AreEqual(sprintf "<date>%s</date>" (date.ToString("yyyy-MM-dd")), writer.ToString())

[<Test>]
let serializeFloat() =
    let writer = new StringWriter()
    let value = 2.52
    writeTag writer "float" value writeFloat2f
    Assert.AreEqual(sprintf "<float>%.2f</float>" value, writer.ToString())