//  Copyright 2012 Gregor Uhlenheuer
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.

namespace SharpXml

open System
open System.Globalization
open System.IO
open System.Text

open SharpXml.Utils

/// XML serializer
type XmlSerializer() =

    /// UTF-8 encoding without BOM
    static let utf8encoding = UTF8Encoding(false)

    /// Header string for XML output
    static let xmlHeader = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

    /// Deserialize the input string into the specified type
    static member DeserializeFromString<'T> input =
        if notEmpty input then
            try
                let reader = Deserializer.getReaderFunc typeof<'T>
                let info = XmlParser.ParserInfo input
                XmlParser.eatRoot info
                reader info :?> 'T
            with
            | :? SharpXmlException -> Unchecked.defaultof<'T>
        else
            Unchecked.defaultof<'T>

    /// Deserialize the input string into the specified type
    static member DeserializeFromString(input, targetType) =
        if notEmpty input then
            try
                let reader = Deserializer.getReaderFunc targetType
                let info = XmlParser.ParserInfo input
                XmlParser.eatRoot info
                reader info
            with
            | :? SharpXmlException -> null
        else
            null

    /// Deserialize the input reader into the specified type
    static member DeserializeFromReader<'T> (reader : TextReader) =
        XmlSerializer.DeserializeFromString<'T>(reader.ReadToEnd())

    /// Deserialize the input stream into the specified type
    static member DeserializeFromStream<'T> (stream : Stream) =
        use reader = new StreamReader(stream, utf8encoding)
        XmlSerializer.DeserializeFromString<'T>(reader.ReadToEnd())

    /// Serialize the given object into a XML string
    static member SerializeToString<'T> (element : 'T) =
        let sb = StringBuilder()
        use writer = new StringWriter(sb, CultureInfo.InvariantCulture)
        if XmlConfig.Instance.WriteXmlHeader then writer.Write(xmlHeader)
        Serializer.writeType writer element
        sb.ToString()

    /// Serialize the given object into XML output using the specified TextWriter
    static member SerializeToWriter<'T> (writer : TextWriter, element : 'T) =
        if XmlConfig.Instance.WriteXmlHeader then writer.Write(xmlHeader)
        Serializer.writeType writer element
