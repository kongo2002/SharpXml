namespace SharpXml

open System
open System.Globalization
open System.IO
open System.Text

/// XML serializer
type XmlSerializer() =

    let empty = String.IsNullOrWhiteSpace

    member x.DeserializeFromString<'T> input : 'T =
        if empty input then Unchecked.defaultof<'T> else
            match TypeParser.getParser typeof<'T> with
            | Some parser -> unbox parser.Invoke input
            | _ -> Unchecked.defaultof<'T>

    member x.SerializeToString<'T> (element : 'T) =
        let sb = StringBuilder()
        let t = typeof<'T>
        use writer = new StringWriter(sb, CultureInfo.InvariantCulture)
        match Type.GetTypeCode(t) with
        | TypeCode.String ->
            Serializer<'T>.WriteTag(writer, "value", element)
        | _ ->
            let serializer = Serializer<'T>.GetWriterFunc()
            serializer writer element

