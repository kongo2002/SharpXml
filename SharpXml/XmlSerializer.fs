namespace SharpXml

open System
open System.Globalization
open System.IO
open System.Text

/// XML serializer
type XmlSerializer() =

    let empty = String.IsNullOrWhiteSpace

    let deserializeFromString (targetType : Type) (input : string) =
        if empty input then null else
            match TypeParser.getParser targetType with
            | Some parser -> parser.Invoke input
            | _ -> null

    let serializeToString t element =
        let sb = StringBuilder()
        use writer = new StringWriter(sb, CultureInfo.InvariantCulture)
        match Type.GetTypeCode(t) with
        | TypeCode.String ->
            Serializer.writeStringObject writer "value" element
        | _ ->
            let serializer = Serializer.getWriterFunc t
            serializer writer "value" element

    member x.DeserializeFromString<'T> input =
        deserializeFromString typeof<'T> input :?> 'T

    member x.SerializeToString<'T> element =
        serializeToString typeof<'T> element

