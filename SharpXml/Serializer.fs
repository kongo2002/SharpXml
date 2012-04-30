namespace SharpXml

module DateTimeSerializer =

    open System
    open System.Xml
    open SharpXml.Extensions

    let shortDateTimeFormat = "yyyy-MM-dd"
    let defaultFormat = "dd/MM/yyyy HH:mm:ss"
    let defaultFormatWithFraction = "dd/MM/yyyy HH:mm:ss.fff"
    let xsdFormat = "yyyy-MM-ddTHH:mm:ss.fffffffZ"
    let xsdFormat3F = "yyyy-MM-ddTHH:mm:ss.fffZ"
    let xsdFormatSeconds = "yyyy-MM-ddTHH:mm:ssZ"

    let toXsdFormat (date : DateTime) =
        // TODO: replace this with own logic
        XmlConvert.ToString(date.ToUniversal(), XmlDateTimeSerializationMode.Utc)

    let toShortestXsdFormat (date : DateTime) =
        let day = date.TimeOfDay
        if day.Ticks = 0L then date.ToString(shortDateTimeFormat)
        elif day.Milliseconds = 0 then date.ToUniversal().ToString(xsdFormatSeconds)
        else toXsdFormat date

module Serializer =

    open System
    open System.Collections.Generic
    open System.Globalization
    open System.IO

    open SharpXml.Extensions

    type WriterFunc = TextWriter -> string -> obj -> unit

    let serializerCache = ref (Dictionary<Type, WriterFunc>())

    let writeTag (writer : TextWriter) (name : string) value writeFunc =
        writer.Write("<{0}>", name)
        writeFunc value
        writer.Write("</{0}>", name)

    let writeString (writer : TextWriter) (name : string) (content : string) =
        writer.Write("<{0}>{1}</{0}>", name, content)

    let nullableWriter writer name value func =
        if value <> null then func writer name value

    let writeStringObject writer name (value : obj) =
        let v : string = unbox value
        writeString writer name v

    let writeObject writer name (value : obj) =
        writeTag writer name value (writer.Write)

    let writeFloat2f writer name (value : obj) =
        let v = sprintf "%.2f" (unbox value)
        writeString writer name v

    let writeDateTime writer name (value : obj) =
        let v = DateTimeSerializer.toShortestXsdFormat (unbox value)
        writeString writer name v

    let writeNullableDateTime writer name (value : obj) =
        nullableWriter writer name value writeDateTime

    let writeDateTimeOffset writer name (value : obj) =
        let v : DateTimeOffset = unbox value
        writeString writer name (v.ToString("o"))

    let writeNullableDateTimeOffset writer name (value : obj) =
        nullableWriter writer name value writeDateTimeOffset

    let writeGuid writer name (value : obj) =
        let v : Guid = unbox value
        writeString writer name (v.ToString("N"))

    let writeNullableGuid writer name (value : obj) =
        nullableWriter writer name value writeGuid

    let writeChar writer name (value : obj) =
        let v : char = unbox value
        writeTag writer name v writer.Write

    let writeByte writer name (value : obj) =
        let v : byte = unbox value
        writeTag writer name v writer.Write

    let writeSByte writer name (value : obj) =
        let v : sbyte = unbox value
        writeTag writer name v writer.Write

    let writeBytes writer name (value : obj) =
        let v = Convert.ToBase64String(unbox value)
        writeString writer name v

    let writeUInt16 writer name (value : obj) =
        let v : uint16 = unbox value
        writeTag writer name v writer.Write

    let writeInt16 writer name (value : obj) =
        let v : int16 = unbox value
        writeTag writer name v writer.Write

    let writeInt32 writer name (value : obj) =
        let v : int = unbox value
        writeTag writer name v writer.Write

    let writeUInt32 writer name (value : obj) =
        let v : uint32 = unbox value
        writeTag writer name v writer.Write

    let writeInt64 writer name (value : obj) =
        let v : int64 = unbox value
        writeTag writer name v writer.Write

    let writeUInt64 writer name (value : obj) =
        let v : uint64 = unbox value
        writeTag writer name v writer.Write

    let writeFloat writer name (value : obj) =
        let v : float = unbox value
        writeTag writer name v writer.Write

    let writeFloat32 writer name (value : obj) =
        let v : float32 = unbox value
        writeTag writer name v writer.Write

    let writeBool writer name (value : obj) =
        let v : bool = unbox value
        // TODO: maybe these should be written in lowercase instead
        writeTag writer name v writer.Write

    let writeDecimal writer name (value : obj) =
        let v : decimal = unbox value
        writeString writer name (v.ToString(CultureInfo.InvariantCulture))

    let writeEnum writer name (value : obj) =
        writeObject writer name value

    let writeEnumNames writer name (value : obj) =
        let v : int = unbox value
        writeTag writer name v (writer.Write)

    let getValueTypeWriter (t : Type) =
        match Type.GetTypeCode(t.NullableUnderlying()) with
        | TypeCode.Boolean -> writeBool
        | TypeCode.Byte -> writeByte
        | TypeCode.Char -> writeChar
        | TypeCode.DateTime -> writeDateTime
        | TypeCode.Decimal -> writeDecimal
        | TypeCode.Double -> writeFloat32
        | TypeCode.Int16 -> writeInt16
        | TypeCode.Int32 -> writeInt32
        | TypeCode.Int64 -> writeInt64
        | TypeCode.SByte -> writeSByte
        | TypeCode.Single -> writeFloat
        | TypeCode.UInt16 -> writeUInt16
        | TypeCode.UInt32 -> writeUInt32
        | TypeCode.UInt64 -> writeUInt64
        | _ -> writeObject

    let determineWriter (t : Type) =
        if t = typeof<string> then
            writeStringObject
        elif t.IsValueType then
            getValueTypeWriter t
        else
            writeObject

    let getWriterFunc (t : Type) =
        match (!serializerCache).TryGetValue t with
        | true, serializer -> serializer
        | _ ->
            let serializer = determineWriter t
            Atom.updateAtomDict serializerCache t serializer