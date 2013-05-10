//  Copyright 2012-2013 Gregor Uhlenheuer
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

#nowarn "9"
#nowarn "51"

namespace SharpXml

/// Record type containing the type specific information
/// for the first element to serialize
type internal TypeInfo = {
    Type : System.Type
    OriginalName : string
    ClsName : string
    Namespace : string option }

type internal NameInfo = {
    Name : string
    Item : string
    Key : string
    Value : string }

/// Writer function delegate
type internal WriterFunc = NameInfo -> System.IO.TextWriter -> obj -> unit

/// Record type containing the serialization information
/// for a specific property member
type internal PropertyWriterInfo = {
    Info : System.Reflection.PropertyInfo
    OriginalName : string
    Name : NameInfo
    GetFunc : GetterFunc
    WriteFunc : Lazy<WriterFunc>
    Default : obj }

module internal SerializerBase =

    open System
    open System.IO

    /// General purpose XML tags writer function
    let writeTag (name : string) (info : NameInfo) (w : TextWriter) writeFunc (value : obj) =
        w.Write('<'); w.Write(name); w.Write('>')
        writeFunc info w value
        w.Write("</"); w.Write(name); w.Write('>')

    /// XML tag writer function with an additional namespace attribute
    let writeTagNamespace (name : string) (ns : string) (info : NameInfo) (w : TextWriter) writeFunc (value : obj) =
        w.Write('<'); w.Write(name); w.Write(" xmlns=\""); w.Write(ns); w.Write("\">")
        writeFunc info w value
        w.Write("</"); w.Write(name); w.Write('>')

    /// Empty tag writer function
    let writeEmptyTag (name : string) (w : TextWriter) =
        w.Write('<')
        w.Write(name);
        w.Write("></");
        w.Write(name);
        w.Write('>');

/// Module containing the serialization logic
/// for value types
module internal ValueTypeSerializer =

    open System
    open System.Globalization
    open System.IO

    open Microsoft.FSharp.NativeInterop

    open SharpXml.Extensions

    let shortDateTimeFormat = "yyyy-MM-dd"
    let defaultFormat = "dd/MM/yyyy HH:mm:ss"
    let defaultFormatWithFraction = "dd/MM/yyyy HH:mm:ss.fff"
    let xsdFormat = "yyyy-MM-ddTHH:mm:ss.fffffffZ"
    let xsdFormat3F = "yyyy-MM-ddTHH:mm:ss.fffZ"
    let xsdFormatSeconds = "yyyy-MM-ddTHH:mm:ssZ"

    /// Convert the given DateTime into the shortest possible XSD format
    let toShortestXsdFormat (date : DateTime) =
        let day = date.TimeOfDay
        if day.Ticks = 0L then date.ToString(shortDateTimeFormat)
        elif day.Milliseconds = 0 then date.ToUniversal().ToString(xsdFormatSeconds)
        else date.ToString() // TODO

    let inline writeString (writer : TextWriter) (content : string) =
        let len = content.Length
        if len > 0 then
            let chars = content.ToCharArray()
            let mutable curr = 0
            let mutable char = NativePtr.read &&chars.[0]
            while curr < len do
                match char with
                | '<' -> writer.Write("&lt;")
                | '>' -> writer.Write("&gt;")
                | _ -> writer.Write(char)
                curr <- curr + 1
                if curr < len then char <- NativePtr.read &&chars.[curr]

    let inline nullableWriter n writer value func =
        if value <> null then func n writer value

    let writeStringObject _ writer (value : obj) =
        let v : string = unbox value
        writeString writer v

    let writeObject _ (writer : TextWriter) (value : obj) =
        writer.Write(value)

    let writeDateTime _ (writer : TextWriter) (value : obj) =
        let v = toShortestXsdFormat (unbox value)
        writer.Write(v)

    let writeNullableDateTime n writer (value : obj) =
        nullableWriter n writer value writeDateTime

    let writeDateTimeOffset _ (writer : TextWriter) (value : obj) =
        let v : DateTimeOffset = unbox value
        writer.Write(v.ToString("o"))

    let writeNullableDateTimeOffset n writer (value : obj) =
        nullableWriter n writer value writeDateTimeOffset

    let writeGuid _ (writer : TextWriter) (value : obj) =
        let v : Guid = unbox value
        writer.Write(v.ToString("N"))

    let writeNullableGuid n writer (value : obj) =
        nullableWriter n writer value writeGuid

    let writeChar _ (writer : TextWriter) (value : obj) =
        let v : char = unbox value
        writer.Write(v)

    let writeChars _ (writer : TextWriter) (value : obj) =
        let v : char[] = unbox value
        writer.Write(v)

    let writeByte _ (writer : TextWriter) (value : obj) =
        let v : byte = unbox value
        writer.Write(v)

    let writeSByte _ (writer : TextWriter) (value : obj) =
        let v : sbyte = unbox value
        writer.Write(v)

    let writeBytes _ (writer : TextWriter) (value : obj) =
        let v = Convert.ToBase64String(unbox value)
        writer.Write(v)

    let writeUInt16 _ (writer : TextWriter) (value : obj) =
        let v : uint16 = unbox value
        writer.Write(v)

    let writeInt16 _ (writer : TextWriter) (value : obj) =
        let v : int16 = unbox value
        writer.Write(v)

    let writeInt32 _ (writer : TextWriter) (value : obj) =
        let v : int = unbox value
        writer.Write(v)

    let writeUInt32 _ (writer : TextWriter) (value : obj) =
        let v : uint32 = unbox value
        writer.Write(v)

    let writeInt64 _ (writer : TextWriter) (value : obj) =
        let v : int64 = unbox value
        writer.Write(v)

    let writeUInt64 _ (writer : TextWriter) (value : obj) =
        let v : uint64 = unbox value
        writer.Write(v)

    let writeFloat _ (writer : TextWriter) (value : obj) =
        let v : float = unbox value
        writer.Write(v.ToString(CultureInfo.InvariantCulture))

    let writeFloat32 _ (writer : TextWriter) (value : obj) =
        let v : float32 = unbox value
        writer.Write(v.ToString(CultureInfo.InvariantCulture))

    let writeBool _ (writer : TextWriter) (value : obj) =
        let v : bool = unbox value
        match v with
        | true -> writer.Write("true")
        | false -> writer.Write("false")

    let writeDecimal _ (writer : TextWriter) (value : obj) =
        let v : decimal = unbox value
        writer.Write(v.ToString(CultureInfo.InvariantCulture))

    let writeEnum n writer (value : obj) =
        writeObject n writer value

    let writeEnumNames _ (writer : TextWriter) (value : obj) =
        let v : int = unbox value
        writer.Write(v)

    let writeType _ writer (value : obj) =
        let v : Type = unbox value
        writeString writer v.AssemblyQualifiedName

    let writeException _ writer (value : obj) =
        let v : Exception = unbox value
        writeString writer v.Message

    /// Get the appropriate writer function for the
    /// specified value type
    let getValueTypeWriter (t : Type) =
        if t = typeof<Nullable<DateTime>> then
            Some writeNullableDateTime
        elif t = typeof<Guid> then
            Some writeGuid
        elif t = typeof<Nullable<Guid>> then
            Some writeNullableGuid
        elif t = typeof<DateTimeOffset> then
            Some writeDateTimeOffset
        elif t = typeof<Nullable<DateTimeOffset>> then
            Some writeNullableDateTimeOffset
        elif t.IsEnum || t.UnderlyingSystemType.IsEnum then
            if t.HasAttribute("FlagsAttribute") then Some writeEnumNames else Some writeEnum
        else
            match Type.GetTypeCode(t.NullableUnderlying()) with
            | TypeCode.Boolean -> Some writeBool
            | TypeCode.Byte -> Some writeByte
            | TypeCode.Char -> Some writeChar
            | TypeCode.DateTime -> Some writeDateTime
            | TypeCode.Decimal -> Some writeDecimal
            | TypeCode.Double -> Some writeFloat
            | TypeCode.Int16 -> Some writeInt16
            | TypeCode.Int32 -> Some writeInt32
            | TypeCode.Int64 -> Some writeInt64
            | TypeCode.SByte -> Some writeSByte
            | TypeCode.Single -> Some writeFloat32
            | TypeCode.UInt16 -> Some writeUInt16
            | TypeCode.UInt32 -> Some writeUInt32
            | TypeCode.UInt64 -> Some writeUInt64
            | _ -> None

/// Serialization logic for list and collection types
module internal ListSerializer =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.IO
    open System.Reflection

    open SerializerBase

    /// Writer function for integer arrays
    let writeIntArray name (writer : TextWriter) (value : obj) =
        let array : int [] = unbox value
        array
        |> Array.iter (fun elem -> writeTag name.Item name writer ValueTypeSerializer.writeInt32 elem)

    /// Writer function for string arrays
    let writeStrArray name (writer : TextWriter) (value : obj) =
        let array : string [] = unbox value
        array
        |> Array.iter (fun elem -> writeTag name.Item name writer ValueTypeSerializer.writeStringObject elem)

    /// Writer function for untyped IEnumerables
    let writeEnumerable determineFunc name (writer : TextWriter) (value : obj) =
        let collection : IEnumerable = unbox value
        collection
        |> Seq.cast
        |> Seq.iter (fun elem ->
            let writeFunc = determineFunc (elem.GetType())
            writeTag name.Item name writer writeFunc elem)

    /// Writer function for generic IEnumerables
    let writeGenericEnumerable<'a> (writeFunc : WriterFunc) name (writer : TextWriter) (value : obj) =
        let collection : IEnumerable<'a> = unbox value
        collection
        |> Seq.iter (writeTag name.Item name writer writeFunc)

    /// Wrapper function to get the generic IEnumerable writer
    let getGenericEnumerableWriter elemWriter t =
        // TODO: this does not look sane at all
        let flags = BindingFlags.NonPublic ||| BindingFlags.Static
        let writer = Type.GetType("SharpXml.ListSerializer").GetMethod("writeGenericEnumerable", flags)
        let mtd = writer.MakeGenericMethod([| t |])
        fun n (w : TextWriter) x -> mtd.Invoke(null, [| elemWriter; n; w; x |]) |> ignore

/// Serialization logic
module internal Serializer =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.IO
    open System.Reflection
    open System.Text.RegularExpressions

    open SharpXml.Attempt
    open SharpXml.Common
    open SharpXml.Extensions
    open SharpXml.TypeHelper
    open SharpXml.Utils

    open SerializerBase

    let propertyCache = ref (Dictionary<Type, PropertyWriterInfo[]>())
    let serializerCache = ref (Dictionary<Type, WriterFunc>())
    let typeInfoCache = ref (Dictionary<Type, TypeInfo>())

    let writerFuncName = "ToXml"
    let instanceFlags = BindingFlags.Public ||| BindingFlags.Instance
    let staticFlags = BindingFlags.Public ||| BindingFlags.Static

    /// Try to determine one of a special serialization
    /// function, i.e. Exception, Uri
    let getSpecialWriters (t : Type) = fun () ->
        if t = typeof<Uri> then Some ValueTypeSerializer.writeStringObject
        elif isOrDerived t typeof<Exception> then Some ValueTypeSerializer.writeException
        elif t.IsInstanceOfType(typeof<Type>) then Some ValueTypeSerializer.writeType
        else None

    let writeAbstractProperties _ _ _ = ()

    /// Determine the name of the TypeInfo based on the given type
    let getTypeName (t : Type) =
        let baseName = if t.IsArray then "Array" else removeGenericSuffix <| t.NullableUnderlying().Name
        if XmlConfig.Instance.EmitCamelCaseNames then
            baseName.ToCamelCase()
        else
            baseName

    /// Build a TypeInfo object based on the given Type
    let buildTypeInfo t =
        match getAttribute<XmlElementAttribute> t with
        | Some attr ->
            { Type = t
              OriginalName = t.Name
              ClsName = if notWhite attr.Name then attr.Name else getTypeName t
              Namespace =
                  match String.IsNullOrWhiteSpace(attr.Namespace) with
                  | false -> Some attr.Namespace
                  | true  -> None }
        | None ->
            { Type = t
              OriginalName = t.Name
              ClsName = getTypeName t
              Namespace = None }

    /// Get the TypeInfo object associated with the given Type
    let getTypeInfo (t : Type) =
        match (!typeInfoCache).TryGetValue t with
        | true, ti -> ti
        | _ -> Atom.updateAtomDict typeInfoCache t (buildTypeInfo t)

    /// Try to determine the string writer function
    let getStringWriter (t : Type) = fun () ->
        if t = typeof<string> then Some ValueTypeSerializer.writeStringObject else None

    /// Try to determine a matching value type writer function
    let getValueTypeWriter (t : Type) = fun () ->
        if t.IsValueType then ValueTypeSerializer.getValueTypeWriter t else None

    /// Try to determine a member function 'ToXml'
    let getInstanceWriter (t : Type) = fun() ->
        match t.GetMethod(writerFuncName, instanceFlags, null, Type.EmptyTypes, null) |> Utils.toOption with
        | Some func ->
            (fun _ (w : TextWriter) x -> w.Write(func.Invoke(x, null))) |> Some
        | _ -> None

    /// Try to determine a static function 'ToXml'
    let getStaticWriter (t : Type) = fun () ->
        match t.GetMethod(writerFuncName, staticFlags, null, [| t |], null) |> Utils.toOption with
        | Some func ->
            (fun _ (w : TextWriter) x -> w.Write(func.Invoke(null, [| x |]))) |> Some
        | _ -> None

    /// Try to get a custom serializer function
    let getCustomWriter (t : Type) = fun () ->
        match XmlConfig.Instance.TryGetSerializer t with
        | Some func ->
            Some (fun _ (w : TextWriter) x -> w.Write(func.Invoke(x)))
        | _ -> None

    /// Return a default NameInfo instance for the specified name
    let getDefaultNameInfo name = {
        Name = name
        Item = "item"
        Key = "key"
        Value = "value" }

    /// Build a NameInfo object for the given PropertyInfo
    let getNameInfo (property : PropertyInfo) =
        let attribute = getAttribute<XmlElementAttribute> property
        let get str fb = if notWhite str then str else fb
        let name = if XmlConfig.Instance.EmitCamelCaseNames then property.Name.ToCamelCase() else property.Name
        let itemName =
            match property.PropertyType with
            | GenericTypeOf GenericTypes.iEnum elem ->
                getTypeName elem
            | _ -> "item"
        let keyName, valueName =
            // TODO: add check for other key-value collections
            match property.PropertyType with
            | GenericTypesOf GenericTypes.iDict (k, v) ->
                getTypeName k, getTypeName v
            | _ -> "key", "value"
        match attribute with
        | Some attr ->
            let item = get attr.ItemName itemName
            let key = get attr.KeyName keyName
            let value = get attr.ValueName valueName
            let name = get attr.Name name
            { Name = name; Item = item; Key = key; Value = value }
        | _ -> { Name = name; Item = itemName; Key = keyName; Value = valueName }

    /// Build a PropertyWriterInfo object based on the
    /// specified PropertyInfo
    let rec buildPropertyWriterInfo (propInfo : PropertyInfo) =
        { Info = propInfo
          OriginalName = propInfo.Name
          Name = getNameInfo propInfo
          GetFunc = ReflectionHelpers.getObjGetter propInfo
          WriteFunc = lazy getWriterFunc propInfo.PropertyType
          Default = ReflectionHelpers.getDefaultValue propInfo.PropertyType }

    /// Try to determine a enumerable serialization function
    and getEnumerableWriter (t : Type) = fun () ->
        match t with
        | GenericTypeOf GenericTypes.iEnum elemType ->
            let elemWriter = getWriterFunc elemType
            Some <| ListSerializer.getGenericEnumerableWriter elemWriter elemType
        | _ when matchInterface typeof<IEnumerable> t ->
            Some <| ListSerializer.writeEnumerable getWriterFunc
        | _ -> None

    /// Get the PropertyWriterInfo array for the given type
    and getProperties (t : Type) =
        match (!propertyCache).TryGetValue t with
        | true, props -> props
        | _ ->
            let props =
                ReflectionHelpers.getSerializableProperties t
                |> Seq.filter (fun p -> p.GetIndexParameters().Length = 0)
                |> Seq.map buildPropertyWriterInfo
                |> Array.ofSeq
            Atom.updateAtomDict propertyCache t props

    /// Writer for classes and other reference types
    and writeClass props _ (writer : TextWriter) (value : obj) =
        props
        |> Array.iter (fun p ->
            let v = p.GetFunc.Invoke(value)
            if v <> null then
                let writeFunc = p.WriteFunc.Value
                writeTag p.Name.Name p.Name writer writeFunc v
            elif XmlConfig.Instance.IncludeNullValues then
                writeEmptyTag p.Name.Name writer)

    /// Try to determine a class or interface serialization function
    and getClassWriter (t : Type) = fun () ->
        if t.IsClass || t.IsInterface then
            if t.IsAbstract && not t.IsInterface then
                Some writeAbstractProperties
            else
                let properties = getProperties t
                Some (writeClass properties)
        else None

    /// Try to determine a writer function for a dictionary
    and getDictionaryWriter (t : Type) = fun () ->
        if matchInterface typeof<IDictionary> t then
            Some writeDictionary
        else None

    /// Try to determine a writer function for array types
    and getArrayWriter (t : Type) = fun () ->
        if t.IsArray then
            if t = typeof<byte[]> then Some ValueTypeSerializer.writeBytes
            elif t = typeof<char[]> then Some ValueTypeSerializer.writeChars
            elif t = typeof<int[]> then Some ListSerializer.writeIntArray
            elif t = typeof<string[]> then Some ListSerializer.writeStrArray
            // other array types will be handled by the IEnumerable writer
            else None
        else None

    /// Writer function for a dictionary
    and writeDictionary name (writer : TextWriter) (value : obj) =
        let map : IDictionary = unbox value
        let keyWriter : WriterFunc option ref = ref None
        let valueWriter : WriterFunc option ref = ref None
        map.Keys
        |> Seq.cast
        |> Seq.iter (fun key ->
            let dictVal = map.[key]
            if (!keyWriter).IsNone then keyWriter := Some <| getWriterFunc (key.GetType())
            if (!valueWriter).IsNone then valueWriter := Some <| getWriterFunc (dictVal.GetType())
            match !keyWriter, !valueWriter with
            | Some kv, Some vw ->
                writer.Write(sprintf "<%s>" name.Item)
                writeTag name.Key name writer kv key
                writeTag name.Value name writer vw dictVal
                writer.Write(sprintf "</%s>" name.Item)
            | _ -> ())

    and getObjectWriter = fun() ->
        Some ValueTypeSerializer.writeObject

    /// Determine the associated serialization writer
    /// function for the specified type
    and determineWriter (t : Type) =
        let writer = attempt {
            let! strWriter = getStringWriter t
            let! customWriter = getCustomWriter t
            let! valueWriter = getValueTypeWriter t
            let! specialWriter = getSpecialWriters t
            let! arrayWriter = getArrayWriter t
            let! dictWriter = getDictionaryWriter t
            let! enumerableWriter = getEnumerableWriter t
            let! instanceWriter = getInstanceWriter t
            let! staticWriter = getStaticWriter t
            let! classWriter = getClassWriter t
            let! objectWriter = getObjectWriter
            objectWriter }
        writer

    /// Get the writer function to serialize the
    /// specified type
    and getWriterFunc (t : Type) =
        match (!serializerCache).TryGetValue t with
        | true, serializer -> serializer
        | _ ->
            match determineWriter t with
            | Some s -> Atom.updateAtomDict serializerCache t s
            | _ ->
                let err = sprintf "could not determine serialization logic for type '%s'" t.FullName
                raise (SharpXmlException err)

    /// Write the given type using the appropriate serialization logic
    let writeType (writer : TextWriter) element targetType =
        let tInfo = getTypeInfo targetType
        let name = getDefaultNameInfo tInfo.ClsName
        let writerFunc = getWriterFunc targetType
        match tInfo.Namespace with
        | Some ns ->
            writeTagNamespace name.Name ns name writer writerFunc element
        | None ->
            writeTag name.Name name writer writerFunc element

    /// Clear the serializer cache
    let clearCache() =
        Atom.clearAtomDict serializerCache
