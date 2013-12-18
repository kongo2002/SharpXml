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
    Namespace : string option
    Attributes : (string * string) array }

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
    Default : obj
    Order : int }

/// Record type containing the serialization information
/// for a specific XML attribute registered on a type
type internal AttributeWriterInfo = {
    Key : string
    GetFunc : GetterFunc
    ToStr : SerializerFunc }

/// Record containing all property and attribute writer
/// information used for serialization of a specific type
type internal TypeWriterInfo = {
    Properties : PropertyWriterInfo list
    Attributes : AttributeWriterInfo list }

/// Internal module of convenience serialization functions
module internal SerializerBase =

    open System
    open System.IO

    open Extensions
    open Utils

    /// General purpose XML tags writer function
    let writeTag (name : string) (info : NameInfo) (w : TextWriter) writeFunc (value : obj) =
        w.Write('<'); w.Write(name); w.Write('>')
        writeFunc info w value
        w.Write("</"); w.Write(name); w.Write('>')

    /// XML tag writer function with an additional namespace attribute
    let writeTagNamespace (ns : string) (name : string) (info : NameInfo) (w : TextWriter) writeFunc (value : obj) =
        w.Write('<'); w.Write(name); w.Write(" xmlns=\""); w.Write(ns); w.Write("\">")
        writeFunc info w value
        w.Write("</"); w.Write(name); w.Write('>')

    /// XML tag writer function with additional attributes
    let writeTagAttributes (name : string) (attr : (string * string) list) (info : NameInfo) (w : TextWriter) writeFunc (value : obj) =
        if List.isEmpty attr then writeTag name info w writeFunc value
        else
            w.Write('<'); w.Write(name);
            attr |> List.iter (fun (k, v) -> w.Write(' '); w.Write(k); w.Write("=\""); w.Write(v); w.Write("\""))
            w.Write('>');
            writeFunc info w value
            w.Write("</"); w.Write(name); w.Write('>')

    /// Empty tag writer function
    let writeEmptyTag (name : string) (w : TextWriter) =
        w.Write('<')
        w.Write(name);
        w.Write("></");
        w.Write(name);
        w.Write('>');

    let injectWriteTagAttributes (func: WriterFunc) attr =
        fun (n : NameInfo) w x -> writeTagAttributes n.Name attr n w func x

    let injectWriteTag (func : WriterFunc) = 
        fun (n : NameInfo) w x -> writeTag n.Name n w func x

    let extractAttributeValues (info: TypeWriterInfo) (value : obj) =
        info.Attributes
        |> List.choose (fun a ->
            let v = a.GetFunc.Invoke value
            if v <> null then Some (a.Key, a.ToStr.Invoke(v)) else None)

/// Module containing the serialization logic
/// for value types
module internal ValueTypeSerializer =

    open System
    open System.Globalization
    open System.IO

    open Microsoft.FSharp.NativeInterop

    open Extensions
    open Utils

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

    let writeType _ writer (value : obj) =
        let v : Type = unbox value
        writeString writer v.AssemblyQualifiedName

    let writeException _ writer (value : obj) =
        let v : Exception = unbox value
        writeString writer v.Message

    let getEnumValueWriter t =
        let under = Enum.GetUnderlyingType(t)
        match Type.GetTypeCode(under) with
        | TypeCode.Boolean -> Some <| writeBool
        | TypeCode.Byte    -> Some <| writeByte
        | TypeCode.Char    -> Some <| writeChar
        | TypeCode.Decimal -> Some <| writeDecimal
        | TypeCode.Double  -> Some <| writeFloat
        | TypeCode.Int16   -> Some <| writeInt16
        | TypeCode.Int32   -> Some <| writeInt32
        | TypeCode.Int64   -> Some <| writeInt64
        | TypeCode.SByte   -> Some <| writeSByte
        | TypeCode.Single  -> Some <| writeFloat32
        | TypeCode.UInt16  -> Some <| writeUInt16
        | TypeCode.UInt32  -> Some <| writeUInt32
        | TypeCode.UInt64  -> Some <| writeUInt64
        | _ -> None

    /// Get the appropriate writer function for the
    /// specified value type
    let getValueTypeInnerWriter (t : Type) =
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
            if t.HasAttribute("FlagsAttribute")
            then getEnumValueWriter t
            else Some writeEnum
        else
            match Type.GetTypeCode(t.NullableUnderlying()) with
            | TypeCode.Boolean  -> Some writeBool
            | TypeCode.Byte     -> Some writeByte
            | TypeCode.Char     -> Some writeChar
            | TypeCode.DateTime -> Some writeDateTime
            | TypeCode.Decimal  -> Some writeDecimal
            | TypeCode.Double   -> Some writeFloat
            | TypeCode.Int16    -> Some writeInt16
            | TypeCode.Int32    -> Some writeInt32
            | TypeCode.Int64    -> Some writeInt64
            | TypeCode.SByte    -> Some writeSByte
            | TypeCode.Single   -> Some writeFloat32
            | TypeCode.UInt16   -> Some writeUInt16
            | TypeCode.UInt32   -> Some writeUInt32
            | TypeCode.UInt64   -> Some writeUInt64
            | _                 -> None

    let getValueTypeWriter (t : Type) =
        match getValueTypeInnerWriter t with
        | Some writer ->
            let write (n : NameInfo) w v = SerializerBase.writeTag n.Name n w writer v
            Some write
        | None -> None

/// Various helper functions for XML attribute
/// serialization
module internal AttributeSerializer =

    open System
    
    open Extensions
    open Utils

    let nullableStrFunc<'a when 'a: (new: unit -> 'a)
                            and 'a :> ValueType
                            and 'a: struct> (func : 'a -> string) =
        SerializerFunc(fun (v : obj) ->
            let value : Nullable<'a> = unbox v
            if value.HasValue then func value.Value else null)

    let writeGuid (v : Guid) =
        v.ToString("N")

    let writeDateTimeOffset (v : DateTimeOffset) =
        v.ToString("o")

    let writeByte    (v : byte)    = v.ToString()
    let writeChar    (v : char)    = v.ToString()
    let writeDecimal (v : decimal) = v.ToString()
    let writeFloat   (v : float)   = v.ToString()
    let writeInt16   (v : int16)   = v.ToString()
    let writeInt32   (v : int)     = v.ToString()
    let writeInt64   (v : int)     = v.ToString()
    let writeSByte   (v : sbyte)   = v.ToString()
    let writeFloat32 (v : single)  = v.ToString()
    let writeUInt16  (v : uint16)  = v.ToString()
    let writeUInt32  (v : uint16)  = v.ToString()
    let writeUInt64  (v : uint64)  = v.ToString()

    let writeBool v =
        match v with
        | true  -> "true"
        | false -> "false"

    let attributeValueWriter (t: Type) =
        match Type.GetTypeCode(t) with
        | TypeCode.Boolean -> Some <| SerializerFunc(unbox >> writeBool)
        | TypeCode.Byte    -> Some <| SerializerFunc(unbox >> writeByte)
        | TypeCode.Char    -> Some <| SerializerFunc(unbox >> writeChar)
        | TypeCode.Decimal -> Some <| SerializerFunc(unbox >> writeDecimal)
        | TypeCode.Double  -> Some <| SerializerFunc(unbox >> writeFloat)
        | TypeCode.Int16   -> Some <| SerializerFunc(unbox >> writeInt16)
        | TypeCode.Int32   -> Some <| SerializerFunc(unbox >> writeInt32)
        | TypeCode.Int64   -> Some <| SerializerFunc(unbox >> writeInt64)
        | TypeCode.SByte   -> Some <| SerializerFunc(unbox >> writeSByte)
        | TypeCode.Single  -> Some <| SerializerFunc(unbox >> writeFloat32)
        | TypeCode.UInt16  -> Some <| SerializerFunc(unbox >> writeUInt16)
        | TypeCode.UInt32  -> Some <| SerializerFunc(unbox >> writeUInt32)
        | TypeCode.UInt64  -> Some <| SerializerFunc(unbox >> writeUInt64)
        | TypeCode.DateTime -> Some <| SerializerFunc(unbox >> toShortestXsdFormat)
        | _                -> None

    let writeEnumNames (t: Type) =
        let under = Enum.GetUnderlyingType(t)
        attributeValueWriter under

    let writeEnum (v : obj) =
        v.ToString()

    let getValueWriter (t: Type) : SerializerFunc option =
        if t = typeof<Nullable<DateTime>> then
            Some <| nullableStrFunc toShortestXsdFormat 
        elif t = typeof<Guid> then
            Some <| SerializerFunc(unbox >> writeGuid)
        elif t = typeof<Nullable<Guid>> then
            Some <| nullableStrFunc writeGuid
        elif t = typeof<DateTimeOffset> then
            Some <| SerializerFunc(unbox >> writeDateTimeOffset)
        elif t = typeof<Nullable<DateTimeOffset>> then
            Some <| nullableStrFunc writeDateTimeOffset
        elif t.IsEnum || t.UnderlyingSystemType.IsEnum then
            if t.HasAttribute("FlagsAttribute")
            then writeEnumNames t
            else None
        else
            attributeValueWriter (t.NullableUnderlying())

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
    let writeEnumerable determineFunc (name : NameInfo) (writer : TextWriter) (value : obj) =
        let collection : IEnumerable = unbox value
        let nameInfo = { name with Name = name.Item }
        collection
        |> Seq.cast
        |> Seq.iter (fun elem ->
            let writeFunc = determineFunc (elem.GetType())
            let n : NameInfo = { name with Name = name.Item }
            writeFunc n writer elem)

    /// Writer function for generic IEnumerables
    let writeGenericEnumerable<'a> (writeFunc : WriterFunc) (name : NameInfo) (writer : TextWriter) (value : obj) =
        let collection : IEnumerable<'a> = unbox value
        let nameInfo = { name with Name = name.Item }
        let inner _ _ _ =
            collection
            |> Seq.iter (writeFunc nameInfo writer)
        writeTag name.Name name writer inner value

    /// Writer function for generic IEnumerables with attribute values
    let writeGenericEnumerableAttributes<'a> (writeFunc : WriterFunc) (ti : TypeWriterInfo) (name : NameInfo) (writer : TextWriter) (value : obj) =
        let collection : IEnumerable<'a> = unbox value
        let nameInfo = { name with Name = name.Item }
        let attr = extractAttributeValues ti value
        let inner _ _ _ =
            collection
            |> Seq.iter (writeFunc nameInfo writer)
        writeTagAttributes name.Name attr name writer inner value

    /// Wrapper function to get the generic IEnumerable writer
    let getGenericEnumerableWriter elemWriter t =
        // TODO: this does not look sane at all
        let flags = BindingFlags.NonPublic ||| BindingFlags.Static
        let writer = Type.GetType("SharpXml.ListSerializer").GetMethod("writeGenericEnumerable", flags)
        let mtd = writer.MakeGenericMethod([| t |])
        fun n (w : TextWriter) x ->
            // TODO: I guess this kind of invokation is slow...
            mtd.Invoke(null, [| elemWriter; n; w; x |]) |> ignore

    let getGenericEnumerableAttributeWriter elemWriter t (ti : TypeWriterInfo) =
        // TODO: this does not look sane at all
        let flags = BindingFlags.NonPublic ||| BindingFlags.Static
        let writer = Type.GetType("SharpXml.ListSerializer").GetMethod("writeGenericEnumerableAttributes", flags)
        let mtd = writer.MakeGenericMethod([| t |])
        fun n (w : TextWriter) x ->
            // TODO: I guess this kind of invokation is slow...
            mtd.Invoke(null, [| elemWriter; ti; n; w; x |]) |> ignore

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

    let propertyCache = ref (Dictionary<Type, TypeWriterInfo>())
    let serializerCache = ref (Dictionary<Type, WriterFunc>())
    let typeInfoCache = ref (Dictionary<Type, TypeInfo>())

    let writerFuncName = "ToXml"
    let instanceFlags = BindingFlags.Public ||| BindingFlags.Instance
    let staticFlags = BindingFlags.Public ||| BindingFlags.Static
    let namespaceRegex = Regex(@"^\s*""([^""]+)""", RegexOptions.Compiled)

    /// Try to determine one of a special serialization
    /// function, i.e. Exception, Uri
    let getSpecialWriters (t : Type) = fun () ->
        if t = typeof<Uri> then Some <| injectWriteTag ValueTypeSerializer.writeStringObject
        elif isOrDerived t typeof<Exception> then Some <| injectWriteTag ValueTypeSerializer.writeException
        elif t <> typeof<obj> && t.IsInstanceOfType(typeof<Type>) then Some <| injectWriteTag ValueTypeSerializer.writeType
        else None

    let writeAbstractProperties _ _ _ = ()

    /// Determine the name of the TypeInfo based on the given type
    let getTypeName (t : Type) =
        let baseName = if t.IsArray then "Array" else removeGenericSuffix <| t.NullableUnderlying().Name
        if XmlConfig.Instance.EmitCamelCaseNames then
            baseName.ToCamelCase()
        else
            baseName

    /// Parse all static namespace values that are specified
    /// via XmlNamespaceAttribute elements
    let getNamespaceAttributes t =
        // keep track of duplicates
        let set = HashSet<string>()
        getAttributes<XmlNamespaceAttribute> t
        |> Array.map (fun a -> a.Attributes)
        |> Array.concat
        |> Array.choose (fun a ->
            match a.Split('=') with
            | [|k; v|] when notWhite k ->
                let m = namespaceRegex.Match v
                if m.Success && m.Groups.Count > 1 then
                    let key = k.Trim()
                    let unique = set.Add key
                    if unique then Some (key, m.Groups.[1].Value) else None
                else None
            | _ -> None)

    /// Build a TypeInfo object based on the given Type
    let buildTypeInfo t =
        match getAttribute<XmlElementAttribute> t with
        | Some attr ->
            { Type = t
              OriginalName = t.Name
              ClsName = if notWhite attr.Name then attr.Name else getTypeName t
              Namespace = if notWhite attr.Namespace then Some attr.Namespace else None
              Attributes = getNamespaceAttributes t }
        | None ->
            { Type = t
              OriginalName = t.Name
              ClsName = getTypeName t
              Namespace = None
              Attributes = getNamespaceAttributes t }

    /// Get the TypeInfo object associated with the given Type
    let getTypeInfo (t : Type) =
        match (!typeInfoCache).TryGetValue t with
        | true, ti -> ti
        | _ -> Atom.updateAtomDict typeInfoCache t (buildTypeInfo t)

    /// Try to determine the string writer function
    let getStringWriter (t : Type) = fun () ->
        if t = typeof<string> then Some <| injectWriteTag ValueTypeSerializer.writeStringObject else None

    /// Try to determine a matching value type writer function
    let getValueTypeWriter (t : Type) = fun () ->
        if t.IsValueType then ValueTypeSerializer.getValueTypeWriter t else None

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
            { Name = name; Item = item; Key = key; Value = value }, attr.Order
        | _ -> { Name = name; Item = itemName; Key = keyName; Value = valueName }, Int32.MaxValue

    /// Build a PropertyWriterInfo object based on the
    /// specified PropertyInfo
    let rec buildPropertyWriterInfo (propInfo : PropertyInfo) =
        let nameInfo, order = getNameInfo propInfo
        { Info = propInfo
          OriginalName = propInfo.Name
          Name = nameInfo
          GetFunc = ReflectionHelpers.getObjGetter propInfo
          WriteFunc = lazy getWriterFunc propInfo.PropertyType
          Default = ReflectionHelpers.getDefaultValue propInfo.PropertyType
          Order = order }

    and getInjectWriter (t : Type) writer =
        let writerInfo = getTypeWriterInfo t
        let writeAttrInject ni w x =
            let attr = extractAttributeValues writerInfo x
            injectWriteTagAttributes writer attr ni w x
        writeAttrInject

    /// Try to determine a member function 'ToXml'
    and getInstanceWriter (t : Type) = fun() ->
        match t.GetMethod(writerFuncName, instanceFlags, null, Type.EmptyTypes, null) |> Utils.toOption with
        | Some func ->
            let writer = (fun _ (w : TextWriter) x -> w.Write(func.Invoke(x, null)))
            if not XmlConfig.Instance.UseAttributes then
                injectWriteTag writer
            else getInjectWriter t writer
            |> Some
        | _ -> None

    /// Try to determine a static function 'ToXml'
    and getStaticWriter (t : Type) = fun () ->
        match t.GetMethod(writerFuncName, staticFlags, null, [| t |], null) |> Utils.toOption with
        | Some func ->
            let writer = (fun _ (w : TextWriter) x -> w.Write(func.Invoke(null, [| x |])))
            if not XmlConfig.Instance.UseAttributes then
                injectWriteTag writer
            else getInjectWriter t writer
            |> Some
        | _ -> None

    /// Try to get a custom serializer function
    and getCustomWriter (t : Type) = fun () ->
        match XmlConfig.Instance.TryGetSerializer t with
        | Some func ->
            let writer = (fun _ (w : TextWriter) x -> w.Write(func.Invoke(x)))
            if not XmlConfig.Instance.UseAttributes then
                injectWriteTag writer
            else getInjectWriter t writer
            |> Some
        | _ -> None

    /// Try to determine a enumerable serialization function
    and getEnumerableWriter (t : Type) = fun () ->
        match t with
        | GenericTypeOf GenericTypes.iEnum elemType ->
            let elemWriter = getWriterFunc elemType
            if XmlConfig.Instance.UseAttributes
            then
                let info = getTypeWriterInfo t
                Some <| ListSerializer.getGenericEnumerableAttributeWriter elemWriter elemType info
            else
                Some <| ListSerializer.getGenericEnumerableWriter elemWriter elemType
        | _ when matchInterface typeof<IEnumerable> t ->
            Some <| injectWriteTag (ListSerializer.writeEnumerable getWriterFunc)
        | _ -> None

    /// Determine the TypeWriterInfo for the given PropertyInfo
    and determineWriterInfo (ps, attrs) (pi : PropertyInfo) =
        match getAttribute<XmlAttributeAttribute> pi with
        | Some attr ->
            let t = pi.PropertyType
            let writer =
                match XmlConfig.Instance.TryGetSerializer t with
                | Some serializer -> serializer
                | None ->
                    match AttributeSerializer.getValueWriter t with
                    | Some writer -> writer
                    | None -> SerializerFunc(fun v -> v.ToString())
            let key = if notWhite attr.Name then attr.Name else pi.Name
            let getter = ReflectionHelpers.getObjGetter pi
            let info =
                { GetFunc = getter;
                  Key = key;
                  ToStr = writer }
            ps, info :: attrs 
        | None ->
            let info = buildPropertyWriterInfo pi
            info :: ps, attrs

    /// Get the PropertyWriterInfo array for the given type
    and getTypeWriterInfo (t : Type) =
        match (!propertyCache).TryGetValue t with
        | true, props -> props
        | _ ->
            let properties = ReflectionHelpers.getSerializableProperties t
            let order = fun x -> x.Order
            let info =
                if XmlConfig.Instance.UseAttributes then
                    let ps, attrs =
                        properties
                        |> Array.fold determineWriterInfo ([], [])
                    { Properties = List.sortBy order ps; Attributes = List.rev attrs }
                else
                    let ps =
                        properties
                        |> Array.map buildPropertyWriterInfo
                        |> List.ofArray
                        // sort by order descending
                        |> List.sortBy order
                    { Properties = ps; Attributes = [] }
            Atom.updateAtomDict propertyCache t info

    /// Class writer function without attribute support
    and writeClass func (info: TypeWriterInfo) (name : NameInfo) (writer : TextWriter) (value : obj) =
        func name.Name name writer (writeClassInner info) value

    /// Class writer function with attribute support
    and writeClassWithAttributes (typeInfo : TypeInfo) (info : TypeWriterInfo) (name : NameInfo) (writer : TextWriter) (value : obj) =
        let statics = typeInfo.Attributes |> List.ofArray
        let attr =
            extractAttributeValues info value
            |> List.append statics
        writeTagAttributes name.Name attr name writer (writeClassInner info) value

    /// Writer for classes and other reference types
    and writeClassInner (info: TypeWriterInfo) _ (writer : TextWriter) (value : obj) =
        info.Properties
        |> List.iter (fun p ->
            let v = p.GetFunc.Invoke(value)
            if v <> null then
                let writeFunc = p.WriteFunc.Value
                writeFunc p.Name writer v
            elif XmlConfig.Instance.IncludeNullValues then
                writeEmptyTag p.Name.Name writer)

    /// Try to determine a class or interface serialization function
    and getClassWriter (t : Type) = fun () ->
        if t.IsClass || t.IsInterface then
            if t.IsAbstract && not t.IsInterface then
                Some writeAbstractProperties
            else
                let typeInfo = getTypeInfo t
                let writerInfo = getTypeWriterInfo t
                if XmlConfig.Instance.UseAttributes then
                    Some (writeClassWithAttributes typeInfo writerInfo)
                else
                    let func =
                        match typeInfo.Namespace with
                        | Some ns -> writeTagNamespace ns
                        | None    -> writeTag
                    Some (writeClass func writerInfo)
        else None

    /// Try to determine a writer function for a dictionary
    and getDictionaryWriter (t : Type) = fun () ->
        if matchInterface typeof<IDictionary> t then
            Some <| injectWriteTag writeDictionary
        else None

    /// Try to determine a writer function for array types
    and getArrayWriter (t : Type) = fun () ->
        if t.IsArray then
            if t = typeof<byte[]> then Some <| injectWriteTag ValueTypeSerializer.writeBytes
            elif t = typeof<char[]> then Some <| injectWriteTag ValueTypeSerializer.writeChars
            elif t = typeof<int[]> then Some <| injectWriteTag ListSerializer.writeIntArray
            elif t = typeof<string[]> then Some <| injectWriteTag ListSerializer.writeStrArray
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
                let inner _ _ _= 
                    kv { name with Name = name.Key } writer key
                    vw { name with Name = name.Value } writer dictVal
                writeTag name.Item name writer inner key
            | _ -> ())

    and getObjectWriter = fun() ->
        Some <| injectWriteTag ValueTypeSerializer.writeObject

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
        let typeInfo = getTypeInfo targetType
        let name = getDefaultNameInfo typeInfo.ClsName
        let writerFunc = getWriterFunc targetType
        writerFunc name writer element

    /// Clear the serializer cache
    let clearCache() =
        Atom.clearAtomDict serializerCache
