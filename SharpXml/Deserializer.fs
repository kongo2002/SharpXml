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

namespace SharpXml

/// Application exception thrown during SharpXml
/// serialization and deserialization
exception SharpXmlException of string

/// Reader function delegate
type internal ReaderFunc = XmlParser.ParserInfo -> obj

/// Record type containing the deserialization information
/// for a specific property member
type internal PropertyReaderInfo = {
    Info : System.Reflection.PropertyInfo
    Reader : ReaderFunc
    Setter : SetterFunc }

/// Record type containing the deserialization information
/// of a specific type and all its members that have to be deserialized
type internal TypeBuilderInfo = {
    Type : System.Type
    // TODO: I would love to use a case-insensitive FSharpMap instead
    Props : System.Collections.Generic.Dictionary<string, PropertyReaderInfo>
    Ctor : EmptyConstructor }

/// Record type containing the deserialization information
/// for record types
type internal RecordBuilderInfo = {
    Type : System.Type
    Readers : Map<string, (ReaderFunc * int)>
    Ctor : obj[] -> obj
    Fields : int }

/// Record type containing the deserialization information
/// for tuple types
type internal TupleBuilderInfo = {
    Type : System.Type
    Readers : ReaderFunc array
    Ctor : obj[] -> obj
    Fields : int }

module internal ValueTypeDeserializer =

    open System

    open SharpXml.XmlParser

    let inline buildValueReader reader = fun info ->
        let str = eatContent info
        if not info.IsEnd then
            let value = reader str
            eatClosingTag info
            value
        else null

    let getEnumReader (t : Type) = fun () ->
        if t.IsEnum then
            fun i -> Enum.Parse(t, i)
            |> buildValueReader
            |> Some
        else None

    let getValueReader (t : Type) = fun () ->
        let reader =
            match Type.GetTypeCode(t) with
            | TypeCode.Boolean -> Boolean.Parse >> box |> Some
            | TypeCode.Byte -> Byte.Parse >> box |> Some
            | TypeCode.Int16 -> Int16.Parse >> box |> Some
            | TypeCode.Int32 -> Int32.Parse >> box |> Some
            | TypeCode.Int64 -> Int64.Parse >> box |> Some
            | TypeCode.Char -> Char.Parse >> box |> Some
            | TypeCode.DateTime -> DateTime.Parse >> box |> Some
            | TypeCode.Decimal -> Decimal.Parse >> box |> Some
            | TypeCode.Double -> Double.Parse >> box |> Some
            | TypeCode.SByte -> SByte.Parse >> box |> Some
            | TypeCode.Single -> Single.Parse >> box |> Some
            | TypeCode.UInt16 -> UInt16.Parse >> box |> Some
            | TypeCode.UInt32 -> UInt32.Parse >> box |> Some
            | TypeCode.UInt64 -> UInt64.Parse >> box |> Some
            | TypeCode.String -> box |> Some
            | _ -> None
        match reader with
        | Some r -> Some(buildValueReader r)
        | _ -> None

    /// String reader function
    let stringReader : ParserInfo -> obj =
        box |> buildValueReader

/// Dictionary related deserialization logic
module internal DictionaryDeserializer =

    open System.Collections
    open System.Collections.Generic
    open System.Collections.Specialized

    open SharpXml.XmlParser

    let parseKeyValueCollection invoker (keyReader : ReaderFunc) (valueReader : ReaderFunc) (input : ParserInfo) =
        let rec inner() =
            if not input.IsEnd then
                let itemTag, _ = eatSomeTag input
                if not input.IsEnd && itemTag <> TagType.Close then
                    // read key tag
                    eatSomeTag input |> ignore
                    let key = keyReader input
                    let vTag, _ = eatSomeTag input
                    if not input.IsEnd && vTag <> TagType.Close then
                        let value = valueReader input
                        invoker key value
                        eatClosingTag input
                        inner()
        inner()

    /// Dictionary reader function
    let dictReader<'a, 'b when 'a : equality> (keyReader : ReaderFunc) (valueReader : ReaderFunc) xml =
        let dictionary = Dictionary<'a, 'b>()
        let invoker (key : obj) (value : obj) =
            dictionary.[key :?> 'a] <- value :?> 'b
        parseKeyValueCollection invoker keyReader valueReader xml
        dictionary

    /// Reader function for non-generic NameValueCollection
    let nameValueCollectionReader (ctor : unit -> #NameValueCollection) xml =
        let collection = ctor()
        let invoker (key : obj) (value : obj) =
            collection.[key :?> string] <- value :?> string
        let reader = ValueTypeDeserializer.stringReader
        parseKeyValueCollection invoker reader reader xml
        box collection

    /// Reader function for non-generic HashTable
    let hashTableReader xml =
        let table = Hashtable()
        let invoker (key : obj) (value : obj) =
            table.Add(key :?> string, value :?> string)
        let reader = ValueTypeDeserializer.stringReader
        parseKeyValueCollection invoker reader reader xml
        box table

/// List related deserialization logic
module internal ListDeserializer =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Collections.Specialized

    open SharpXml.XmlParser

    /// Parse one element for a deserialised list structure
    let inline parseListElement<'a> (reader : ReaderFunc) element =
        match reader(element) with
        | null -> None
        | x -> Some(x :?> 'a)

    let parseList<'a> (elemParser : ReaderFunc) (info : ParserInfo) =
        let list = List<'a>()
        let rec inner() =
            if not info.IsEnd then
                let tag, _ = eatSomeTag info
                if not info.IsEnd && tag <> TagType.Close then
                    // TODO: maybe use an option value in here
                    // TODO: ...parseListElement
                    let value = elemParser info :?> 'a
                    list.Add(value)
                    inner()
        inner()
        list

    let parseListUntyped (lst : IList) (elemParser : ReaderFunc) (info : ParserInfo) =
        let rec inner() =
            if not info.IsEnd then
                let tag, _ = eatSomeTag info
                if not info.IsEnd && tag <> TagType.Close then
                    // TODO: maybe use an option value in here
                    // TODO: ...parseListElement
                    let value = elemParser info
                    lst.Add(value) |> ignore
                    inner()
        inner()

    /// Reader function for immutable F# lists
    let listReader<'a> (reader : ReaderFunc) xml =
        let list = parseList<'a> reader xml
        List.ofSeq list

    /// Reader function for CLR list (System.Collections.Generic.List<T>)
    let clrListReader<'a> (reader : ReaderFunc) xml =
        parseList<'a> reader xml

    /// Reader function for arrays
    let arrayReader<'a> (reader : ReaderFunc) xml =
        let list = parseList<'a> reader xml
        list.ToArray()

    /// Reader function for untyped collections
    let collectionReader (ctor : EmptyConstructor) xml =
        let list = ctor.Invoke() :?> IList
        parseListUntyped list ValueTypeDeserializer.stringReader xml
        list

    /// Reader function for hash sets
    let hashSetReader<'a> (reader : ReaderFunc) xml =
        HashSet(parseList<'a> reader xml)

    /// Reader function for generic collections
    let genericCollectionReader<'a> (reader : ReaderFunc) (ctor : EmptyConstructor) xml =
        let collection = ctor.Invoke() :?> ICollection<'a>
        let list = parseList<'a> reader xml
        list.ForEach(fun elem -> collection.Add(elem))
        collection

    let genericROReader<'a> (reader : ReaderFunc) (ctor : System.Reflection.ConstructorInfo) xml =
        let list = clrListReader<'a> reader xml
        ctor.Invoke([| list |])

    /// Reader function for queues
    let queueReader<'a> (reader : ReaderFunc) xml =
        Queue<'a>(parseList<'a> reader xml)

    /// Reader function for stacks
    let stackReader<'a> (reader : ReaderFunc) xml =
        Stack<'a>(parseList<'a> reader xml)

    /// Reader function for generic linked lists
    let linkedListReader<'a> (reader : ReaderFunc) xml =
        LinkedList<'a>(parseList<'a> reader xml)

    /// Specialized reader function for string arrays
    let stringArrayReader xml =
        listReader<string> ValueTypeDeserializer.stringReader xml |> List.toArray |> box

    /// Specialized reader function for integer arrays
    let intArrayReader xml =
        let intReader = Int32.Parse >> box |> ValueTypeDeserializer.buildValueReader
        listReader<int> intReader xml |> List.toArray |> box

    /// Specialized reader function for byte arrays
    let byteArrayReader xml =
        let reader = ValueTypeDeserializer.buildValueReader Convert.FromBase64String
        reader xml |> box

    /// Specialized reader function for char arrays
    let charArrayReader xml =
        let reader = ValueTypeDeserializer.buildValueReader (fun v -> v.ToCharArray())
        reader xml |> box

/// Deserialization logic
module internal Deserializer =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Collections.ObjectModel
    open System.Collections.Specialized
    open System.Reflection

    open Microsoft.FSharp.Reflection

    open SharpXml.Attempt
    open SharpXml.Extensions
    open SharpXml.TypeHelper
    open SharpXml.Utils
    open SharpXml.XmlParser

    /// Name of the static parsing method
    let parseMethodName = "ParseXml"

    /// BindingFlags to find the static parse method
    let parseMethodFlags = BindingFlags.Public ||| BindingFlags.Static

    /// TypeBuilder dictionary
    let propertyCache = ref (Dictionary<Type, TypeBuilderInfo>())

    /// Reader function cache
    let readerCache = ref (Dictionary<Type, ReaderFunc>())

    /// Try to find a constructor of the specified type
    /// with a single string parameter
    let findStringConstructor (t : Type) =
        t.GetConstructors()
        |> Array.tryFind (fun ctor ->
            let ps = ctor.GetParameters()
            ps.Length = 1 && ps.[0].ParameterType = typeof<string>)

    /// Try to get a reader based on a string value constructor
    let getStringTypeConstructor (t : Type) = fun () ->
        match findStringConstructor t with
        | Some ctor ->
            fun (v : string) -> ctor.Invoke([| v |])
            |> ValueTypeDeserializer.buildValueReader
            |> Some
        | _ -> None

    let getGenericListFunction name t =
        // TODO: I don't like this string-based reflection at all
        let flags = BindingFlags.NonPublic ||| BindingFlags.Static
        let reader = Type.GetType("SharpXml.ListDeserializer").GetMethod(name, flags)
        reader.MakeGenericMethod([| t |])

    /// Try to find the static 'ParseXml' method on the specified type
    let findStaticParseMethod (t : Type) =
        t.GetMethod(parseMethodName, parseMethodFlags, null, [| typeof<string> |], null)
        |> toOption

    /// Try to get a reader based on the type's static 'ParseXml' method
    let getStaticParseMethod (t : Type) = fun () ->
        match findStaticParseMethod t with
        | Some parse ->
            // TODO: maybe use Delegate.CreateDelegate()
            fun (v : string) -> parse.Invoke(null, [| v |])
            |> ValueTypeDeserializer.buildValueReader
            |> Some
        | _ -> None

    /// Reader function that utilizes a custom DeserializerFunc
    let customDeserializerReader (func: DeserializerFunc) = fun (info: ParserInfo) ->
        if not info.IsEnd then
            let start = info.Index
            let index = eatUnknownTilClosing info
            let toParse = new String(info.Value, start, index - start)
            func.Invoke(toParse)
        else null

    /// Get a reader function for NameValueCollection types
    let getNameValueCollectionReader (t : Type) =
        let ctor =
            if t = typeof<NameValueCollection> then
                fun() -> NameValueCollection()
            else
                let func = ReflectionHelpers.getConstructorMethod t
                fun() -> func.Invoke() :?> NameValueCollection
        Some <| DictionaryDeserializer.nameValueCollectionReader ctor

    /// Build the PropertyReaderInfo record based on the given PropertyInfo
    let rec buildReaderInfo (p : PropertyInfo) = {
        Info = p;
        Reader = getReaderFunc p.PropertyType;
        Setter = ReflectionHelpers.getObjSetter p }

    /// Build the TypeBuilderInfo record for the given Type
    and buildTypeBuilderInfo (t : Type) =
        let map =
            ReflectionHelpers.getDeserializableProperties t
            |> Seq.map (fun p ->
                let name =
                    match getAttribute<SharpXml.Common.XmlElementAttribute> p with
                    | Some attr when notWhite attr.Name -> attr.Name
                    | _ -> p.Name
                name, buildReaderInfo p)
            |> dict
        { Type = t
          Props = Dictionary(map, StringComparer.OrdinalIgnoreCase)
          Ctor = ReflectionHelpers.getConstructorMethod t }

    /// Determine the TypeBuilderInfo for the given Type
    and getTypeBuilderInfo (t : Type) =
        match (!propertyCache).TryGetValue t with
        | true, builder -> builder
        | _ ->
            let builder = buildTypeBuilderInfo t
            Atom.updateAtomDict propertyCache t builder

    and buildGenericFunction name t =
        let mtd = getGenericListFunction name t
        let elemReader = getReaderFunc t
        fun (xml : ParserInfo) -> mtd.Invoke(null, [| elemReader; xml |])

    /// Get a reader function for generic F# lists
    and getTypedFsListReader =
        buildGenericFunction "listReader"

    /// Get a reader function for generic lists
    and getTypedListReader =
        buildGenericFunction "clrListReader"

    /// Get a reader function for arrays
    and getTypedArrayReader =
        buildGenericFunction "arrayReader"

    /// Get a reader function for hash sets
    and getHashSetReader =
        buildGenericFunction "hashSetReader"

    /// Get a reader function for queues
    and getQueueReader =
        buildGenericFunction "queueReader"

    /// Get a reader function for stacks
    and getStackReader =
        buildGenericFunction "stackReader"

    /// Get a reader function for generic linked lists
    and getLinkedListReader =
        buildGenericFunction "linkedListReader"

    /// Get a reader function for generic collections
    and getGenericCollectionReader (listType : Type) (t : Type) =
        let mtd = getGenericListFunction "genericCollectionReader" t
        let elemReader = getReaderFunc t
        let ctor = ReflectionHelpers.getConstructorMethod listType
        fun (xml : ParserInfo) -> mtd.Invoke(null, [| elemReader; ctor; xml |])

    /// Get a reader function for generic readonly collections
    and getGenericROReader ctor (listType : Type) (t : Type) =
        let mtd = getGenericListFunction "genericROReader" t
        let elemReader = getReaderFunc t
        fun (xml : ParserInfo) -> mtd.Invoke(null, [| elemReader; ctor; xml |])

    /// Try to determine a reader function for array types
    and getArrayReader (t : Type) = fun () ->
        if not t.IsArray then None else
            if t = typeof<string[]> then Some ListDeserializer.stringArrayReader
            elif t = typeof<int[]> then Some ListDeserializer.intArrayReader
            elif t = typeof<byte[]> then Some ListDeserializer.byteArrayReader
            elif t = typeof<char[]> then Some ListDeserializer.charArrayReader
            else
                let elem = t.GetElementType()
                Some <| getTypedArrayReader elem

    /// Try to determine a reader function for list types
    and getListReader (t : Type) = fun () ->
        if isGenericType t then
            match t with
            | GenericTypeOf GenericTypes.roColl gen ->
                let param = GenericTypes.iList.MakeGenericType([| gen |])
                let ctor = t.GetConstructor([| param |])
                if ctor <> null then Some <| getGenericROReader ctor t gen else None
            | GenericTypeOf GenericTypes.hashSet gen -> Some <| getHashSetReader gen
            | GenericTypeOf GenericTypes.linkedList gen -> Some <| getLinkedListReader gen
            | GenericTypeOf GenericTypes.iColl gen ->
                if hasGenericTypeDefinitions t [| GenericTypes.list |]
                then Some <| getTypedListReader gen
                else Some <| getGenericCollectionReader t gen
            | GenericTypeOf GenericTypes.queue gen -> Some <| getQueueReader gen
            | GenericTypeOf GenericTypes.stack gen -> Some <| getStackReader gen
            | GenericTypeOf GenericTypes.fsList gen -> Some <| getTypedFsListReader gen
            | _ -> None
        elif isOrDerived t typeof<NameValueCollection> then
            getNameValueCollectionReader t
        elif matchInterface typeof<IList> t then
            let ctor = ReflectionHelpers.getConstructorMethod t
            let reader = ListDeserializer.collectionReader ctor >> box
            Some reader
        else None

    and getTypedDictionaryReader key value =
        // TODO: this does not look sane at all
        let flags = BindingFlags.NonPublic ||| BindingFlags.Static
        let reader = Type.GetType("SharpXml.DictionaryDeserializer").GetMethod("dictReader", flags)
        let mtd = reader.MakeGenericMethod([| key; value |])
        let keyReader = getReaderFunc key
        let valueReader = getReaderFunc value
        fun (xml : ParserInfo) -> mtd.Invoke(null, [| keyReader; valueReader; xml |])

    and getDictionaryReader (t : Type) = fun () ->
        let dictInterface = typeof<IDictionary>
        if matchInterface dictInterface t then
            match t with
            | GenericTypesOf GenericTypes.dict (k, v) ->
                Some <| getTypedDictionaryReader k v
            | _ when t = typeof<Hashtable> ->
                Some <| DictionaryDeserializer.hashTableReader
            | _ -> None
        else None

    /// Class reader function
    and readClass (builder : TypeBuilderInfo) (xml : ParserInfo) =
        let instance = builder.Ctor.Invoke()
        let rec inner() =
            if not xml.IsEnd then
                let name, tag = eatTag xml
                match tag with
                | TagType.Open ->
                    match builder.Props.TryGetValue name with
                    | true, prop ->
                        try
                            let reader = prop.Reader
                            prop.Setter.Invoke(instance, reader(xml))
                        with ex ->
                            let error = sprintf "Unable to deserialize property '%s' of type '%s'" prop.Info.Name prop.Info.DeclaringType.FullName
                            if XmlConfig.Instance.ThrowOnError then
                                raise (SharpXmlException error)
                            else
                                Diagnostics.Trace.WriteLine(error)
                            eatUnknownTilClosing xml |> ignore
                    | _ -> eatUnknownTilClosing xml |> ignore
                    inner()
                | TagType.Single -> inner()
                | _ -> ()
        inner()
        instance

    /// Try to determine a matching class reader function
    and getClassReader (t : Type) = fun () ->
        if t.IsClass && not t.IsAbstract then
            getTypeBuilderInfo t
            |> readClass
            |> Some
        else None

    and getRecordBuilderInfo (t : Type) : RecordBuilderInfo =
        let fields = FSharpType.GetRecordFields t
        let readers =
            fields
            // we have to lowercase the property name because
            // the F# map is case-sensitive
            |> Array.mapi (fun i pi -> pi.Name.ToLowerInvariant(), (getReaderFunc pi.PropertyType, i))
            |> Map.ofArray
        { Type = t; Readers = readers; Ctor = FSharpValue.PreComputeRecordConstructor t; Fields = fields.Length }

    and readRecord (rb : RecordBuilderInfo) (xml : ParserInfo) =
        let objects = Array.zeroCreate<obj> rb.Fields
        let rec inner() =
            if not xml.IsEnd then
                let name, tag = eatTag xml
                match tag with
                | TagType.Open ->
                    // we have to lowercase here because the map is case-sensitive
                    let lower = name.ToLowerInvariant()
                    match rb.Readers.TryFind lower with
                    | Some (reader, i) ->
                        try
                            objects.[i] <- reader xml
                        with ex ->
                            let error = sprintf "Unable to deserialize field of record type '%s'" rb.Type.FullName
                            if XmlConfig.Instance.ThrowOnError then
                                raise (SharpXmlException error)
                            else
                                Diagnostics.Trace.WriteLine(error)
                    | _ -> ()
                    inner()
                | TagType.Single -> inner()
                | _ -> ()
        inner()
        rb.Ctor objects

    and readTuple (tb : TupleBuilderInfo) (xml : ParserInfo) =
        let objects = Array.zeroCreate<obj> tb.Fields
        let rec inner index  =
            if not xml.IsEnd && index < tb.Fields then
                let tag, _ = eatSomeTag xml
                match tag with
                | TagType.Open ->
                    try
                        objects.[index] <- tb.Readers.[index] xml
                    with ex ->
                        let error = sprintf "Unable to deserialize item %d of tuple type '%s'" (index+1) tb.Type.FullName
                        if XmlConfig.Instance.ThrowOnError then
                            raise (SharpXmlException error)
                        else
                            Diagnostics.Trace.WriteLine(error)
                    inner (index+1)
                | TagType.Single -> inner (index+1)
                | _ -> ()
        inner 0
        tb.Ctor objects

    and getFsRecordReader (t : Type) = fun() ->
        if FSharpType.IsRecord t then
            getRecordBuilderInfo t
            |> readRecord
            |> Some
        else None

    and getTupleBuilderInfo (t : Type) : TupleBuilderInfo =
        let items = FSharpType.GetTupleElements t
        let readers =
            items
            |> Array.map getReaderFunc
        let ctor = FSharpValue.PreComputeTupleConstructor t
        { Type = t; Readers = readers; Fields = items.Length; Ctor = ctor }

    and getFsTupleReader (t : Type) = fun() ->
        if FSharpType.IsTuple t then
            getTupleBuilderInfo t
            |> readTuple
            |> Some
        else None

    /// Determine the ReaderFunc delegate for the given Type
    and determineReader (objType : Type) =
        let t = objType.NullableUnderlying()
        let reader = attempt {
            let! enumReader = ValueTypeDeserializer.getEnumReader t
            let! valueReader = ValueTypeDeserializer.getValueReader t
            let! arrayReader = getArrayReader t
            let! dictReader = getDictionaryReader t
            let! listReader = getListReader t
            let! staticReader = getStaticParseMethod t
            let! stringCtor = getStringTypeConstructor t
            let! parseXmlReader = getStaticParseMethod t
            let! recordReader = getFsRecordReader t
            let! tupleReader = getFsTupleReader t
            let! classReader = getClassReader t
            classReader }
        reader

    /// Get the ReaderFunc for the specified type.
    /// The function is either obtained from the cache or built on request
    and getReaderFunc (t : Type) =
        match XmlConfig.Instance.TryGetDeserializer t with
        | Some custom -> customDeserializerReader custom
        | None ->
            match (!readerCache).TryGetValue t with
            | true, reader -> reader
            | _ ->
                match determineReader t with
                | Some func -> Atom.updateAtomDict readerCache t func
                | _ ->
                    let err = sprintf "could not determine deserialization logic for type '%s'" t.FullName
                    raise (SharpXmlException err)