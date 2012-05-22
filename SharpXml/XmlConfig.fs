namespace SharpXml

open System
open System.Collections.Generic

/// Delegate type that contains type
/// specific serialization logic
type SerializerFunc = delegate of obj -> string

/// Delegate type that contains type
/// specific deserialization logic
type DeserializerFunc = delegate of string -> obj

/// Singleton configuration class containing all
/// preferences of the XmlSerializer
type XmlConfig private() =

    let mutable includeNullValues = false
    let mutable excludeTypeInfo = true
    let mutable emitCamelCaseNames = false
    let mutable writeXmlHeader = false
    let mutable throwOnError = false

    let serializerCache = ref (Dictionary<Type, SerializerFunc>())
    let deserializerCache = ref (Dictionary<Type, DeserializerFunc>())

    static let mutable instance = lazy(XmlConfig())

    /// Singleton instance
    static member Instance with get() = instance.Value

    /// Whether to include null values in the serialized output
    member x.IncludeNullValues
        with get() = includeNullValues
        and set(v) = includeNullValues <- v

    /// Whether to exclude additional type information for
    /// dynamic/anonymous types in the serialized output
    member x.ExcludeTypeInfo
        with get() = excludeTypeInfo
        and set(v) = excludeTypeInfo <- v

    /// Whether to convert property names into camel case
    /// for the serialized output (i.e 'MyValue' -> 'myValue')
    member x.EmitCamelCaseNames
        with get() = emitCamelCaseNames
        and set(v) = emitCamelCaseNames <- v

    /// Whether to include a XML header sequence in the
    /// serialized output
    member x.WriteXmlHeader
        with get() = writeXmlHeader
        and set(v) = writeXmlHeader <- v

    /// Whether to throw exceptions on deserialization errors
    member x.ThrowOnError
        with get() = throwOnError
        and set(v) = throwOnError <- v

    /// Register a serializer delegate for the specified type
    member x.RegisterSerializer<'T> (func : SerializerFunc) =
        Atom.updateAtomDict serializerCache typeof<'T> func |> ignore

    /// Register a deserializer delegate for the specified type
    member x.RegisterDeserializer<'T> (func : DeserializerFunc) =
        Atom.updateAtomDict deserializerCache typeof<'T> func |> ignore

    /// Unregister the serializer delegate for the specified type
    member x.UnregisterSerializer<'T>() =
        Atom.removeAtomDictElement serializerCache typeof<'T>

    /// Unregister the deserializer delegate for the specified type
    member x.UnregisterDeserializer<'T>() =
        Atom.removeAtomDictElement deserializerCache typeof<'T>

    /// Try to get a serializer delegate for the specified type
    member internal x.TryGetSerializer (t : Type) =
        (!serializerCache).TryGetValue t
        |> Utils.tryToOption

    /// Try to get a deserializer delegate for the specified type
    member internal x.TryGetDeserializer (t : Type) =
        (!deserializerCache).TryGetValue t
        |> Utils.tryToOption
