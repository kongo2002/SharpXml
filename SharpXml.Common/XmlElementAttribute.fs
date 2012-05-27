namespace SharpXml.Common

open System

/// Attribute that allows customization of the serialization
/// and deserialization behavior of the SharpXml.XmlSerializer
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property)>]
type XmlElementAttribute() =
    inherit Attribute()

    let mutable name = Unchecked.defaultof<string>
    let mutable itemName = Unchecked.defaultof<string>
    let mutable keyName = Unchecked.defaultof<string>
    let mutable valueName = Unchecked.defaultof<string>

    /// Name to override the property name
    member x.Name
        with get() = name
        and set(v) = name <- v

    /// Name to override item names of collections
    member x.ItemName
        with get() = itemName
        and set(v) = itemName <- v

    /// Name to override key names of key-value collections
    member x.KeyName
        with get() = keyName
        and set(v) = keyName <- v

    /// Name to override value names of key-value collections
    member x.ValueName
        with get() = valueName
        and set(v) = valueName <- v

