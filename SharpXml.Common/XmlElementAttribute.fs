namespace SharpXml.Common

open System

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property)>]
type XmlElementAttribute() =
    inherit Attribute()

    let mutable name = Unchecked.defaultof<string>
    let mutable itemName = Unchecked.defaultof<string>
    let mutable keyName = Unchecked.defaultof<string>

    member x.Name
        with get() = name
        and set(v) = name <- v

    member x.ItemName
        with get() = itemName
        and set(v) = itemName <- v

    member x.KeyName
        with get() = keyName
        and set(v) = keyName <- v

