namespace SharpXml

type XmlConfig private() =

    let mutable includeNullValues = false
    let mutable excludeTypeInfo = true
    let mutable emitCamelCaseNames = false
    let mutable writeXmlHeader = false

    static let mutable instance = lazy(XmlConfig())
    static member Instance with get() = instance.Value

    member x.IncludeNullValues
        with get() = includeNullValues
        and set(v) = includeNullValues <- v

    member x.ExcludeTypeInfo
        with get() = excludeTypeInfo
        and set(v) = excludeTypeInfo <- v

    member x.EmitCamelCaseNames
        with get() = emitCamelCaseNames
        and set(v) = emitCamelCaseNames <- v

    member x.WriteXmlHeader
        with get() = writeXmlHeader
        and set(v) = writeXmlHeader <- v
