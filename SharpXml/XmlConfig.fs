namespace SharpXml

/// Singleton configuration class containing all
/// preferences of the XmlSerializer
type XmlConfig private() =

    let mutable includeNullValues = false
    let mutable excludeTypeInfo = true
    let mutable emitCamelCaseNames = false
    let mutable writeXmlHeader = false

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
