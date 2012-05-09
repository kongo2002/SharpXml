namespace SharpXml.Tests

module Types =

    open System.Collections.Generic
    open System.Runtime.Serialization

    type TestClass(val1 : int, val2 : string) =

        let mutable v1 = val1
        let mutable v2 = val2

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type TestClass2() =

        let mutable v1 = Unchecked.defaultof<string>
        let mutable v2 = Unchecked.defaultof<int>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type NestedClass() =

        let mutable v1 = Unchecked.defaultof<string>
        let mutable v2 = Unchecked.defaultof<TestClass2>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type NestedClass2() =

        let mutable v1 = Unchecked.defaultof<string>
        let mutable v2 = Unchecked.defaultof<NestedClass2>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    [<DataContract(Name = "ContractClass", Namespace = "")>]
    type ContractClass() =

        let mutable v1 = Unchecked.defaultof<string>
        let mutable v2 = Unchecked.defaultof<int>

        [<DataMember>]
        member x.V1
            with get() = v1
            and set v = v1 <- v
        [<DataMember>]
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type TestRecord = {
        Value : int
        Name : string }

    [<DataContract(Name = "ContractClass2", Namespace = "")>]
    type ContractClass2() =

        let mutable v1 = Unchecked.defaultof<string>
        let mutable v2 = Unchecked.defaultof<Dictionary<string,int>>

        [<DataMember>]
        member x.V1
            with get() = v1
            and set v = v1 <- v
        [<DataMember>]
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type ITestInterface =
        abstract member Member1 : int
            with get, set

    type IAnotherInterface =
        inherit ITestInterface
        abstract member Member2 : string
            with get, set
