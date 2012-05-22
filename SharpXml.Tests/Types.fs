namespace SharpXml.Tests

module Types =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization

    type TestEnum =
        | Undefined = 0
        | Foo = 1
        | Bar = 2

    type CustomList<'T> =
        inherit List<'T>

        new () = { inherit List<'T>() }
        new (collection : IEnumerable<'T>) = { inherit List<'T>(collection) }

        member x.AddPair(first, second) =
            x.Add(first)
            x.Add(second)

    type TestClass(val1 : int, val2 : string) =

        let mutable v1 = val1
        let mutable v2 = val2

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type SimpleClass() =

        let mutable v1 = Unchecked.defaultof<string>
        let mutable v2 = Unchecked.defaultof<int>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type TestClass3() =

        let mutable v1 = Unchecked.defaultof<string[]>
        let mutable v2 = Unchecked.defaultof<int>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type TestClass4() =

        let mutable v1 = Unchecked.defaultof<TestClass[]>
        let mutable v2 = Unchecked.defaultof<int>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type ListClass() =

        let mutable v1 = Unchecked.defaultof<List<TestClass>>
        let mutable v2 = Unchecked.defaultof<int>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type DictClass() =

        let mutable v1 = Unchecked.defaultof<Dictionary<string, int>>
        let mutable v2 = Unchecked.defaultof<int>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type EnumClass() =

        let mutable v1 = Unchecked.defaultof<TestEnum>
        let mutable v2 = Unchecked.defaultof<int>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type NestedClass() =

        let mutable v1 = Unchecked.defaultof<string>
        let mutable v2 = Unchecked.defaultof<SimpleClass>

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

    type CustomListClass() =

        let mutable v1 = Unchecked.defaultof<int>
        let mutable v2 = Unchecked.defaultof<CustomList<string>>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type ArrayListClass() =

        let mutable v1 = Unchecked.defaultof<int>
        let mutable v2 = Unchecked.defaultof<ArrayList>

        member x.V1
            with get() = v1
            and set v = v1 <- v
        member x.V2
            with get() = v2
            and set v = v2 <- v

    type GenericListClass<'T>() =

        let mutable v1 = Unchecked.defaultof<int>
        let mutable v2 = Unchecked.defaultof<List<'T>>

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

    [<DataContract(Name = "ContractClass3", Namespace = "")>]
    type ContractClass3() =

        let mutable v1 = Unchecked.defaultof<string[]>
        let mutable v2 = Unchecked.defaultof<int>

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
