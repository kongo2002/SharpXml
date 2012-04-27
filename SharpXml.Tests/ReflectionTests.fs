module ReflectionTests

open System
open NUnit.Framework
open SharpXml

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

type ITestInterface =
    abstract member Member1 : int
        with get, set

type IAnotherInterface =
    inherit ITestInterface
    abstract member Member2 : string
        with get, set

[<Test>]
let getProperties01() =
    let props = Reflection.getPublicProperties typeof<TestClass>
    Assert.AreEqual(2, props.Length)

[<Test>]
let getInterfaceProperties01() =
    let props = Reflection.getInterfaceProperties typeof<ITestInterface>
    Assert.AreEqual(1, props.Length)
    Assert.AreEqual("Member1", props.[0].Name)
    Assert.AreEqual(typeof<int>, props.[0].PropertyType)

[<Test>]
let getInterfaceProperties02() =
    let props = Reflection.getInterfaceProperties typeof<IAnotherInterface>
    Assert.AreEqual(2, props.Length)

[<Test>]
let getDefaultValue() =
    Assert.AreEqual(null, Reflection.getDefaultValue typeof<TestClass>)
    Assert.AreEqual(DateTime.MinValue, Reflection.getDefaultValue typeof<DateTime>)
    Assert.AreEqual(0uy, Reflection.getDefaultValue typeof<byte>)
    Assert.AreEqual(null, Reflection.getDefaultValue typeof<IAnotherInterface>)

[<Test>]
let stringOrValueTypes() =
    let t = [ typeof<string>; typeof<int>; typeof<char> ]
    Assert.IsTrue(Reflection.areStringOrValueTypes t)
    Assert.IsFalse(Reflection.areStringOrValueTypes [ typeof<TestClass> ])

[<Test>]
let getEmptyConstructor01() =
    let ctor1 = Reflection.getConstructorMethod typeof<TestClass>
    let ctor2 = Reflection.getConstructorMethod typeof<TestClass2>
    Assert.IsNotNull(ctor1)
    Assert.IsNotNull(ctor2)

[<Test>]
let getEmptyConstructor02() =
    let ctor1 = Reflection.getConstructorMethodByName "System.String"
    let ctor2 = Reflection.getConstructorMethodByName "ReflectionTests+TestClass2"
    Assert.IsNotNull(ctor1)
    Assert.IsNotNull(ctor2)