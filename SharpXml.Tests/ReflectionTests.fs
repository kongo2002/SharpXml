namespace SharpXml.Tests

module ReflectionTests =

    open System
    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.Types

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
        let ctor2 = Reflection.getConstructorMethodByName "SharpXml.Tests.Types+TestClass2"
        Assert.IsNotNull(ctor1)
        Assert.IsNotNull(ctor2)

    [<Test>]
    let getGetter01() =
        let cls = TestClass(200, "foobar")
        [ "V1", box 200; "V2", box "foobar" ]
        |> List.iter (fun (n, v) ->
            let pi = typeof<TestClass>.GetProperty(n)
            let getter = Reflection.getGetter pi
            let ret = getter.Invoke(cls)
            Assert.AreEqual(v, ret))

    [<Test>]
    let getSetter01() =
        let cls = TestClass(200, "foobar")
        [ "V1", box 42, (fun (x:TestClass) -> box x.V1) ; "V2", box "barfoo", (fun (x:TestClass) -> box x.V2) ]
        |> List.iter (fun (n, v, g) ->
            let pi = typeof<TestClass>.GetProperty(n)
            let setter = Reflection.getSetter pi
            setter.Invoke(cls, v)
            Assert.AreEqual(v, g(cls)))