namespace SharpXml.Tests

module ReflectionTests =

    open System
    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.Types
    open SharpXml.Tests.TestHelpers

    [<Test>]
    let ``Can get public properties``() =
        let props = Reflection.getPublicProperties typeof<TestClass>
        props.Length |> should equal 2

    [<Test>]
    let ``Can get public properties of interfaces``() =
        let props = Reflection.getInterfaceProperties typeof<ITestInterface>
        props.Length |> should equal 1
        props.[0].Name |> should equal "Member1"
        props.[0].PropertyType |> should equal typeof<int>

    [<Test>]
    let ``Can get public properties of inherited interfaces``() =
        let props = Reflection.getInterfaceProperties typeof<IAnotherInterface>
        props.Length |> should equal 2

    [<Test>]
    let ``Can get various default constructors``() =
        Reflection.getDefaultValue typeof<TestClass> |> shouldBe Null
        Reflection.getDefaultValue typeof<DateTime> |> should equal DateTime.MinValue
        Reflection.getDefaultValue typeof<byte> |> should equal 0uy
        Reflection.getDefaultValue typeof<IAnotherInterface> |> shouldBe Null

    [<Test>]
    let ``Can determine value types``() =
        let t = [ typeof<string>; typeof<int>; typeof<char> ]
        Reflection.areStringOrValueTypes t |> shouldBe True
        Reflection.areStringOrValueTypes [ typeof<TestClass> ] |> shouldBe False

    [<Test>]
    let ``Can get empty constructors of different classes``() =
        let ctor1 = Reflection.getConstructorMethod typeof<TestClass>
        let ctor2 = Reflection.getConstructorMethod typeof<SimpleClass>
        ctor1 |> shouldBe notNull
        ctor2 |> shouldBe notNull

    [<Test>]
    let ``Can get empty constructors by class names``() =
        let ctor1 = Reflection.getConstructorMethodByName "System.String"
        let ctor2 = Reflection.getConstructorMethodByName "SharpXml.Tests.Types+SimpleClass"
        ctor1 |> shouldBe notNull
        ctor2 |> shouldBe notNull

    [<Test>]
    let ``Can determine property getters``() =
        let cls = TestClass(200, "foobar")
        [ "V1", box 200; "V2", box "foobar" ]
        |> List.iter (fun (n, v) ->
            let pi = typeof<TestClass>.GetProperty(n)
            let getter = Reflection.getGetter pi
            let ret = getter.Invoke(cls)
            ret |> should equal v)

    [<Test>]
    let ``Can determine property setters``() =
        let cls = TestClass(200, "foobar")
        [ "V1", box 42, (fun (x:TestClass) -> box x.V1) ; "V2", box "barfoo", (fun (x:TestClass) -> box x.V2) ]
        |> List.iter (fun (n, v, g) ->
            let pi = typeof<TestClass>.GetProperty(n)
            let setter = Reflection.getSetter pi
            setter.Invoke(cls, v)
            g(cls) |> should equal v)