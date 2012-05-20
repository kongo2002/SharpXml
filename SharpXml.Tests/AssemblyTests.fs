namespace SharpXml.Tests

module AssemblyTests =

    open System
    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers

    [<Test>]
    let ``Can determine the correct assembly name``() =
        let fullName = Assembly.getAssemblyName typeof<string>.AssemblyQualifiedName
        let name = Assembly.getAssemblyName typeof<string>.Name
        fullName |> should equal (Some "mscorlib")
        name |> should equal None