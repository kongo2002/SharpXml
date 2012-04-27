module AssemblyTests

open System
open NUnit.Framework

open SharpXml

[<Test>]
let assemblyName() =
    let fullName = Assembly.getAssemblyName typeof<string>.AssemblyQualifiedName
    let name = Assembly.getAssemblyName typeof<string>.Name
    Assert.AreEqual(Some "mscorlib", fullName)
    Assert.AreEqual(None, name)