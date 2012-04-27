module UtilsTests

open System
open NUnit.Framework

open SharpXml.Attempt

[<Test>]
let attempt01() =
    let runs = ref 0
    let func v = fun () -> runs := !runs + 1; v
    let result = attempt {
        let! v1 = func None
        let! v2 = func None
        let! v3 = func <| Some 20
        v3 }
    Assert.AreEqual(Some 20, result)
    Assert.AreEqual(3, !runs)

[<Test>]
let attempt02() =
    let runs = ref 0
    let func v = fun () -> runs := !runs + 1; v
    let result = attempt {
        let! v1 = func None
        let! v2 = func <| Some 42
        let! v3 = func None
        v3 }
    Assert.AreEqual(Some 42, result)
    Assert.AreEqual(2, !runs)

[<Test>]
let attempt03() =
    let runs = ref 0
    let func v = fun () -> runs := !runs + 1; v
    let result = attempt {
        let! v1 = func None
        let! v2 = func None
        let! v3 = func None
        v3 }
    Assert.AreEqual(None, result)
    Assert.AreEqual(3, !runs)