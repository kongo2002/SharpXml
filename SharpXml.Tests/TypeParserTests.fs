module TypeParserTests

open SharpXml
open NUnit.Framework

[<Test>]
let eatTag01() =
    let input = " foo <testTag rest/> <hamEggs/>"
    let index, value = TypeParser.eatTag input 0
    Assert.AreEqual("testTag", value)
    Assert.AreEqual(19, index)

[<Test>]
let eatTag02() =
    let input = "<testTag />"
    let index, value = TypeParser.eatTag input 0
    Assert.AreEqual("testTag", value)
    Assert.AreEqual(10, index)

[<Test>]
let eatTag03() =
    let input = "< fooBar /> < testTag />"
    let index, value = TypeParser.eatTag input 10
    Assert.AreEqual("testTag", value)
    Assert.AreEqual(23, index)

[<Test>]
let eatTag04() =
    let input = "xxxx<fooBar>xxxxxxxxxx"
    let index, value = TypeParser.eatTag input 0
    Assert.AreEqual("fooBar", value)
    Assert.AreEqual(11, index)