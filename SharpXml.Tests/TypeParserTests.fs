namespace SharpXml.Tests

module TypeParserTests =

    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers

    [<Test>]
    let eatTag01() =
        let input = " foo <testTag rest/> <hamEggs/>"
        let index, value = TypeParser.eatTag input 0
        value |> should equal "testTag"
        index |> should equal 19

    [<Test>]
    let eatTag02() =
        let input = "<testTag />"
        let index, value = TypeParser.eatTag input 0
        value |> should equal "testTag"
        index |> should equal 10

    [<Test>]
    let eatTag03() =
        let input = "< fooBar /> < testTag />"
        let index, value = TypeParser.eatTag input 10
        value |> should equal "testTag"
        index |> should equal 23

    [<Test>]
    let eatTag04() =
        let input = "xxxx<fooBar>xxxxxxxxxx"
        let index, value = TypeParser.eatTag input 0
        value |> should equal "fooBar"
        index |> should equal 11