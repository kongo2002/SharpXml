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

    [<Test>]
    let eatContent01() =
        let input = "<one>this is a small test</test>"
        TypeParser.eatContent input 5 |> should equal "this is a small test"

    [<Test>]
    let eatContent02() =
        let input = "<one>this is &lt;b&gt;a&lt;/b&gt; small test</test>"
        TypeParser.eatContent input 5 |> should equal "this is <b>a</b> small test"