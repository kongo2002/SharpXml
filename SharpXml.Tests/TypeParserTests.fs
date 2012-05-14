namespace SharpXml.Tests

module TypeParserTests =

    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers

    [<Test>]
    let eatTag01() =
        let input = " foo <testTag rest/> <hamEggs/>"
        let index, value, single = TypeParser.eatTag input 0
        value |> should equal "testTag"
        index |> should equal 19
        single |> should equal true

    [<Test>]
    let eatTag02() =
        let input = "<testTag />"
        let index, value, single = TypeParser.eatTag input 0
        value |> should equal "testTag"
        index |> should equal 10
        single |> should equal true


    [<Test>]
    let eatTag03() =
        let input = "< fooBar /> < testTag />"
        let index, value, single = TypeParser.eatTag input 10
        value |> should equal "testTag"
        index |> should equal 23
        single |> should equal true

    [<Test>]
    let eatTag04() =
        let input = "xxxx<fooBar>xxxxxxxxxx"
        let index, value, single = TypeParser.eatTag input 0
        value |> should equal "fooBar"
        index |> should equal 11
        single |> should equal false

    [<Test>]
    let eatContent01() =
        let input = "<one>this is a small test</test>"
        TypeParser.eatContent input 5 |> should equal "this is a small test"

    [<Test>]
    let eatContent02() =
        let input = "<one>this is &lt;b&gt;a&lt;/b&gt; small test</test>"
        TypeParser.eatContent input 5 |> should equal "this is <b>a</b> small test"