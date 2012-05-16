namespace SharpXml.Tests

module TestHelpers =

    open System.Diagnostics
    open NUnit.Framework
    open NUnit.Framework.Constraints

    let time func iterations =
        let sw = Stopwatch.StartNew()
        let rec loop f i =
            if i > 0 then f(); loop f (i-1)
        loop func iterations
        sw.Stop()
#if DEBUG
        Debug.WriteLine <| sprintf "Iterations: %d; Elapsed: %A" iterations sw.Elapsed
#else
        System.Console.WriteLine("Iterations: {0}; Elapsed: {1}", iterations, sw.Elapsed)
#endif

    let should (func : 'a -> #Constraint) x (actual : obj) =
        let constr = func x
        Assert.That(actual, constr)

    let shouldBe (func : #Constraint) (actual : obj) =
        Assert.That(actual, func)

    let equal x = EqualConstraint(x)

    let contain x = ContainsConstraint(x)

    let sameAs x = SameAsConstraint(x)

    let throw x = ThrowsConstraint(x)

    let throwNothing = ThrowsNothingConstraint()

    let Null = NullConstraint()

    let notNull = NotConstraint(NullConstraint())

    let True = TrueConstraint()

    let False = FalseConstraint()