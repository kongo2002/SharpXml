namespace SharpXml.Tests

module TestHelpers =

    open NUnit.Framework
    open NUnit.Framework.Constraints

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