namespace SharpXml

module Utils =

    open System.Threading

    let rec swapRef<'T when 'T : not struct> reference newValue =
        let current = !reference
        let result = Interlocked.CompareExchange<'T>(reference, newValue, current)
        if not (obj.ReferenceEquals(result, current)) then
            Thread.SpinWait 20
            swapRef reference newValue
