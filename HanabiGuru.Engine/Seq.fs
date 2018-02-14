module Seq

open System.Collections.Generic

let findClosest p n (s : seq<_>) =
    use e = s.GetEnumerator()
    let rec step i (e : IEnumerator<_>) (closestMatch, pos) =
        let nextStep = step (i + 1) e
        let distance = ((-) n) >> abs
        let closer x y = distance x < distance y
        let iCloser = pos |> Option.map (closer i) |> Option.defaultValue true
        let nextItem (e : IEnumerator<_>) = if e.MoveNext() then Some e.Current else None

        match iCloser, nextItem e with
        | false, _
        | true, None -> closestMatch
        | true, Some item when p item && i >= n -> Some e.Current
        | true, Some item when p item && i < n -> nextStep (Some item, Some i)
        | true, Some _ -> nextStep (closestMatch, pos)
    step 0 e (None, None)
