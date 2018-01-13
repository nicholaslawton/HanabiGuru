module Seq

open System.Collections.Generic

let findClosest p n (s : seq<_>) =
    use e = s.GetEnumerator()
    let rec step i (closestMatch, pos) (e : IEnumerator<_>) =
        if not <| e.MoveNext() then closestMatch else
            if not <| p e.Current then
        match (e.MoveNext(), i < n, e.Current) with
        | (false, _, _) -> closestMatch
        | (true, false, _) -> step (i + 1) closestMatch e
        | (false, e.MoveNext
        if i < n && e.MoveNext()
        then step (i + 1) closestMatch e
        else 
    let mutable i = 0
    while e.MoveNext() && i < n do
        i <- i + 1
        if i >= n && p e.Current
        then Some e.Current
        else None

let splitAt n (s : seq<_>) =
    let rec takeStep taken i (e : IEnumerator<_>) =
        if i < n && e.MoveNext()
        then takeStep (e.Current :: taken) (i + 1) e
        else List.rev taken
    use e = s.GetEnumerator()
    let taken = takeStep [] 0 e
    let remainder = seq { while e.MoveNext() do yield e.Current }
    (taken, remainder)

(*
    let mutable taken = []
    let mutable i = 0
    let remainder =
        seq {
            use e = s.GetEnumerator()
            while e.MoveNext() do
                if i < n
                then
                    taken <- e.Current :: taken
                    i <- i + 1
                else yield e.Current
        }
        |> Seq.skip n

    (List.rev taken, remainder)
    *)
