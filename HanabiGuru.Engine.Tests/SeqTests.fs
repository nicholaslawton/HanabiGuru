module HanabiGuru.Engine.Tests.SeqTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

let seqEqual x y = Seq.compareWith Operators.compare x y = 0

[<Property>]
let ``Splitting and concatenating a sequence results in the original sequence`` (NonNegativeInt n) (list : int list) =
    let s = seq list
    Seq.splitAt n s |> fun (x, y) -> Seq.append x y |> seqEqual s

[<Property>]
let ``Splitting a sequence at a point less than its length returns a sequence of the requested length``
    (NonNegativeInt n)
    (list : string list) =

    let s = seq list
    let maxN = Seq.length s
    let n = if n <= maxN then n else n % (maxN + 1)
    Seq.splitAt n s |> fst |> Seq.length =! n

[<Property>]
let ``Splitting a sequence at a point beyond its length returns the original sequence and an empty sequence``
    (NonNegativeInt n)
    (list : string list) =

    let s = seq list
    let n = n + Seq.length s
    test <@ Seq.splitAt n s |> fun (x, y) -> seqEqual x s && Seq.isEmpty y @>
