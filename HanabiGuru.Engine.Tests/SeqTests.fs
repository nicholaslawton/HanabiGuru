module HanabiGuru.Engine.Tests.SeqTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

[<Property>]
let ``Finding the closest matching element returns the closest matching element``
    (p : int -> bool)
    (NonNegativeInt n)
    (list : int list) =

    let s = seq list
    let closestMatch =
        Seq.indexed s
        |> Seq.filter (snd >> p)
        |> Seq.sortBy (fst >> ((-) n) >> abs)
        |> Seq.map snd
        |> Seq.tryHead
    Seq.findClosest p n s =! closestMatch
