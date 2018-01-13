module HanabiGuru.Engine.Tests.SeqTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

[<Property>]
let ``Finding the closest matching element returns none when no elements match`` (NonNegativeInt n) (list : int list) =
    let s = seq list
    Seq.findClosest (fun _ -> false) n s =! None

[<Property>]
let ``Finding the closest matching element returns the element at the target index when all elements match``
    (NonNegativeInt n) =

    let s = Seq.initInfinite id
    Seq.findClosest (fun _ -> true) n s =! Some n

[<Property>]
let ``Finding the closest matching element returns a match when one can be found``
    (p : int -> bool)
    (NonNegativeInt n)
    (list : int list) =

    let s = seq list
    Seq.findClosest p n s <> None =! Seq.exists p s

[<Property>]
let ``Finding the closest matching element returns the closest matching element before the target`` (NonNegativeInt n) =
    let s = Seq.initInfinite id
    Seq.findClosest (fun x -> x % 10 == 0) n s <=! n

(*
let seqEqual x y = Seq.compareWith Operators.compare x y = 0

[<Property>]
let ``Splitting and concatenating a sequence results in the original sequence`` (NonNegativeInt n) (list : int list) =
    let s = seq list
    test <@ Seq.splitAt n s |> fun (x, y) -> Seq.append x y |> seqEqual s @>

[<Property>]
let ``Splitting a sequence returns a sequence of the requested length and the remainder``
    (NonNegativeInt n)
    (list : string list) =

    let s = seq list
    let maxN = Seq.length s
    let n = if n <= maxN then n else n % (maxN + 1)
    let (x, y) = Seq.splitAt n s
    Seq.length x =! n
    Seq.length y =! Seq.length s - n

[<Property>]
let ``Splitting a sequence at a point beyond its length returns the original sequence and an empty sequence``
    (NonNegativeInt n)
    (list : string list) =

    let s = seq list
    let n = n + Seq.length s
    test <@ Seq.splitAt n s |> fun (x, y) -> seqEqual x s && Seq.isEmpty y @>

[<Property>]
let ``Splitting a sequence evaluates each element up to the split once only`` (NonNegativeInt n) =
    let mutable evaluations = 0
    Seq.initInfinite (fun i -> evaluations <- evaluations + 1; i)
    |> Seq.splitAt n
    |> ignore
    evaluations =! n
    *)
