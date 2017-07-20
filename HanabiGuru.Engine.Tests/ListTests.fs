﻿module HanabiGuru.Engine.Tests.ListTests

open FsCheck.Xunit
open Swensen.Unquote

let assertSortingBeforeOperationSameAsAfter f x list =
    let fThenSort x = f x >> List.sort
    let sortThenF x = List.sort >> f x
    fThenSort x list =! sortThenF x list

[<Property>]
let ``Extracting an item from an empty list returns nothing and an empty list`` (x : string) =
    List.extract x List.empty =! (None, List.empty)

[<Property>]
let ``Extracting the first item from a list returns the item and the tail`` (head : int) (tail : int list) =
    (head :: tail) |> List.extract head =! (Some head, tail)

[<Property>]
let ``Removal is commutative`` (list : int list) (x : int) (y : int) =
    list |> List.remove x |> List.remove y =! (list |> List.remove y |> List.remove x)

[<Property>]
let ``Removing and then sorting leaves the same list as sorting and then removing`` (list : char list) (x : char) =
    assertSortingBeforeOperationSameAsAfter List.remove x list

[<Property>]
let ``Removing from an empty list returns an empty list`` (list : char list) =
    List.removeEach list List.empty =! List.empty

[<Property>]
let ``Removing an empty list from a list returns the list`` (list : string list) =
    List.removeEach List.empty list =! list

[<Property>]
let ``Removing a list from itself returns an empty list`` (list : int list) =
    List.removeEach list list =! List.empty

[<Property>]
let ``List removal is commutative`` (list : int list) (xs : int list) (ys : int list) =
    list |> List.removeEach xs |> List.removeEach ys =! (list |> List.removeEach ys |> List.removeEach xs)

[<Property>]
let ``Removing and then sorting is the same as sorting and then removing`` (list : int list) (xs : int list) =
    assertSortingBeforeOperationSameAsAfter List.removeEach xs list

[<Property>]
let ``Remove followed by except is the same as except followed by remove`` (list : int list) (xs : int list) =
    let removeThenExcept xs = List.removeEach xs >> List.except xs
    let exceptThenRemove xs = List.except xs >> List.removeEach xs
    removeThenExcept xs list =! exceptThenRemove xs list

[<Property>]
let ``Remove followed by except is the same as except`` (list : int list) (xs : int list) =
    let removeThenExcept xs = List.removeEach xs >> List.except xs
    removeThenExcept xs list =! List.except xs list

[<Property>]
let ``Removing the first part of a list returns the rest of the list`` (firstPart : int list) (rest : int list) =
    (firstPart @ rest) |> List.removeEach firstPart =! rest

[<Property>]
let ``List removal returns a list reduced by the number of overlapping items`` (list : int list) (xs : int list) =
    let expectedLength =
        List.map (fun value -> (value, 1)) list
        |> List.append (List.map (fun value -> (value, -1)) xs)
        |> List.groupBy fst
        |> List.map (fun (value, occurrences) -> (value, List.sumBy snd occurrences |> max 0))
        |> List.sumBy snd
    List.removeEach xs list |> List.length =! expectedLength
