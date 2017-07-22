module HanabiGuru.Engine.Tests.ResultTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

[<Property>]
let ``The items in a list of ok results are the same as the list in the collected result`` (xs : int list) =
    xs |> List.map Ok |> Result.collect =! Ok xs

[<Property>]
let ``The errors in a list of results are the same as the list of errors in the collected result``
    (xs : int list)
    (errors : NonEmptyArray<string>) =

    errors.Get
    |> List.ofArray
    |> List.map Error
    |> List.append (List.map Ok xs)
    |> Result.collect =! (errors.Get |> List.ofArray |> Error)

[<Property>]
let ``Sorting and then collecting a list of results is the same as collecting and then sorting``
    (xs : int list)
    (errors : string list) =

    let resultsList = xs |> List.map Ok |> List.append (List.map Error errors)
    let sortThenCollect = List.sort >> Result.collect
    let collectThenSort = Result.collect >> Result.map List.sort >> Result.mapError List.sort
    sortThenCollect resultsList =! collectThenSort resultsList
